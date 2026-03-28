using Dates
using InteractiveUtils
using Pkg
using Printf
using Sockets
using TOML

function parse_args(args::Vector{String})
    options = Dict{String, String}()
    positionals = String[]
    index = 1
    while index <= length(args)
        arg = args[index]
        if startswith(arg, "--")
            key = arg[3:end]
            if index < length(args) && !startswith(args[index + 1], "--")
                options[key] = args[index + 1]
                index += 2
            else
                options[key] = "true"
                index += 1
            end
        else
            push!(positionals, arg)
            index += 1
        end
    end
    options, positionals
end

function csv_cell(value)
    text = value === nothing ? "" : string(value)
    escaped = replace(text, "\"" => "\"\"")
    if occursin(',', escaped) || occursin('\n', escaped) || occursin('"', escaped)
        return "\"" * escaped * "\""
    end
    escaped
end

function write_csv_table(path::AbstractString, headers::Vector{String}, rows::Vector{<:AbstractDict})
    open(path, "w") do io
        println(io, join(csv_cell.(headers), ","))
        for row in rows
            println(io, join([csv_cell(get(row, header, "")) for header in headers], ","))
        end
    end
end

function write_text_file(path::AbstractString, contents::AbstractString)
    open(path, "w") do io
        write(io, contents)
    end
end

function format_now()
    Dates.format(Dates.now(), dateformat"yyyy-mm-ddTHH:MM:SS")
end

function path_probe_status(path::AbstractString)
    if isdir(path)
        return "directory"
    elseif isfile(path)
        return "file"
    end
    "missing"
end

function probe_writable_directory(path::AbstractString)
    target_directory = isdir(path) ? path : dirname(path)
    if !isdir(target_directory)
        try
            mkpath(target_directory)
        catch
            return false, target_directory
        end
    end

    probe_file = joinpath(target_directory, ".cluster-julia-write-probe-" * string(getpid()) * "-" * string(rand(UInt)))
    try
        open(probe_file, "w") do io
            write(io, "cluster validation probe\n")
        end
        rm(probe_file; force = true)
        return true, target_directory
    catch
        return false, target_directory
    end
end

function julia_executable_path()
    candidate = joinpath(Sys.BINDIR, Base.julia_exename())
    try
        return realpath(candidate)
    catch
        return candidate
    end
end

function capture_pkg_status_text()
    io = IOBuffer()
    try
        Pkg.status(; io = io, mode = Pkg.PKGMODE_PROJECT)
    catch err
        println(io, "Pkg.status failed: ", sprint(showerror, err, catch_backtrace()))
    end
    String(take!(io))
end

function project_dependency_names(project_toml::AbstractString)
    parsed = TOML.parsefile(project_toml)
    deps = get(parsed, "deps", Dict{String, Any}())
    sort!(collect(keys(deps)))
end

function dependency_versions_by_name()
    versions = Dict{String, String}()
    for entry in values(Pkg.dependencies())
        if entry.version !== nothing
            versions[entry.name] = string(entry.version)
        end
    end
    versions
end

function environment_snapshot(project_root::AbstractString)
    manifest_path = joinpath(project_root, "Manifest.toml")
    manifest_version = if isfile(manifest_path)
        get(TOML.parsefile(manifest_path), "julia_version", "unknown")
    else
        "missing"
    end

    scheduler_env = Dict(
        "SLURM_JOB_ID" => get(ENV, "SLURM_JOB_ID", ""),
        "SLURM_CLUSTER_NAME" => get(ENV, "SLURM_CLUSTER_NAME", ""),
        "PBS_JOBID" => get(ENV, "PBS_JOBID", ""),
        "LSB_JOBID" => get(ENV, "LSB_JOBID", "")
    )

    Dict(
        "timestamp" => format_now(),
        "hostname" => gethostname(),
        "project_root" => project_root,
        "pwd" => pwd(),
        "julia_executable" => julia_executable_path(),
        "julia_cmd" => repr(Base.julia_cmd()),
        "julia_version" => string(VERSION),
        "manifest_julia_version" => string(manifest_version),
        "active_project_before_activation" => string(Base.active_project()),
        "load_path" => join(LOAD_PATH, " | "),
        "depot_path" => join(DEPOT_PATH, " | "),
        "julia_project_env" => get(ENV, "JULIA_PROJECT", ""),
        "julia_load_path_env" => get(ENV, "JULIA_LOAD_PATH", ""),
        "julia_depot_path_env" => get(ENV, "JULIA_DEPOT_PATH", ""),
        "path_env" => get(ENV, "PATH", ""),
        "threads" => string(Threads.nthreads()),
        "scheduler_env" => join(["$(key)=$(value)" for (key, value) in scheduler_env], " | ")
    )
end

options, _ = parse_args(ARGS)
project_root = abspath(normpath(get(options, "project-root", joinpath(@__DIR__, ".."))))
timestamp = Dates.format(Dates.now(), dateformat"yyyymmdd-HHMMSS")
default_output_dir = joinpath(project_root, ".runs", "cluster-julia-diagnostic-" * timestamp)
output_dir = abspath(normpath(get(options, "output-dir", default_output_dir)))
mkpath(output_dir)

log_file = joinpath(output_dir, "cluster_julia_diagnostic.log")
stage_rows = Dict{String, Any}[]
package_rows = Dict{String, Any}[]
environment_rows = environment_snapshot(project_root)

open(log_file, "w") do log_io
    function log_line(message::AbstractString)
        line = "$(format_now()) $message"
        println(line)
        println(log_io, line)
        flush(log_io)
    end

    function record_stage(name::String, status::String, seconds::Float64, message::String; extra = Dict{String, Any}())
        row = Dict{String, Any}(
            "stage" => name,
            "status" => status,
            "seconds" => @sprintf("%.6f", seconds),
            "message" => message
        )
        merge!(row, extra)
        push!(stage_rows, row)
        log_line("[$(uppercase(status))] $name ($(round(seconds; digits = 3)) s) $message")
        row
    end

    function run_stage(action::Function, name::String)
        started = time()
        try
            result = action()
            payload = result isa AbstractDict ? Dict{String, Any}(result) : Dict{String, Any}("message" => string(result))
            status = get(payload, "status", "ok")
            message = get(payload, "message", "")
            delete!(payload, "status")
            delete!(payload, "message")
            record_stage(name, status, time() - started, message; extra = payload)
        catch err
            record_stage(
                name,
                "error",
                time() - started,
                sprint(showerror, err, catch_backtrace());
                extra = Dict(
                    "error_type" => string(typeof(err))
                )
            )
        end
    end

    log_line("Project root: $project_root")
    log_line("Output dir:   $output_dir")
    log_line("Julia exe:    $(environment_rows["julia_executable"])")
    log_line("Julia cmd:    $(environment_rows["julia_cmd"])")

    run_stage("folder_integrity") do
        required_paths = [
            joinpath(project_root, "Project.toml"),
            joinpath(project_root, "Manifest.toml"),
            joinpath(project_root, "SUSYWBEWorkflow.wl"),
            joinpath(project_root, "solver", "NumericalWBE_SUSY.m"),
            joinpath(project_root, "solver", "SusyWronskianSolver.m"),
            joinpath(project_root, "utils", "functions.m"),
            joinpath(project_root, "julia", "SUSYWBEJuliaTaskRunner.jl"),
            joinpath(project_root, "julia", "SUSYWBEPersistentWorker.jl"),
            joinpath(project_root, "julia", "SUSYWBEJuliaSolver.jl"),
            joinpath(project_root, "julia", "bethe_functions.jl")
        ]
        missing = filter(path -> !ispath(path), required_paths)
        Dict(
            "status" => isempty(missing) ? "ok" : "error",
            "message" => isempty(missing) ? "Required project files are present." : "Missing required project files.",
            "missing_paths" => join(missing, " | ")
        )
    end

    run_stage("julia_version_check") do
        manifest_version = environment_rows["manifest_julia_version"]
        version_status =
            if manifest_version == "missing" || manifest_version == "unknown"
                "warning"
            else
                manifest_parts = split(manifest_version, ".")
                runtime_parts = split(string(VERSION), ".")
                if length(manifest_parts) >= 2 && length(runtime_parts) >= 2 &&
                   manifest_parts[1:2] == runtime_parts[1:2]
                    VERSION == VersionNumber(manifest_version) ? "ok" : "warning"
                else
                    "error"
                end
            end

        Dict(
            "status" => version_status,
            "message" => "Runtime Julia $(VERSION); manifest was generated with $(manifest_version).",
            "runtime_version" => string(VERSION),
            "manifest_version" => manifest_version
        )
    end

    run_stage("depot_writability") do
        writable_rows = String[]
        any_writable = false
        for depot in DEPOT_PATH
            writable, probed_dir = probe_writable_directory(depot)
            push!(writable_rows, "$(depot)=>$(writable ? "writable" : "not-writable") via $(probed_dir)")
            any_writable |= writable
        end
        Dict(
            "status" => any_writable ? "ok" : "error",
            "message" => any_writable ? "At least one Julia depot is writable." : "No writable Julia depot found.",
            "depot_probe_results" => join(writable_rows, " | ")
        )
    end

    run_stage("project_activation") do
        Pkg.activate(project_root)
        active_project = Base.active_project()
        expected_project = abspath(joinpath(project_root, "Project.toml"))
        Dict(
            "status" => active_project == expected_project ? "ok" : "error",
            "message" => "Active project is $(active_project).",
            "active_project" => string(active_project),
            "expected_project" => expected_project
        )
    end

    run_stage("instantiate") do
        Pkg.instantiate()
        Dict(
            "status" => "ok",
            "message" => "Pkg.instantiate() completed successfully."
        )
    end

    run_stage("precompile") do
        Pkg.precompile()
        Dict(
            "status" => "ok",
            "message" => "Pkg.precompile() completed successfully."
        )
    end

    pkg_status_path = joinpath(output_dir, "cluster_julia_pkg_status.txt")
    run_stage("pkg_status_snapshot") do
        pkg_status_text = capture_pkg_status_text()
        write_text_file(pkg_status_path, pkg_status_text)
        Dict(
            "status" => "ok",
            "message" => "Wrote Julia package status snapshot.",
            "pkg_status_file" => pkg_status_path
        )
    end

    run_stage("required_package_imports") do
        package_names = project_dependency_names(joinpath(project_root, "Project.toml"))
        versions = dependency_versions_by_name()
        import_failures = String[]

        empty!(package_rows)
        for package_name in package_names
            started = time()
            row = Dict{String, Any}(
                "package" => package_name,
                "version" => get(versions, package_name, ""),
                "status" => "ok",
                "seconds" => "",
                "error" => ""
            )
            try
                Base.require(Main, Symbol(package_name))
            catch err
                row["status"] = "error"
                row["error"] = sprint(showerror, err, catch_backtrace())
                push!(import_failures, package_name)
            end
            row["seconds"] = @sprintf("%.6f", time() - started)
            push!(package_rows, row)
        end

        Dict(
            "status" => isempty(import_failures) ? "ok" : "error",
            "message" => isempty(import_failures) ? "All direct Julia dependencies imported successfully." : "Package import failures: $(join(import_failures, ", "))",
            "package_count" => length(package_names)
        )
    end

    run_stage("worker_warmup") do
        Base.include(Main, abspath(joinpath(project_root, "julia", "SUSYWBEJuliaSolver.jl")))
        Base.invokelatest(getproperty(Main, :SUSYWBEJuliaSolver).worker_warmup)
        Dict(
            "status" => "ok",
            "message" => "Julia worker warmup completed successfully."
        )
    end

    run_stage("project_write_read_probe") do
        probe_dir = joinpath(project_root, ".runs", "cluster-write-probe")
        mkpath(probe_dir)
        probe_file = joinpath(probe_dir, "cluster_julia_probe.txt")
        probe_text = "cluster-julia-diagnostic $(format_now())"
        write_text_file(probe_file, probe_text)
        roundtrip_text = read(probe_file, String)
        Dict(
            "status" => probe_text == roundtrip_text ? "ok" : "error",
            "message" => probe_text == roundtrip_text ? "Project write/read probe succeeded." : "Project write/read probe returned unexpected contents.",
            "probe_file" => probe_file
        )
    end
end

environment_path = joinpath(output_dir, "cluster_julia_environment.txt")
environment_lines = [
    "timestamp=$(environment_rows["timestamp"])",
    "hostname=$(environment_rows["hostname"])",
    "project_root=$(environment_rows["project_root"])",
    "pwd=$(environment_rows["pwd"])",
    "julia_executable=$(environment_rows["julia_executable"])",
    "julia_cmd=$(environment_rows["julia_cmd"])",
    "julia_version=$(environment_rows["julia_version"])",
    "manifest_julia_version=$(environment_rows["manifest_julia_version"])",
    "active_project_before_activation=$(environment_rows["active_project_before_activation"])",
    "load_path=$(environment_rows["load_path"])",
    "depot_path=$(environment_rows["depot_path"])",
    "julia_project_env=$(environment_rows["julia_project_env"])",
    "julia_load_path_env=$(environment_rows["julia_load_path_env"])",
    "julia_depot_path_env=$(environment_rows["julia_depot_path_env"])",
    "threads=$(environment_rows["threads"])",
    "scheduler_env=$(environment_rows["scheduler_env"])"
]
write_text_file(environment_path, join(environment_lines, "\n") * "\n")

write_csv_table(
    joinpath(output_dir, "cluster_julia_stages.csv"),
    ["stage", "status", "seconds", "message", "error_type", "missing_paths", "runtime_version", "manifest_version", "depot_probe_results", "active_project", "expected_project", "pkg_status_file", "package_count", "probe_file"],
    stage_rows
)
write_csv_table(
    joinpath(output_dir, "cluster_julia_packages.csv"),
    ["package", "version", "status", "seconds", "error"],
    package_rows
)

overall_success = all(get(row, "status", "ok") != "error" for row in stage_rows)
summary_path = joinpath(output_dir, "cluster_julia_summary.txt")
summary_lines = [
    "overall_status=$(overall_success ? "ok" : "error")",
    "log_file=$(log_file)",
    "environment_file=$(environment_path)",
    "stage_file=$(joinpath(output_dir, "cluster_julia_stages.csv"))",
    "package_file=$(joinpath(output_dir, "cluster_julia_packages.csv"))",
    "pkg_status_file=$(joinpath(output_dir, "cluster_julia_pkg_status.txt"))"
]
write_text_file(summary_path, join(summary_lines, "\n") * "\n")

println("Cluster Julia diagnostic output: $(output_dir)")
println("Overall Julia diagnostic status: $(overall_success ? "ok" : "error")")
exit(overall_success ? 0 : 1)
