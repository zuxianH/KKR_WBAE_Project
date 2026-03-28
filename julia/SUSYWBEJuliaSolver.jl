module SUSYWBEJuliaSolver

using CSV
using DataFrames
using HomotopyContinuation
using IncompleteLU
using LinearAlgebra
using SparseArrays

include(joinpath(@__DIR__, "bethe_functions.jl"))

LinearAlgebra.BLAS.set_num_threads(1)

const WORKER_PROTOCOL_VERSION = "1"

maybe_remove(path::AbstractString) = (ispath(path) && rm(path; force = true); nothing)
ensure_parent_dir(path::AbstractString) = mkpath(dirname(path))

function write_timings(timing_file::AbstractString, timings::Dict{String, Float64})
    ensure_parent_dir(timing_file)
    ordered_rows = sort(collect(timings); by = first)
    CSV.write(
        timing_file,
        DataFrame(stage = first.(ordered_rows), seconds = last.(ordered_rows))
    )
end

function write_output(output_file::AbstractString, values::AbstractVector)
    ensure_parent_dir(output_file)
    CSV.write(
        output_file,
        DataFrame(var = string.(vars[1:end-1]), b_final_value = values)
    )
end

function worker_warmup()
    temp_dir = mktempdir()
    try
        temp_csv = joinpath(temp_dir, "warmup.csv")
        CSV.write(temp_csv, DataFrame(var = ["x"], b_final_value = [1.0]))
        CSV.read(temp_csv, DataFrame)

        x = Variable(:x)
        h = Variable(:h)
        F = System([x^2 - h], variables = [x], parameters = [h])
        solve(
            F,
            [[1.0]];
            start_parameters = [1.0],
            target_parameters = [0.0],
            show_progress = false,
            tracker_options = TrackerOptions(
                automatic_differentiation = 3,
                terminate_cond = 1e50,
                extended_precision = true,
                parameters = :conservative
            )
        )
        nothing
    finally
        rm(temp_dir; force = true, recursive = true)
    end
end

function prepare_problem(problem_path::AbstractString, λ0::Float64, scale_param::Float64; start_solution = nothing)
    global my_scale_param = scale_param
    load_scale_data_and_initialize(scale_param, String(problem_path))
    load_jacobian(scale_param)
    global u0 = isnothing(start_solution) ? Float64.(vars_init) : Float64.(start_solution) .* scale_param
    global p0 = [λ0]
    global prob = BifurcationProblem(
        G,
        u0 / scale_param,
        p0,
        1;
        J = (x, p) -> scaled_Jac(x, p),
        record_from_solution = (x, p; k...) -> Tuple(x)
    )
    nothing
end

function build_homotopy_system()
    myvars = [Symbol(string(var)) for var in vcat(load_initial_data.var, "h")]
    @eval @var $(myvars...)
    my_expression = [eval(Meta.parse(df[k])) for k in 1:length(df)]
    System(
        my_expression,
        variables = [Variable(Symbol(string(var))) for var in load_initial_data.var],
        parameters = [Variable(:h)]
    )
end

function max_syt_from_loaded_data()
    string_syt = String(load_initial_data.syt[1])
    pieces = filter(!isempty, split(replace(string_syt, r"[^\d,]" => ""), ","))
    isempty(pieces) ? 0 : maximum(parse.(Int, pieces))
end

function run_palc_fallback(problem_path::AbstractString, λ0::Float64, scale_param::Float64, timings::Dict{String, Float64}; start_solution = nothing)
    prepare_problem(problem_path, λ0, scale_param; start_solution = start_solution)

    local_solver = GMRESIterativeSolvers(N = length(u0) + 1)
    tracked_indices = collect(1:length(u0))
    tolerance_window = ones(Float64, length(tracked_indices))

    function callback_local(state; dp, kwargs...)
        previous_state = get(kwargs, :z0, nothing)
        from_newton = get(kwargs, :fromNewton, false)
        if !from_newton && previous_state !== nothing
            dp_jump = abs(previous_state.p - state.p)
            dx_vals = abs.(previous_state.u[tracked_indices] .- state.x[tracked_indices])
            if all(dx_vals .> tolerance_window) || dp_jump > dp
                return false
            end
        end
        true
    end

    function run_main()
        main_opts = ParameterOpts(
            -1e-3,
            1e20,
            1e-4,
            λ0,
            0.0,
            100,
            1e-7,
            0.007,
            false
        )
        global my_verbosity = 0
        global ls = local_solver
        global x_error_list = Any[]
        continuation(
            prob,
            PALC(tangent = Bordered(), θ = 0.5),
            make_opts(main_opts),
            normC = norminf,
            callback_newton = (state; kwargs...) -> callback_local(state; kwargs..., dp = 70.0),
            verbosity = my_verbosity,
            linear_algo = BorderingBLS(solver = local_solver, tol = 1e-7, check_precision = true),
            finalise_solution = my_finalise_solution
        )
    end

    palc_initial_start = time()
    run_dummy = run_main()
    timings["palc_initial_run_seconds"] = time() - palc_initial_start

    r_squared_table = calculate_r_squared_table(run_dummy)
    low_r_squared_components = r_squared_table[r_squared_table.R_squared .< 0.6, :]
    tracked_indices = isempty(low_r_squared_components.Component) ? collect(1:length(u0)) : collect(low_r_squared_components.Component)

    abs_diff = [abs(run_dummy.sol[end].x[i] - run_dummy.sol[1].x[i]) for i in tracked_indices]
    tolerance_window = abs_diff / 200

    ratio = 0.0
    tolerance_divisor = 500.0
    tolerance_step = 1000.0
    max_iterations = 10
    iteration_count = 0
    final_result = run_dummy

    palc_refine_start = time()
    while ratio <= 0.99 && iteration_count < max_iterations
        tolerance_window = abs_diff / tolerance_divisor
        final_result = run_main()

        itnewton_values = final_result.branch.itnewton
        total_count =
            count(==(0), itnewton_values) +
            count(==(1), itnewton_values) +
            count(==(2), itnewton_values)
        ratio = total_count / max(1, length(itnewton_values))

        println(
            "PALC iteration $(iteration_count + 1) ratio=$(ratio) final_param=$(final_result.param[end])"
        )

        tolerance_divisor += tolerance_step
        iteration_count += 1
    end
    timings["palc_refinement_seconds"] = time() - palc_refine_start

    if iteration_count == max_iterations || final_result.param[end] != 0
        error("PALC failed to reach lambda = 0.")
    end

    extract_last_solution(final_result) * scale_param
end

function solve_job(initial_data_file::AbstractString, output_file::AbstractString, timings::Dict{String, Float64})
    load_problem_start = time()
    λ0 = Float64(CSV.read(String(initial_data_file), DataFrame).lambda0[1])
    prepare_problem(String(initial_data_file), λ0, 1.0)
    scale_param = maximum(abs.(u0))
    prepare_problem(String(initial_data_file), λ0, scale_param)
    timings["load_problem_seconds"] = time() - load_problem_start

    build_system_start = time()
    F = build_homotopy_system()
    timings["build_system_seconds"] = time() - build_system_start

    max_syt = max_syt_from_loaded_data()
    final_vector =
        if max_syt <= 21
            homotopy_start = time()
            homotopy_result = solve(
                F,
                [u0 / scale_param];
                start_parameters = p0,
                target_parameters = [0.0],
                show_progress = false,
                tracker_options = TrackerOptions(
                    automatic_differentiation = 3,
                    terminate_cond = 1e50,
                    extended_precision = true,
                    parameters = :conservative
                )
            )
            timings["homotopy_solve_seconds"] = time() - homotopy_start

            real_result = real_solutions(homotopy_result)
            if isempty(real_result)
                println("No real solutions from HomotopyContinuation. Falling back to PALC.")
                run_palc_fallback(String(initial_data_file), λ0, scale_param, timings)
            else
                reduce(vcat, real_result) * scale_param
            end
        else
            println("Problem exceeds HomotopyContinuation threshold. Falling back to PALC.")
            run_palc_fallback(String(initial_data_file), λ0, scale_param, timings)
        end

    write_output_start = time()
    write_output(output_file, final_vector)
    timings["write_output_seconds"] = time() - write_output_start

    final_vector
end

function run_job_core(
    saved_data_dir::AbstractString,
    initial_data_file::AbstractString,
    output_file::AbstractString,
    timing_file::AbstractString;
    startup_seconds::Float64 = 0.0,
    package_load_seconds::Float64 = 0.0
)
    mkpath(saved_data_dir)
    ensure_parent_dir(output_file)
    ensure_parent_dir(timing_file)
    maybe_remove(output_file)
    maybe_remove(timing_file)

    timings = Dict{String, Float64}()
    timings["package_load_seconds"] = package_load_seconds
    timings["startup_seconds"] = startup_seconds

    total_start = time()
    try
        solve_job(initial_data_file, output_file, timings)
    finally
        timings["script_total_seconds"] = time() - total_start
        write_timings(timing_file, timings)
    end
end

function run_job(
    saved_data_dir::AbstractString,
    initial_data_file::AbstractString,
    output_file::AbstractString,
    timing_file::AbstractString;
    stdout_file::Union{Nothing, AbstractString} = nothing,
    stderr_file::Union{Nothing, AbstractString} = nothing,
    startup_seconds::Float64 = 0.0,
    package_load_seconds::Float64 = 0.0
)
    if stdout_file === nothing && stderr_file === nothing
        return run_job_core(
            saved_data_dir,
            initial_data_file,
            output_file,
            timing_file;
            startup_seconds = startup_seconds,
            package_load_seconds = package_load_seconds
        )
    end

    stdout_path = stdout_file === nothing ? joinpath(saved_data_dir, "julia.stdout.log") : String(stdout_file)
    stderr_path = stderr_file === nothing ? joinpath(saved_data_dir, "julia.stderr.log") : String(stderr_file)
    ensure_parent_dir(stdout_path)
    ensure_parent_dir(stderr_path)
    maybe_remove(stdout_path)
    maybe_remove(stderr_path)

    open(stdout_path, "w") do outio
        open(stderr_path, "w") do errio
            try
                redirect_stdout(outio) do
                    redirect_stderr(errio) do
                        run_job_core(
                            saved_data_dir,
                            initial_data_file,
                            output_file,
                            timing_file;
                            startup_seconds = startup_seconds,
                            package_load_seconds = package_load_seconds
                        )
                    end
                end
            catch err
                showerror(errio, err, catch_backtrace())
                println(errio)
                rethrow(err)
            end
        end
    end
end

end
