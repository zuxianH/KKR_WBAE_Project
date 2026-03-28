script_start = time()

include(joinpath(@__DIR__, "SUSYWBEJuliaSolver.jl"))
using .SUSYWBEJuliaSolver

startup_seconds = time() - script_start
warmup_start = time()
SUSYWBEJuliaSolver.worker_warmup()
startup_seconds += time() - warmup_start

println("READY\t$(SUSYWBEJuliaSolver.WORKER_PROTOCOL_VERSION)\t$(startup_seconds)")
flush(stdout)

function parse_run_fields(parts)
    if length(parts) != 8
        error("RUN expects 7 arguments.")
    end
    (
        parts[2],
        parts[3],
        parts[4],
        parts[5],
        parts[6],
        parts[7],
        parts[8]
    )
end

for raw_line in eachline(stdin)
    line = chomp(raw_line)
    isempty(line) && continue
    parts = split(line, '\t')
    command = first(parts)

    if command == "PING"
        println("PONG")
        flush(stdout)
        continue
    end

    if command == "QUIT"
        println("BYE")
        flush(stdout)
        break
    end

    if command == "RUN"
        job_start = time()
        try
            saved_data_dir, initial_data_file, output_file, timing_file, stdout_file, stderr_file, startup_tag = parse_run_fields(parts)
            startup_value = parse(Float64, startup_tag)
            SUSYWBEJuliaSolver.run_job(
                saved_data_dir,
                initial_data_file,
                output_file,
                timing_file;
                stdout_file = stdout_file,
                stderr_file = stderr_file,
                startup_seconds = startup_value,
                package_load_seconds = 0.0
            )
            println("RESULT\tOK\t$(time() - job_start)")
        catch err
            println(stderr, "Persistent worker error: ", sprint(showerror, err))
            println("RESULT\tERROR\t$(time() - job_start)")
        end
        flush(stdout)
        flush(stderr)
        continue
    end

    println("RESULT\tERROR\t0.0")
    flush(stdout)
end
