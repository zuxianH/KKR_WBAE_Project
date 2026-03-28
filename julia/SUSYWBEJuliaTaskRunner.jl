if length(ARGS) < 4
    error("Usage: julia SUSYWBEJuliaTaskRunner.jl <saved_data_dir> <initial_data_file> <output_file> <timing_file>")
end

script_start = time()

include(joinpath(@__DIR__, "SUSYWBEJuliaSolver.jl"))
using .SUSYWBEJuliaSolver

package_load_seconds = time() - script_start

SUSYWBEJuliaSolver.run_job(
    abspath(ARGS[1]),
    abspath(ARGS[2]),
    abspath(ARGS[3]),
    abspath(ARGS[4]);
    startup_seconds = package_load_seconds,
    package_load_seconds = package_load_seconds
)
