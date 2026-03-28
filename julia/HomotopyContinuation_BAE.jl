script_start = time()
include("bethe_functions.jl")
using IncompleteLU, LinearAlgebra, SparseArrays
using HomotopyContinuation

package_load_seconds = time() - script_start
LinearAlgebra.BLAS.set_num_threads(1)

function bae_saved_data_dir()
    if isdefined(@__MODULE__, :BAE_SAVED_DATA_DIR)
        return String(getfield(@__MODULE__, :BAE_SAVED_DATA_DIR))
    end
    return joinpath(@__DIR__, "saved_data")
end

function bae_initial_data_file()
    if isdefined(@__MODULE__, :BAE_INITIAL_DATA_FILE)
        return String(getfield(@__MODULE__, :BAE_INITIAL_DATA_FILE))
    end
    return joinpath(bae_saved_data_dir(), "initial_data.csv")
end

function bae_output_file()
    if isdefined(@__MODULE__, :BAE_OUTPUT_FILE)
        return String(getfield(@__MODULE__, :BAE_OUTPUT_FILE))
    end
    return joinpath(bae_saved_data_dir(), "result_$(load_initial_data.syt[1]).csv")
end

bae_timing_file = if isdefined(@__MODULE__, :BAE_TIMING_FILE)
    String(getfield(@__MODULE__, :BAE_TIMING_FILE))
else
    joinpath(bae_saved_data_dir(), "timings.csv")
end
bae_timings = Dict{String, Float64}(
    "package_load_seconds" => package_load_seconds
)

my_path = bae_saved_data_dir()
initial_data_file = bae_initial_data_file()

function load_problem_scale(problem_path::String, λ0::Float64,scale_param::Float64)
    load_scale_data_and_initialize(scale_param, problem_path);
    load_jacobian(scale_param)
    global u0 = Float64.(vars_init)      
    global p0 = [λ0];  
    global prob = BifurcationProblem(
    G, u0/scale_param, p0,1; 
    J = (x, p) -> (scaled_Jac(x, p)),
    record_from_solution = (x, p; k...) -> (Tuple(x))
    );
end 

t_load_problem = time()
# -------load and scale system and vars-----------------------
my_λ0 = Float64(CSV.read(initial_data_file, DataFrame).lambda0[1])
load_problem_scale(initial_data_file,my_λ0,1.)
my_scale_param = maximum(abs.(u0))
load_problem_scale(initial_data_file,my_λ0,my_scale_param)
bae_timings["load_problem_seconds"] = time() - t_load_problem
#norm(G(u0/my_scale_param,p0))
# -------------------------------------------------------------------------


t_build_system = time()
# -------load system and vars to HomotopyContinuation-----------------------
# define variables 
myvars = [Symbol(string(var)) for var in [load_initial_data.var; "h"]]
# evaluate var
@eval @var $(myvars...)
# define expression 
my_expression = [eval(Meta.parse(df[k])) for k in 1:length(df)];
F = System(
    my_expression,
    variables = [Variable(Symbol(string(var))) for var in load_initial_data.var], 
    parameters = [h]
);
bae_timings["build_system_seconds"] = time() - t_build_system
# -------------------------------------------------------------------------


# ---------extract name of previous result----------------------------------
# call syt in string
string_syt = load_initial_data.syt[1]
# maximal element of syt 
max_syt = maximum(parse.(Int, split(replace(string_syt, r"[^\d,]" => ""), ",")))
output_file = bae_output_file()
# -------------------------------------------------------------------------

final_output = if isfile(output_file)
    println("File $(basename(output_file)) already exists. Loading existing solutions.")
    df_out = CSV.read(output_file, DataFrame)
    df_out.b_final_value
else
    println("File $(basename(output_file)) does not exist. Proceeding with HomotopyContinuation.")

    if max_syt <=21
        my_start_solutions = [u0/my_scale_param]
        t_homotopy = time()
        result = @time solve(
            F, 
            my_start_solutions; 
            start_parameters=p0, 
            target_parameters=[0.],
            show_progress= true,
            tracker_options = TrackerOptions(
                automatic_differentiation = 3, 
                terminate_cond = 1e50,
                extended_precision = true,
                parameters = :conservative
                #parameters=TrackerParameters(β_τ = 0.01, β_ω_p = 10.0) 
            )
        )
        bae_timings["homotopy_solve_seconds"] = time() - t_homotopy

        
        if isempty(real_solutions(result))
            println("No real solutions found from HomotopyContinuation.")
            println("Start PALC")
            include("PALC_BAE.jl")
        else
            # Save the solutions to a CSV file with name result_<SYT>.csv
            df_out = DataFrame(var = string.(vars[1:end-1]), b_final_value = reduce(vcat, real_solutions(result)) * my_scale_param)
            t_write_output = time()
            CSV.write(output_file, df_out)
            bae_timings["write_output_seconds"] = time() - t_write_output
            reduce(vcat, real_solutions(result)) * my_scale_param
        end
    else 
        include("PALC_BAE.jl")
    end 

end

bae_timings["script_total_seconds"] = time() - script_start
CSV.write(
    bae_timing_file,
    sort(
        DataFrame(
            stage = collect(keys(bae_timings)),
            seconds = collect(values(bae_timings))
        ),
        :stage
    )
)
final_output
