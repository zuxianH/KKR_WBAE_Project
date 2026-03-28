using CSV, DataFrames, BifurcationKit, GLM, LinearAlgebra

function my_finalise_solution(z, τ, step, contResult; kwargs...)
    # Access the continuation state
    state = kwargs[:state]
    
    # Retrieve the predicted point
    z_pred = getpredictor(state)
    z_sol = getsolution(state)
    
    # Extract the predicted solution and parameter
    x_pred = z_pred.u
    p_pred = z_pred.p

    x_sol = z_sol.u
    p_sol = z_sol.p

    x_error = maximum(abs.(x_pred - x_sol))
    push!(x_error_list, x_error)

    # Continue the continuation process
    return true
end

function load_jacobian(scale_param::Float64)
    b_vars = load_initial_data.var
    dJac = load_initial_data.jacobian
    parsed_rows = [ replace(r, '{' => ' ', '}' => ' ')   for r in dJac]
    flat_dJac = reduce(vcat, [split(row, ",") .|> strip for row in parsed_rows])
    vec_str = String.(flat_dJac)

    scaled_expressions = copy(vec_str)
    for (i, expr) in enumerate(scaled_expressions)
        modified_expr = expr
        for v in b_vars
            modified_expr = replace(modified_expr, Regex("\\b$v\\b") => "($(scale_param)*$v)")
        end
        scaled_expressions[i] = modified_expr
    end

    global expr_dJac = [
        eval(Expr(:->, Expr(:tuple, vars...), Meta.parse(expr)))
        for expr in scaled_expressions
    ]
end

function Jac(u, p)
    args = vcat(u, p)
    element=  [f(args...) for f in expr_dJac]  
    return Float64.(transpose(reshape(element,length(vars)-1,length(vars)-1)))
end

function scaled_Jac(u,p)
    return my_scale_param.*Jac(u,p)
end 


function get_branch(br, sol_idx)
    branch_df = DataFrame([pt for pt in br.branch])
    for col in names(branch_df)
        if eltype(branch_df[!, col]) == Nothing
            branch_df[!, col] = Any[branch_df[!, col]...]  # convert to Any
        end
        replace!(branch_df[!, col], nothing => missing)
    end
    return (br.param,branch_df[!, sol_idx])
end

function calculate_r_squared_table(br)
    # Initialize an empty DataFrame to store R-squared values
    r_squared_table = DataFrame(Component=Int[], R_squared=Float64[])

    # Loop through each component and calculate R-squared
    for i in 1:length(u0)
        # Extract the branch data for the current component
        params, branch_values = get_branch(br, i)
        
        # Perform linear regression
        data = DataFrame(x=params, y=branch_values)
        model = lm(@formula(y ~ x), data)
        
        # Extract R-squared value
        r_squared = r2(model)
        
        # Append to the table
        push!(r_squared_table, (i, r_squared))
    end

    return r_squared_table
end

# Extract the last solution
function extract_last_solution(br)
    last_point = br.branch[end]
    get_last_solution = collect(values(last_point)[1:length(u0)])
    return get_last_solution
end


######################################################################
function load_scale_data_and_initialize(scale_param::Float64, path::String)
    global vars, vars_init, expr_funcs, df, load_initial_data
    load_initial_data = CSV.read(path, DataFrame; stringtype=String)
    global original_exprs = load_initial_data.expression
    global b_vars = load_initial_data.var
    vars = Symbol.(vcat(b_vars, "h"))
    initialvar_processed = replace.(string.(load_initial_data.Initialvar), r"\*10\^" => "e")
    vars_init = BigFloat.(initialvar_processed)
    
    scaled_expressions = copy(original_exprs)
    for (i, expr) in enumerate(scaled_expressions)
        modified_expr = expr
        for v in b_vars
            modified_expr = replace(modified_expr, Regex("\\b$v\\b") => "($(scale_param)*$v)")
        end
        scaled_expressions[i] = modified_expr
    end
    df = scaled_expressions
    
    expr_funcs = let scale_param = scale_param
        [eval(Expr(:->, Expr(:tuple, vars...), Meta.parse(expr)))
         for expr in df]
    end
end

function G(u, p)
    args = vcat(u, p)
    return [f(args...) for f in expr_funcs]  
end


mutable struct ParameterOpts
    ds::Float64
    dsmax::Float64
    dsmin::Float64
    lambda0::Float64
    mydpmin::Float64
    max_newton_iterations::Int64
    newton_tol::Float64
    a::Float64
    verbose::Bool
end



function make_opts(opts::ParameterOpts)
    return ContinuationPar(
        ds = opts.ds,
        dsmin = opts.dsmin, 
        dsmax = opts.dsmax,
        p_min = opts.mydpmin,
        p_max = opts.lambda0,
        max_steps = 10000,
        newton_options = NewtonPar(
            tol = opts.newton_tol,
            max_iterations = opts.max_newton_iterations,
            verbose = opts.verbose,
            #linsolver= ls,
            #linesearch = true,
            #α = 0.7
        ),
        detect_bifurcation = 0,
        detect_event = 0,
        a = opts.a,
        detect_loop = false
    )
end


function run_continuation_with_tol(
    prob::BifurcationProblem, 
    method, 
    opts::ParameterOpts=myOpts; 
    dp_tol)
    
    br = continuation(
            prob,
            method,
            make_opts(opts),
            normC = norminf,
            callback_newton = (state; kwargs...) -> cb(state; kwargs..., dp=dp_tol),
            #callback_newton = BifurcationKit.cbMaxNormAndΔp(1e1, 5e-2),
            verbosity = my_verbosity,
            linear_algo=BorderingBLS(solver = ls, tol = 1e-7,check_precision = true),
            finalise_solution = my_finalise_solution,
    )
    return br 
end

function run_lazy(prob, dp_tol::Float64,myOpts)
    run = run_continuation_with_tol(
        prob,
        PALC(tangent = Bordered(), θ=0.5), 
        #MoorePenrose(method = BifurcationKit.iterative),
        myOpts,
        dp_tol=dp_tol)
    return run
end

function cb(state; dp, kwargs...)
    _x = get(kwargs, :z0, nothing)  # Previous solution
    fromNewton = get(kwargs, :fromNewton, false)
    
    if !fromNewton && _x !== nothing
        dp_jump = abs(_x.p - state.p)  # Change in parameter
        dx_vals = abs.(_x.u .- state.x)  # Change in solution
        
        # Define tolerances dynamically
        tol_vals = [.01 * max(1, abs(x)) for x in state.x]
        
        # Check if any dx exceeds tolerance or dp_jump is too large
        if any(dx_vals .> tol_vals) || dp_jump > dp
            return false
        end
    end
    
    return true
end
