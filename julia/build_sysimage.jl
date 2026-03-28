using PackageCompiler

script_dir = @__DIR__
project_dir = normpath(joinpath(script_dir, ".."))
sysimage_dir = joinpath(project_dir, ".sysimages")
mkpath(sysimage_dir)

sysimage_extension = Sys.isapple() ? ".dylib" : Sys.iswindows() ? ".dll" : ".so"
sysimage_path = joinpath(sysimage_dir, "susywbe_worker" * sysimage_extension)
precompile_file = joinpath(script_dir, "precompile_susywbe_worker.jl")

packages = [
    :CSV,
    :DataFrames,
    :BifurcationKit,
    :GLM,
    :Suppressor,
    :ForwardDiff,
    :IncompleteLU,
    :HomotopyContinuation
]

create_sysimage(
    packages;
    project = project_dir,
    sysimage_path = sysimage_path,
    precompile_execution_file = precompile_file,
    cpu_target = "native"
)

println(sysimage_path)
