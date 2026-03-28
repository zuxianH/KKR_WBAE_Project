(* ::Package:: *)

Off[FrontEndObject::notavail];

ClearAll[ClusterValidationScriptArguments];
ClusterValidationScriptArguments[] := Module[
  {commandLists, scriptPosition},
  commandLists = Select[{$ScriptCommandLine, $CommandLine}, ListQ[#] && # =!= {} &];

  Do[
    scriptPosition = FirstPosition[
      commandList,
      _String?(StringMatchQ[#, ___ ~~ ".wl"] &),
      Missing["NotFound"]
    ];
    If[
      scriptPosition =!= Missing["NotFound"],
      Return[Drop[commandList, scriptPosition[[1]]]]
    ],
    {commandList, commandLists}
  ];

  {}
];


ClearAll[ClusterValidationEnvironmentString];
ClusterValidationEnvironmentString[name_String] := Module[{value},
  value = Quiet@Check[Environment[name], $Failed];
  If[StringQ[value] && StringTrim[value] =!= "", value, $Failed]
];


ClearAll[ClusterValidationProjectRootDefault];
ClusterValidationProjectRootDefault[] := Module[{envValue},
  envValue = ClusterValidationEnvironmentString["BAE_CLUSTER_PROJECT_ROOT"];
  If[
    StringQ[envValue],
    envValue,
    DirectoryName[$InputFileName]
  ]
];


ClearAll[ClusterValidationString];
ClusterValidationString[value_] := Which[
  MatchQ[value, _Missing], ToString[value, InputForm],
  StringQ[value], value,
  NumberQ[value], ToString[N[value], InputForm],
  True, ToString[value, InputForm]
];


ClearAll[ClusterValidationEnsureDirectory];
ClusterValidationEnsureDirectory[path_String] := Module[{expanded = ExpandFileName[path]},
  If[! DirectoryQ[expanded], CreateDirectory[expanded, CreateIntermediateDirectories -> True]];
  expanded
];


ClearAll[ClusterValidationNormalizedDirectory];
ClusterValidationNormalizedDirectory[path_String] := ExpandFileName[FileNameJoin[{path, "."}]];


ClearAll[ClusterValidationWriteExpression];
ClusterValidationWriteExpression[path_String, expr_] :=
  Export[path, ToString[Unevaluated[expr], InputForm], "String"];


ClearAll[ClusterValidationExportCSV];
ClusterValidationExportCSV[path_String, rows_List] := Module[
  {headers, csvRows},
  headers = Sort@DeleteDuplicates@Flatten[Keys /@ Select[rows, AssociationQ]];
  csvRows = Join[
    {headers},
    Map[
      ClusterValidationString /@ Lookup[#, headers, Missing["NotAvailable"]] &,
      rows
    ]
  ];
  Export[path, csvRows, "CSV"]
];


scriptArgs = ClusterValidationScriptArguments[];
projectRoot = ExpandFileName[
  If[
    Length[scriptArgs] >= 1,
    scriptArgs[[1]],
    ClusterValidationProjectRootDefault[]
  ]
];
outputDir = ClusterValidationEnsureDirectory[
  ExpandFileName[
    If[
      Length[scriptArgs] >= 2,
      scriptArgs[[2]],
      Replace[
        ClusterValidationEnvironmentString["BAE_CLUSTER_VALIDATION_DIR"],
        {
          value_String :> value,
          _ :> FileNameJoin[{projectRoot, ".runs", "cluster-validation-wolfram"}]
        }
      ]
    ]
  ]
];

logFile = FileNameJoin[{outputDir, "ClusterValidation.log"}];
logStream = OpenWrite[logFile, BinaryFormat -> False];


ClearAll[ClusterValidationLog];
ClusterValidationLog[args___] := Module[{line},
  line = DateString[{"ISODate", "T", "Time"}] <> " " <>
    StringJoin[ClusterValidationString /@ {args}];
  WriteString[$Output, line <> "\n"];
  WriteString[logStream, line <> "\n"];
  Flush[logStream];
];


ClearAll[ClusterValidationProcessArtifacts];
ClusterValidationProcessArtifacts[prefix_String, result_Association] := Module[
  {stdoutPath, stderrPath},
  stdoutPath = FileNameJoin[{outputDir, prefix <> ".stdout.log"}];
  stderrPath = FileNameJoin[{outputDir, prefix <> ".stderr.log"}];
  Export[stdoutPath, Lookup[result, "StandardOutput", ""], "String"];
  Export[stderrPath, Lookup[result, "StandardError", ""], "String"];
  <|
    "StdoutFile" -> stdoutPath,
    "StderrFile" -> stderrPath
  |>
];


stageRows = {};


SetAttributes[ClusterValidationRunStage, HoldFirst];
ClusterValidationRunStage[name_String, expr_] := Module[
  {started, rawResult, stage},
  ClusterValidationLog["== ", name, " =="];
  started = AbsoluteTime[];
  rawResult = Quiet@Check[expr, $Failed];

  stage = Which[
    AssociationQ[rawResult],
    rawResult,
    rawResult === True,
    <|"Status" -> "ok", "Message" -> "True"|>,
    rawResult === False || rawResult === $Failed,
    <|"Status" -> "error", "Message" -> ClusterValidationString[rawResult]|>,
    True,
    <|"Status" -> "ok", "Message" -> ClusterValidationString[rawResult]|>
  ];

  stage = Join[
    <|
      "Stage" -> name,
      "Seconds" -> N[AbsoluteTime[] - started]
    |>,
    stage
  ];
  AppendTo[stageRows, stage];

  ClusterValidationLog[
    "[",
    ToUpperCase[Lookup[stage, "Status", "ok"]],
    "] ",
    name,
    " (",
    ClusterValidationString[NumberForm[Lookup[stage, "Seconds", 0], {Infinity, 3}]],
    " s): ",
    Lookup[stage, "Message", ""]
  ];

  stage
];


workflowFile = FileNameJoin[{projectRoot, "SUSYWBEWorkflow.wl"}];
workflowLoadedQ = False;
discoveredJuliaExecutable = Missing["NotDiscovered"];
explicitJuliaFromEnv = Quiet@Check[Environment["JULIA"], $Failed];
preferredBackendOptions = {};
preferredBackendLabel = Missing["NotAvailable"];

environmentSummary = <|
  "Timestamp" -> DateString[{"ISODate", "T", "Time"}],
  "ProjectRootArgument" -> projectRoot,
  "OutputDir" -> outputDir,
  "InputFileName" -> $InputFileName,
  "Directory" -> Directory[],
  "Version" -> $Version,
  "VersionNumber" -> $VersionNumber,
  "SystemID" -> $SystemID,
  "MachineName" -> Quiet@Check[$MachineName, Missing["NotAvailable"]],
  "UserBaseDirectory" -> Quiet@Check[$UserBaseDirectory, Missing["NotAvailable"]],
  "PathLength" -> Length[$Path],
  "EnvironmentJULIA" -> explicitJuliaFromEnv,
  "EnvironmentPATH" -> Quiet@Check[Environment["PATH"], Missing["NotAvailable"]],
  "EnvironmentHOSTNAME" -> Quiet@Check[Environment["HOSTNAME"], Missing["NotAvailable"]],
  "EnvironmentSLURMJobID" -> Quiet@Check[Environment["SLURM_JOB_ID"], Missing["NotAvailable"]],
  "CommandLine" -> $CommandLine,
  "ScriptCommandLine" -> $ScriptCommandLine
|>;

ClusterValidationLog["Project root: ", projectRoot];
ClusterValidationLog["Output dir:   ", outputDir];

ClusterValidationRunStage[
  "FolderStructure",
  Module[{requiredPaths, missingPaths},
    requiredPaths = {
      FileNameJoin[{projectRoot, "Project.toml"}],
      FileNameJoin[{projectRoot, "Manifest.toml"}],
      workflowFile,
      FileNameJoin[{projectRoot, "solver", "NumericalWBE_SUSY.m"}],
      FileNameJoin[{projectRoot, "solver", "SusyWronskianSolver.m"}],
      FileNameJoin[{projectRoot, "utils", "functions.m"}],
      FileNameJoin[{projectRoot, "julia", "SUSYWBEJuliaTaskRunner.jl"}],
      FileNameJoin[{projectRoot, "julia", "SUSYWBEPersistentWorker.jl"}],
      FileNameJoin[{projectRoot, "julia", "SUSYWBEJuliaSolver.jl"}],
      FileNameJoin[{projectRoot, "julia", "cluster_julia_diagnostic.jl"}]
    };
    missingPaths = Select[requiredPaths, ! FileExistsQ[#] &];
    <|
      "Status" -> If[missingPaths === {}, "ok", "error"],
      "Message" -> If[missingPaths === {}, "Required project files are present.", "Missing required project files."],
      "MissingPaths" -> missingPaths
    |>
  ]
];

ClusterValidationRunStage[
  "LoadWorkflow",
  Module[{},
    If[! FileExistsQ[workflowFile],
      Return[<|"Status" -> "error", "Message" -> "Workflow file was not found.", "WorkflowFile" -> workflowFile|>]
    ];
    Quiet@Check[Get[workflowFile], Return[<|"Status" -> "error", "Message" -> "Failed to load SUSYWBEWorkflow.wl.", "WorkflowFile" -> workflowFile|>]];
    workflowLoadedQ = True;
    preferredBackendOptions = PreferredJuliaBackendOptions[];
    If[! ListQ[preferredBackendOptions],
      Return[<|"Status" -> "error", "Message" -> "PreferredJuliaBackendOptions[] did not return option rules."|>]
    ];
    preferredBackendLabel = JuliaBackend /. preferredBackendOptions;
    <|
      "Status" -> "ok",
      "Message" -> "Loaded SUSYWBEWorkflow.wl successfully.",
      "ResolvedProjectRoot" -> SUSYWBEProjectDirectory[],
      "PreferredBackend" -> preferredBackendLabel,
      "DefaultSysimagePath" -> DefaultJuliaSysimagePath[]
    |>
  ]
];

ClusterValidationRunStage[
  "ProjectRootAssumptions",
  Module[{originalDirectory, resolvedProjectRoot, resolvedJuliaDir, runEnvironment},
    If[! workflowLoadedQ,
      Return[<|"Status" -> "error", "Message" -> "Workflow was not loaded."|>]
    ];

    originalDirectory = Directory[];
    Internal`WithLocalSettings[
      SetDirectory[$TemporaryDirectory],
      resolvedProjectRoot = SUSYWBEProjectDirectory[];
      resolvedJuliaDir = SUSYWBEJuliaDirectory[];
      runEnvironment = BuildSYTRunEnvironment[
        WorkDir -> FileNameJoin[{outputDir, "directory-assumption-check"}],
        SaveSubSYTCSVFiles -> False
      ],
      SetDirectory[originalDirectory]
    ];

    <|
      "Status" -> If[
        ClusterValidationNormalizedDirectory[resolvedProjectRoot] === ClusterValidationNormalizedDirectory[projectRoot] &&
        StringStartsQ[
          ClusterValidationNormalizedDirectory[resolvedJuliaDir],
          ClusterValidationNormalizedDirectory[projectRoot]
        ],
        "ok",
        "error"
      ],
      "Message" -> "Checked directory and path assumptions with a changed current working directory.",
      "ResolvedProjectRoot" -> resolvedProjectRoot,
      "ResolvedJuliaDir" -> resolvedJuliaDir,
      "SampleWorkDir" -> Lookup[runEnvironment, "WorkDir", Missing["NotAvailable"]]
    |>
  ]
];

ClusterValidationRunStage[
  "JuliaExecutableDiscovery",
  Module[{runEnvironment, explicitJuliaRules},
    If[! workflowLoadedQ,
      Return[<|"Status" -> "error", "Message" -> "Workflow was not loaded."|>]
    ];

    explicitJuliaRules = If[
      StringQ[explicitJuliaFromEnv] && StringTrim[explicitJuliaFromEnv] =!= "",
      {JuliaExecutable -> explicitJuliaFromEnv},
      {}
    ];
    runEnvironment = BuildSYTRunEnvironment[
      WorkDir -> FileNameJoin[{outputDir, "discovery-run"}],
      SaveSubSYTCSVFiles -> False,
      Sequence @@ explicitJuliaRules,
      Sequence @@ preferredBackendOptions
    ];
    discoveredJuliaExecutable = SUSYWBEJuliaExecutable[runEnvironment];

    <|
      "Status" -> If[StringQ[discoveredJuliaExecutable], "ok", "error"],
      "Message" -> "Resolved Julia executable for Wolfram-side launches.",
      "JuliaExecutable" -> discoveredJuliaExecutable,
      "RunnerCommand" -> BuildJuliaCommand[
        FileNameJoin[{SUSYWBEJuliaDirectory[], "SUSYWBEJuliaTaskRunner.jl"}],
        runEnvironment
      ],
      "WorkerCommand" -> BuildJuliaCommand[
        FileNameJoin[{SUSYWBEJuliaDirectory[], "SUSYWBEPersistentWorker.jl"}],
        runEnvironment
      ]
    |>
  ]
];

ClusterValidationRunStage[
  "JuliaVersionCommand",
  Module[{command, result, artifactPaths},
    If[! StringQ[discoveredJuliaExecutable],
      Return[<|"Status" -> "error", "Message" -> "Julia executable was not discovered."|>]
    ];

    command = {discoveredJuliaExecutable, "--version"};
    result = RunProcess[command, All];
    artifactPaths = If[AssociationQ[result], ClusterValidationProcessArtifacts["julia-version", result], <||>];

    Join[
      <|
        "Status" -> If[AssociationQ[result] && Lookup[result, "ExitCode", 1] === 0, "ok", "error"],
        "Message" -> If[AssociationQ[result], "Ran `julia --version` from Wolfram.", "RunProcess failed before returning process metadata."],
        "Command" -> command,
        "ExitCode" -> If[AssociationQ[result], Lookup[result, "ExitCode", Missing["NotAvailable"]], Missing["RunProcessFailed"]],
        "StandardOutput" -> If[AssociationQ[result], Lookup[result, "StandardOutput", ""], ""],
        "StandardError" -> If[AssociationQ[result], Lookup[result, "StandardError", ""], ""]
      |>,
      artifactPaths
    ]
  ]
];

ClusterValidationRunStage[
  "JuliaProjectCommand",
  Module[{command, result, artifactPaths},
    If[! StringQ[discoveredJuliaExecutable],
      Return[<|"Status" -> "error", "Message" -> "Julia executable was not discovered."|>]
    ];

    command = {
      discoveredJuliaExecutable,
      "--project=" <> projectRoot,
      "-e",
      "using Pkg; println(Base.active_project()); println(\"MATHEMATICA_JULIA_OK\")"
    };
    result = RunProcess[command, All];
    artifactPaths = If[AssociationQ[result], ClusterValidationProcessArtifacts["julia-project-command", result], <||>];

    Join[
      <|
        "Status" -> If[
          AssociationQ[result] &&
          Lookup[result, "ExitCode", 1] === 0 &&
          StringContainsQ[Lookup[result, "StandardOutput", ""], "MATHEMATICA_JULIA_OK"],
          "ok",
          "error"
        ],
        "Message" -> If[
          AssociationQ[result],
          "Ran a trivial Julia command with the project environment from Wolfram.",
          "RunProcess failed before returning process metadata."
        ],
        "Command" -> command,
        "ExitCode" -> If[AssociationQ[result], Lookup[result, "ExitCode", Missing["NotAvailable"]], Missing["RunProcessFailed"]],
        "StandardOutput" -> If[AssociationQ[result], Lookup[result, "StandardOutput", ""], ""],
        "StandardError" -> If[AssociationQ[result], Lookup[result, "StandardError", ""], ""]
      |>,
      artifactPaths
    ]
  ]
];

ClusterValidationRunStage[
  "PersistentWorkerProbe",
  Module[
    {
      explicitJuliaRules, runEnvironment, workerCommand, workerInfo, workerProcess,
      readyProbe, quitReply, fallbackEnvironment, fallbackUsedQ = False,
      message = "Persistent worker probe succeeded.", status = "ok"
    },
    If[! workflowLoadedQ,
      Return[<|"Status" -> "error", "Message" -> "Workflow was not loaded."|>]
    ];

    explicitJuliaRules = If[
      StringQ[discoveredJuliaExecutable],
      {JuliaExecutable -> discoveredJuliaExecutable},
      {}
    ];
    runEnvironment = BuildSYTRunEnvironment[
      WorkDir -> FileNameJoin[{outputDir, "persistent-worker-probe"}],
      SaveSubSYTCSVFiles -> False,
      Sequence @@ explicitJuliaRules,
      Sequence @@ preferredBackendOptions
    ];
    workerCommand = BuildJuliaCommand[
      FileNameJoin[{SUSYWBEJuliaDirectory[], "SUSYWBEPersistentWorker.jl"}],
      runEnvironment
    ];
    workerInfo = StartPersistentJuliaWorker[runEnvironment];

    If[
      workerInfo === $Failed &&
      NormalizeJuliaBackend[Lookup[runEnvironment, "JuliaBackend", "ProcessPerRun"]] === "PersistentSessionSysimage",
      fallbackEnvironment = PlainJuliaRunConfiguration[runEnvironment];
      workerInfo = StartPersistentJuliaWorker[fallbackEnvironment];
      If[workerInfo =!= $Failed,
        fallbackUsedQ = True;
        runEnvironment = fallbackEnvironment;
        workerCommand = BuildJuliaCommand[
          FileNameJoin[{SUSYWBEJuliaDirectory[], "SUSYWBEPersistentWorker.jl"}],
          runEnvironment
        ];
        status = "warning";
        message = "Preferred sysimage worker did not start; plain persistent worker succeeded."
      ]
    ];

    If[workerInfo === $Failed,
      Return[<|
        "Status" -> "error",
        "Message" -> "Failed to start the Julia persistent worker from Wolfram.",
        "Command" -> workerCommand
      |>]
    ];

    workerProcess = workerInfo["Process"];
    WriteLine[workerProcess, "PING"];
    readyProbe = ReadJuliaWorkerLine[workerProcess, Lookup[runEnvironment, "JuliaTimeout", 900]];
    WriteLine[workerProcess, "QUIT"];
    quitReply = ReadJuliaWorkerLine[workerProcess, Lookup[runEnvironment, "JuliaTimeout", 900]];
    Quiet@Check[KillProcess[workerProcess], Null];

    <|
      "Status" -> If[
        readyProbe === "PONG" && quitReply === "BYE",
        status,
        "error"
      ],
      "Message" -> If[
        readyProbe === "PONG" && quitReply === "BYE",
        message,
        "Persistent worker did not answer the PING/QUIT probe correctly."
      ],
      "Command" -> workerCommand,
      "WorkerBackend" -> Lookup[runEnvironment, "JuliaBackend", Missing["NotAvailable"]],
      "FallbackUsedQ" -> fallbackUsedQ,
      "StartupSeconds" -> Lookup[workerInfo, "StartupSeconds", Missing["NotAvailable"]],
      "PingReply" -> readyProbe,
      "QuitReply" -> quitReply
    |>
  ]
];


ClearAll[ClusterValidationSmokeResult];
ClusterValidationSmokeResult[result_Association, launchCommand_List] := Module[
  {
    runEnvironment, outputFile, initialDataFile, stdoutFile, stderrFile,
    juliaTimingFile, workflowTimingFile, outputCSV, outputVectorNumericQ,
    issues = {}, juliaStageTimings
  },
  runEnvironment = Lookup[result, "RunEnvironment", <||>];
  outputFile = Lookup[runEnvironment, "OutputFile", Missing["NotAvailable"]];
  initialDataFile = Lookup[runEnvironment, "InitialDataFile", Missing["NotAvailable"]];
  stdoutFile = Lookup[runEnvironment, "StdoutFile", Missing["NotAvailable"]];
  stderrFile = Lookup[runEnvironment, "StderrFile", Missing["NotAvailable"]];
  juliaTimingFile = Lookup[runEnvironment, "JuliaTimingFile", Missing["NotAvailable"]];
  workflowTimingFile = Lookup[result, "WorkflowTimingFile", Missing["NotAvailable"]];

  If[! TrueQ[Lookup[result, "SucceededQ", False]],
    AppendTo[issues, "RunSingleSYT reported failure: " <> ClusterValidationString[Lookup[result, "FailureReason", "Unknown"]]]
  ];
  If[! StringQ[initialDataFile] || ! FileExistsQ[initialDataFile],
    AppendTo[issues, "initial_data.csv was not produced."]
  ];
  If[! StringQ[outputFile] || ! FileExistsQ[outputFile],
    AppendTo[issues, "output.csv was not produced."]
  ];
  If[! StringQ[juliaTimingFile] || ! FileExistsQ[juliaTimingFile],
    AppendTo[issues, "julia.timings.csv was not produced."]
  ];
  If[! StringQ[workflowTimingFile] || ! FileExistsQ[workflowTimingFile],
    AppendTo[issues, "workflow.timings.csv was not produced."]
  ];
  If[! StringQ[stdoutFile] || ! FileExistsQ[stdoutFile],
    AppendTo[issues, "Julia stdout log was not produced."]
  ];
  If[! StringQ[stderrFile] || ! FileExistsQ[stderrFile],
    AppendTo[issues, "Julia stderr log was not produced."]
  ];

  outputCSV = If[StringQ[outputFile] && FileExistsQ[outputFile], Quiet@Check[Import[outputFile, "CSV"], $Failed], $Failed];
  outputVectorNumericQ = Quiet@Check[
    ListQ[outputCSV] &&
    Length[outputCSV] >= 2 &&
    AllTrue[Rest[outputCSV], MatchQ[{_, _?NumericQ, ___}] &],
    False
  ];
  If[outputCSV === $Failed || ! outputVectorNumericQ,
    AppendTo[issues, "output.csv did not contain a numeric solution vector."]
  ];

  juliaStageTimings = Lookup[result, "JuliaStageTimings", {}];
  If[Length[juliaStageTimings] < 1,
    AppendTo[issues, "No Julia stage timings were recorded."]
  ];

  <|
    "Status" -> If[issues === {}, "ok", "error"],
    "Message" -> If[issues === {}, "Smoke test completed successfully.", StringRiffle[issues, " "]],
    "LaunchCommand" -> launchCommand,
    "JuliaBackend" -> Lookup[runEnvironment, "JuliaBackend", Missing["NotAvailable"]],
    "InitialDataFile" -> initialDataFile,
    "OutputFile" -> outputFile,
    "StdoutFile" -> stdoutFile,
    "StderrFile" -> stderrFile,
    "JuliaTimingFile" -> juliaTimingFile,
    "WorkflowTimingFile" -> workflowTimingFile,
    "FailureReason" -> Lookup[result, "FailureReason", None],
    "BetheRoots" -> Lookup[result, "BetheRoots", Missing["NotAvailable"]],
    "JuliaStageTimingCount" -> Length[juliaStageTimings]
  |>
];


SetMarkedPointSelector[UseRandomMinimalMarkedPoint];
SetSingleSYTRunConfiguration[
  InitialLambda -> 70,
  TargetLambda -> 0,
  StartInterpolationOrder -> 2,
  WorkingPrecision -> 40,
  InterpolationOrder -> 3,
  IteratorFunction -> IterateUptdInterimJulia
];

smokeSYT = {{1, 3}, {2}};

ClusterValidationRunStage[
  "SmokeTestPreferredBackend",
  Module[
    {explicitJuliaRules, smokeDir, runOptions, result, runnerPath, launchCommand, stageResult},
    If[! workflowLoadedQ,
      Return[<|"Status" -> "error", "Message" -> "Workflow was not loaded."|>]
    ];

    ResetJuliaSession[];
    explicitJuliaRules = If[StringQ[discoveredJuliaExecutable], {JuliaExecutable -> discoveredJuliaExecutable}, {}];
    smokeDir = FileNameJoin[{outputDir, "smoke-preferred"}];
    runOptions = Join[
      {
        WorkDir -> smokeDir,
        MuteProgress -> True,
        SaveSubSYTCSVFiles -> False,
        SaveBetheRootsBySYT -> False,
        SaveWorkflowTimingCSV -> True
      },
      explicitJuliaRules,
      preferredBackendOptions
    ];
    result = RunSingleSYT[smokeSYT, Sequence @@ runOptions];
    runnerPath = FileNameJoin[{SUSYWBEJuliaDirectory[], "SUSYWBEPersistentWorker.jl"}];
    launchCommand = BuildJuliaCommand[runnerPath, Lookup[result, "RunEnvironment", <||>]];
    stageResult = ClusterValidationSmokeResult[result, launchCommand];
    ClusterValidationWriteExpression[
      FileNameJoin[{smokeDir, "smoke-preferred-result.wl"}],
      result
    ];
    ResetJuliaSession[];
    stageResult
  ]
];

ClusterValidationRunStage[
  "SmokeTestProcessPerRunFallback",
  Module[
    {explicitJuliaRules, smokeDir, runOptions, result, runnerPath, launchCommand, stageResult},
    If[! workflowLoadedQ,
      Return[<|"Status" -> "error", "Message" -> "Workflow was not loaded."|>]
    ];

    ResetJuliaSession[];
    explicitJuliaRules = If[StringQ[discoveredJuliaExecutable], {JuliaExecutable -> discoveredJuliaExecutable}, {}];
    smokeDir = FileNameJoin[{outputDir, "smoke-process-per-run"}];
    runOptions = Join[
      {
        WorkDir -> smokeDir,
        MuteProgress -> True,
        SaveSubSYTCSVFiles -> False,
        SaveBetheRootsBySYT -> False,
        SaveWorkflowTimingCSV -> True,
        JuliaBackend -> "ProcessPerRun",
        JuliaProcessFlags -> DefaultFastJuliaProcessFlags[]
      },
      explicitJuliaRules
    ];
    result = RunSingleSYT[smokeSYT, Sequence @@ runOptions];
    runnerPath = FileNameJoin[{SUSYWBEJuliaDirectory[], "SUSYWBEJuliaTaskRunner.jl"}];
    launchCommand = BuildJuliaCommand[runnerPath, Lookup[result, "RunEnvironment", <||>]];
    stageResult = ClusterValidationSmokeResult[result, launchCommand];
    ClusterValidationWriteExpression[
      FileNameJoin[{smokeDir, "smoke-process-per-run-result.wl"}],
      result
    ];
    ResetJuliaSession[];
    stageResult
  ]
];

summary = <|
  "Environment" -> environmentSummary,
  "WorkflowLoadedQ" -> workflowLoadedQ,
  "DiscoveredJuliaExecutable" -> discoveredJuliaExecutable,
  "PreferredBackend" -> preferredBackendLabel,
  "Stages" -> stageRows,
  "OverallStatus" -> If[Count[Lookup[stageRows, "Status", "ok"], "error"] > 0, "error", "ok"],
  "LogFile" -> logFile
|>;

ClusterValidationWriteExpression[
  FileNameJoin[{outputDir, "ClusterValidation.summary.wl"}],
  summary
];
ClusterValidationExportCSV[
  FileNameJoin[{outputDir, "ClusterValidation.stages.csv"}],
  stageRows
];

ClusterValidationLog["Cluster validation summary written to ", FileNameJoin[{outputDir, "ClusterValidation.summary.wl"}]];
ClusterValidationLog["Cluster validation stage CSV written to ", FileNameJoin[{outputDir, "ClusterValidation.stages.csv"}]];

Close[logStream];

Exit[If[summary["OverallStatus"] === "ok", 0, 1]];
