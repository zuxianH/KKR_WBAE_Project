(* ::Package:: *)

Off[GraphJoin::shdw];
Off[GraphProduct::shdw];
Off[GraphSum::shdw];
Quiet[
  Needs["Combinatorica`"],
  {
    General::compat,
    SetDelayed::write
  }
];
On[GraphJoin::shdw];
On[GraphProduct::shdw];
On[GraphSum::shdw];

ClearAll[SUSYWBEProjectDirectory];
SUSYWBEProjectDirectory[] := Module[
  {inputDirectory, notebookDirectory},
  inputDirectory = If[
    StringQ[$InputFileName] && $InputFileName =!= "",
    DirectoryName[$InputFileName],
    $Failed
  ];
  notebookDirectory = Quiet[
    Check[NotebookDirectory[], $Failed],
    FrontEndObject::notavail
  ];

  SelectFirst[
    {inputDirectory, notebookDirectory, Directory[]},
    StringQ[#] && DirectoryQ[#] &,
    Directory[]
  ]
];

Get[FileNameJoin[{SUSYWBEProjectDirectory[], "SUSYWBEWorkflow.wl"}]];


ClearAll[PrintRunFailureSummary];
PrintRunFailureSummary[result_Association] := Module[
  {runEnvironment, timedResult, timingSeconds},
  runEnvironment = Lookup[result, "RunEnvironment", <||>];
  timedResult = Lookup[result, "TimedResult", Missing["NotAvailable"]];
  timingSeconds = Replace[
    timedResult,
    {
      values_List /; Length[values] >= 1 :> First[values],
      other_ :> other
    }
  ];

  Print["Run failed."];
  Print["FailureReason: ", Lookup[result, "FailureReason", "Unknown"]];
  Print["RunID: ", Lookup[runEnvironment, "RunID", "Unknown"]];
  Print["Result CSV: ", Lookup[result, "BetheRootsFile", "Not saved"]];
  Print["Julia stdout log: ", Lookup[runEnvironment, "StdoutFile", "Unavailable"]];
  Print["Julia stderr log: ", Lookup[runEnvironment, "StderrFile", "Unavailable"]];
  Print["Julia timing CSV: ", Lookup[runEnvironment, "JuliaTimingFile", "Unavailable"]];

  If[NumberQ[timingSeconds] && timingSeconds < 1,
    Print["The run failed almost immediately. This usually means the Julia backend did not start correctly."];
    Print["Run ./check_julia_setup.sh in this runtime directory and inspect the Julia stderr log above."]
  ];
];


ClearAll[ConvertFromCoefficientToRootsYD];
ConvertFromCoefficientToRootsYD[solc_, localYD_] := Module[
  {solList, allBetheQ, allBetheRootEqns, betheRoots},
  solList = If[
    MatchQ[solc, {(_Rule)...}],
    {solc},
    solc
  ];

  Map[
    Function[sol,
      allBetheQ = Table[YQa[w, 0, localYD], {w, Length[localYD] - 1}] /. sol;
      allBetheRootEqns = Thread[allBetheQ == 0];
      betheRoots = Map[
        Function[eqn,
          Module[{rootSolve = Solve[eqn, u]},
            If[rootSolve === {}, Missing["NoSolution"], u /. rootSolve]
          ]
        ],
        allBetheRootEqns
      ];
      betheRoots
    ],
    solList
  ][[1]]
];


ClearAll[BuildSubSYTResult];
BuildSubSYTResult[result_Association] := Module[
  {tableau, solutionHistory, subTableaux, solutionTriples, usableLength},
  tableau = Lookup[result, "Tableau", Missing["NotAvailable"]];
  solutionHistory = Lookup[result, "TimedResult", Missing["NotAvailable"]];
  If[
    ! ListQ[solutionHistory] || Length[solutionHistory] < 2,
    Return[{}]
  ];

  subTableaux = Quiet@Check[SYTlist[tableau], {}];
  solutionTriples = solutionHistory[[2]];
  usableLength = Min[Length[subTableaux], Length[solutionTriples]];

  Table[
    Module[{subTableau, subYD, coefficientSolution, betheRoots},
      subTableau = subTableaux[[k]];
      subYD = ToYD[subTableau];
      coefficientSolution = solutionTriples[[k, 3]];
      betheRoots = ConvertFromCoefficientToRootsYD[coefficientSolution, subYD];
      <|
        "Tableau" -> subTableau,
        "YoungDiagram" -> subYD,
        "BetheRoots" -> betheRoots,
        "ParentTableau" -> tableau
      |>
    ],
    {k, usableLength}
  ]
];


ClearAll[BuildAllSubSYTResults];
BuildAllSubSYTResults[parallelRun_Association] := Module[{successfulRunResults},
  successfulRunResults = Select[
    Lookup[parallelRun, "RunResults", {}],
    TrueQ[Lookup[#, "SucceededQ", False]] &
  ];

  Map[
    Function[runResult,
      <|
        "FinalTableau" -> Lookup[runResult, "Tableau", {}],
        "SubResults" -> BuildSubSYTResult[runResult]
      |>
    ],
    successfulRunResults
  ]
];


ClearAll[UniqueSubSYTResults];
UniqueSubSYTResults[subResultGroups_List] := Module[
  {flatSubResults, groupedSubResults},
  flatSubResults = Flatten[
    Replace[
      Lookup[subResultGroups, "SubResults", {}],
      results_List :> Most[results],
      {1}
    ],
    1
  ];
  groupedSubResults = GroupBy[
    flatSubResults,
    ToString[Lookup[#, "Tableau", {}], InputForm] &
  ];
  Values[Map[First, groupedSubResults]]
];


ClearAll[SaveSubSYTResultCSVRows];
SaveSubSYTResultCSVRows[subResult_Association] := {
  {"Tableau", "YoungDiagram", "ParentTableau", "BetheRoots"},
  {
    ToString[Lookup[subResult, "Tableau", {}], InputForm],
    ToString[Lookup[subResult, "YoungDiagram", {}], InputForm],
    ToString[Lookup[subResult, "ParentTableau", {}], InputForm],
    ToString[Lookup[subResult, "BetheRoots", Missing["NotAvailable"]], InputForm]
  }
};


ClearAll[SaveAllSubSYTResults];
SaveAllSubSYTResults[subResults_List, resultDirectory_String] := Module[
  {directory, uniqueSubResults},
  directory = EnsureSUSYWBEDirectory[resultDirectory];
  uniqueSubResults = UniqueSubSYTResults[subResults];

  Table[
    Module[{tableau, outputFile},
      tableau = Lookup[subResult, "Tableau", {}];
      outputFile = FileNameJoin[{directory, SYTFileName[tableau]}];
      Export[outputFile, SaveSubSYTResultCSVRows[subResult], "CSV"];
      outputFile
    ],
    {subResult, uniqueSubResults}
  ]
];


ClearAll[PrintPlainSummaryLine];
PrintPlainSummaryLine[label_String, value_] :=
  WriteString[$Output, label <> ToString[value, OutputForm] <> "\n"];


SetMarkedPointSelector[UseRandomMinimalMarkedPoint];
SetSingleSYTRunConfiguration[
  InitialLambda -> 70,
  TargetLambda -> 0,
  StartInterpolationOrder -> 2,
  WorkingPrecision -> 40,
  InterpolationOrder -> 3,
  IteratorFunction -> IterateUptdInterimJulia
];

saveJuliaIntermediateCSVFiles = True;
saveFinalSYTFile = True;
saveAllSubSYT = True;
saveWorkflowTimingCSV = False;
muteProgress = True;
resultSYTDirectory = FileNameJoin[{SUSYWBEProjectDirectory[], "Result_SYT"}];

yd = {2, 2, 1};
allsyt = Combinatorica`Tableaux[yd];
parallelRunWorkDir = FileNameJoin[{
  SUSYWBEProjectDirectory[],
  ".runs",
  StringRiffle[ToString /@ yd, "_"] <> "_parallelRun"
}];

parallelRun = RunAllSYTParallel[
  allsyt,
  WorkDir -> parallelRunWorkDir,
  MuteProgress -> muteProgress,
  SaveSubSYTCSVFiles -> saveJuliaIntermediateCSVFiles,
  SaveBetheRootsBySYT -> saveFinalSYTFile,
  SaveWorkflowTimingCSV -> saveWorkflowTimingCSV,
  ResultSYTDirectory -> resultSYTDirectory,
  Sequence @@ PreferredJuliaBackendOptions[]
];

failedRunResults = Select[
  parallelRun["RunResults"],
  ! TrueQ[Lookup[#, "SucceededQ", False]] &
];
Scan[PrintRunFailureSummary, failedRunResults];

allSubSYTResult = If[
  parallelRun["RunResults"] === {},
  {},
  BuildAllSubSYTResults[parallelRun]
];

savedSubSYTFiles = If[
  saveAllSubSYT && allSubSYTResult =!= {},
  SaveAllSubSYTResults[allSubSYTResult, resultSYTDirectory],
  {}
];

PrintPlainSummaryLine["tableaux processed: ", Length[Lookup[parallelRun, "Tableaux", {}]]];
PrintPlainSummaryLine["successful tableaux: ", Count[Lookup[parallelRun, "RunResults", {}][[All, "SucceededQ"]], True]];
PrintPlainSummaryLine[
  "parallel timing (s): ",
  N[Round[1000 * N[Lookup[parallelRun, "Timing", 0]]] / 1000, 10]
];

parallelRun["BetheRoots"]
