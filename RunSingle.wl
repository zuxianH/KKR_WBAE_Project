(* ::Package:: *)

(* Main entry point for computing Bethe-ansatz solutions from standard
   Young tableaux. The implementation details live in SUSYWBEWorkflow.wl. *)

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
        "BetheRoots" -> betheRoots
      |>
    ],
    {k, usableLength}
  ]
];


ClearAll[SaveSubSYTResultCSVRows];
SaveSubSYTResultCSVRows[subResult_Association] := {
  {"Tableau", "YoungDiagram", "BetheRoots"},
  {
    ToString[Lookup[subResult, "Tableau", {}], InputForm],
    ToString[Lookup[subResult, "YoungDiagram", {}], InputForm],
    ToString[Lookup[subResult, "BetheRoots", Missing["NotAvailable"]], InputForm]
  }
};


ClearAll[SaveIntermediateSubSYTResults];
SaveIntermediateSubSYTResults[subResults_List, resultDirectory_String] := Module[
  {directory, intermediateResults},
  directory = EnsureSUSYWBEDirectory[resultDirectory];
  intermediateResults = Most[subResults];
  Table[
    Module[{tableau, outputFile},
      tableau = Lookup[subResult, "Tableau", {}];
      outputFile = FileNameJoin[{directory, SYTFileName[tableau]}];
      Export[outputFile, SaveSubSYTResultCSVRows[subResult], "CSV"];
      outputFile
    ],
    {subResult, intermediateResults}
  ]
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

saveJuliaIntermediateCSVFiles = True;
saveFinalSYTFile = True;
saveAllSubSYT = True;
saveWorkflowTimingCSV = False;
subSYTResultDirectory = FileNameJoin[{SUSYWBEProjectDirectory[], "Result_SYT"}];

syt = {{1, 3, 7}, {2, 5}, {4, 6}};

result = RunSingleSYT[
  syt,
  MuteProgress -> True,
  SaveSubSYTCSVFiles -> saveJuliaIntermediateCSVFiles,
  SaveBetheRootsBySYT -> saveFinalSYTFile,
  SaveWorkflowTimingCSV -> saveWorkflowTimingCSV,
  Sequence @@ PreferredJuliaBackendOptions[]
];

If[! TrueQ[Lookup[result, "SucceededQ", False]],
  PrintRunFailureSummary[result]
];

subSYTResult = If[
  TrueQ[Lookup[result, "SucceededQ", False]],
  BuildSubSYTResult[result],
  {}
];

savedSubSYTFiles = If[
  saveAllSubSYT && subSYTResult =!= {},
  SaveIntermediateSubSYTResults[subSYTResult, subSYTResultDirectory],
  {}
];

result["BetheRoots"]
