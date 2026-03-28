(* ::Package:: *)

Needs["Combinatorica`"];

Get[FileNameJoin[{DirectoryName[$InputFileName], "SUSYWBEWorkflow.wl"}]];

SetMarkedPointSelector[UseRandomMinimalMarkedPoint];
SetSingleSYTRunConfiguration[
  InitialLambda -> 70,
  TargetLambda -> 0,
  StartInterpolationOrder -> 2,
  WorkingPrecision -> 40,
  InterpolationOrder -> 3,
  IteratorFunction -> IterateUptdInterimJulia
];

Result221 = {
  {{{-0.5560934570249785`, -0.06570592636335032`, 0.3840352543145404`}, {-0.4755282581475768211021204479038715362548828125`40.}}},
  {{{-0.5390996433799914803057335395217795296371818235167730548591`39.26279617214681, 0.1960766651534366118927224784717099461138665999297732461794`38.57486827085751 - 0.5002178630304553339950610391834525653335289402235440249486`38.98160155551995 I, 0.1960766651534366118927224784717099461138665999297732461794`38.57486827085751 + 0.5002178630304553339950610391834525653335289402235440249486`38.98160155551995 I}, {-0.293892626146236513040577165156719274818897247314453125`40.}}},
  {{{0.`, 0.` - 0.5` I, 0.` + 0.5` I}, {0}}},
  {{{-0.3840352543145402`, 0.06570592636335026`, 0.5560934570249788`}, {0.475528258147576765590969216646044515073299407958984375`40.}}},
  {{{-0.19607666515343644` - 0.5002178630304556` I, -0.19607666515343644` + 0.5002178630304556` I, 0.5390996433799913`}, {0.293892626146236513040577165156719274818897247314453125`40.}}}
};

yd = {2, 2, 1};
allsyt = Combinatorica`Tableaux[yd];
currentWorkingOptions = {
  JuliaBackend -> "ProcessPerRun",
  JuliaProcessFlags -> DefaultFastJuliaProcessFlags[]
};
optimizedOptions = PreferredJuliaBackendOptions[];
optimizedBackend = JuliaBackend /. optimizedOptions;

serialRun = RunAllSYTSerial[
  allsyt,
  WorkDir -> FileNameJoin[{SUSYWBEProjectDirectory[], ".runs", "validation-fast-serial-221"}],
  Sequence @@ currentWorkingOptions
];

parallelRun = RunAllSYTParallel[
  allsyt,
  WorkDir -> FileNameJoin[{SUSYWBEProjectDirectory[], ".runs", "validation-fast-parallel-221"}],
  Sequence @@ optimizedOptions
];

validation = ValidateSYTRunResults[serialRun, parallelRun, Result221, 10^-12];

Print["tableaux processed: ", validation["NumberOfTableaux"]];
Print["optimized backend: ", optimizedBackend];
Print["serial successful: ", Count[serialRun["RunResults"][[All, "SucceededQ"]], True]];
Print["parallel successful: ", Count[parallelRun["RunResults"][[All, "SucceededQ"]], True]];
Print["serial timing (s): ", NumberForm[serialRun["Timing"], {Infinity, 3}]];
Print["parallel timing (s): ", NumberForm[parallelRun["Timing"], {Infinity, 3}]];
Print["serial vs parallel match: ", validation["SerialParallelMatchQ"]];
Print["parallel vs Result221 match: ", validation["ParallelReferenceMatchQ"]];
Print["serial vs parallel max diff: ", InputForm[validation["SerialParallelMaxDifference"]]];
Print["parallel vs Result221 max diff: ", InputForm[validation["ParallelReferenceMaxDifference"]]];
Print["reference tolerance: ", InputForm[validation["Tolerance"]]];
Print["sample output file: ", parallelRun["RunResults"][[1, "RunEnvironment", "OutputFile"]]];
Print["sample stdout log: ", parallelRun["RunResults"][[1, "RunEnvironment", "StdoutFile"]]];
Print["sample stderr log: ", parallelRun["RunResults"][[1, "RunEnvironment", "StderrFile"]]];
