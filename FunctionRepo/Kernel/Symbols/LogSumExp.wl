(* Wolfram Language Package *)

BeginPackage["FunctionRepo`LogSumExpLayer`", {"FunctionRepo`"}]
(* Exported symbols added here with SymbolName::usage *)
GeneralUtilities`SetUsage[LogSumExp,
	"LogSumExp[array$] LogSumExp of an array on level 1.
LogSumExp[array$, spec$] computes the LogSumExp using a level specification spec$ for ArrayReduce."
];

Begin["`Private`"] (* Begin Private Context *)

logSumExpVec[v_] := With[{
	m = Max[v]
},
	Plus[
		Log @ Total[Exp @ Subtract[v, m]],
		m
	]
];

LogSumExp[array_] := LogSumExp[array, {1}];
LogSumExp[array_, spec_] := ArrayReduce[logSumExpVec, array, spec];

End[] (* End Private Context *)

EndPackage[]


