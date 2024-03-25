(* Wolfram Language Package *)

BeginPackage["FunctionRepo`HeadCount`", {"FunctionRepo`"}]
(* Exported symbols added here with SymbolName::usage *)
GeneralUtilities`SetUsage[HeadCount,
	"HeadCount[f$[$$][$$][$$]] counts how many nested levels of heads there are in an expression."
];

Begin["`Private`"] (* Begin Private Context *)

HeadCount[expr_] := iheadCount[expr];

SetAttributes[iheadCount, HoldFirst];
iheadCount[head_[___]] := iheadCount[head] + 1
iheadCount[_] := 0

End[] (* End Private Context *)

EndPackage[]
