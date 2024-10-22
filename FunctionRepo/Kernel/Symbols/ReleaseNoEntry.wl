(* Wolfram Language Package *)

BeginPackage["FunctionRepo`ReleaseNoEntry`", {"FunctionRepo`"}]
(* Exported symbols added here with SymbolName::usage *)
GeneralUtilities`SetUsage[ReleaseNoEntry,
	"ReleaseNoEntry[expr$] removes NoEntry wrappers inside an expression."
];

Begin["`Private`"] (* Begin Private Context *)

ReleaseNoEntry[expr_] := ReplaceAll[expr, FunctionRepo`NoEntry[e___] :> e];

End[] (* End Private Context *)

EndPackage[]
