(* Wolfram Language Package *)

BeginPackage["FunctionRepo`NoEntry`", {"FunctionRepo`"}]
(* Exported symbols added here with SymbolName::usage *)
GeneralUtilities`SetUsage[NoEntry,
	"NoEntry[expr$1, expr$2, $$] is a holding construct that cannot be accessed by Part, ReplaceAll, Map, etc."
];

Begin["`Private`"] (* Begin Private Context *)

SetAttributes[NoEntry, HoldAll];
expr : NoEntry[___]?System`Private`HoldEntryQ := System`Private`HoldSetNoEntry[expr];

End[] (* End Private Context *)

EndPackage[]
