(* Wolfram Language Package *)

BeginPackage["FunctionRepo`BlockDirectory`", {"FunctionRepo`"}]
(* Exported symbols added here with SymbolName::usage *)
GeneralUtilities`SetUsage[BlockDirectory,
	"BlockDirectory[dir$, expr$] makes dir$ the default directory during the evaluation of expr$ and then resets it."
];

Begin["`Private`"] (* Begin Private Context *) 

SetAttributes[BlockDirectory, HoldRest];

BlockDirectory[dir_, expr_] := WithCleanup[
	SetDirectory[dir],
	expr,
	ResetDirectory[]
];

End[] (* End Private Context *)

EndPackage[]