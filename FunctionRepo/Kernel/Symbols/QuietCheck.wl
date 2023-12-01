(* Wolfram Language Package *)

BeginPackage["FunctionRepo`QuietCheck`", {"FunctionRepo`"}]
(* Exported symbols added here with SymbolName::usage *)
GeneralUtilities`SetUsage[QuietCheck,
	"QuietCheck[expr$, failexpr$] combines the functionalities of Quiet and Check.
QuietCheck[expr$, failexpr$, msgs$] only Quiets and Checks for the given messages msgs$."
];


Begin["`Private`"] (* Begin Private Context *)

SetAttributes[QuietCheck, {HoldAll}];
QuietCheck[expr_, failexpr_, msgs_] := Quiet[Check[expr, failexpr, msgs], msgs];
QuietCheck[expr_, failexpr_] := Quiet[Check[expr, failexpr]];

End[] (* End Private Context *)

EndPackage[]
