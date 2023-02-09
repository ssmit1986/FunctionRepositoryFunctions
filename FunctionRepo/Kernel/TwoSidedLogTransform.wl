(* Wolfram Language Package *)

BeginPackage["FunctionRepo`TwoSidedLogTransform`", {"FunctionRepo`", "GeneralUtilities`"}]
(* Exported symbols added here with SymbolName::usage *)
GeneralUtilities`SetUsage[TwoSidedLogTransform, 
	"TwoSidedLogTransform[n$] is a function that log-transforms large numbers (both negative and positive) in an invertible (1 times) differentiable manner.
InverseFunction[TwoSidedLogTransform] returns the inverse of the function."
];

Begin["`Private`"] (* Begin Private Context *) 

SetAttributes[TwoSidedLogTransform, Listable];
TwoSidedLogTransform[] = Function[
	Null,
	If[ Abs[#] <= 1,
		#,
		Sign[#] * (1 + Log[Abs[#]])
	],
	{Listable}
];

TwoSidedLogTransform[n_] := TwoSidedLogTransform[] @ n;

TwoSidedLogTransform /: InverseFunction[TwoSidedLogTransform] := Function[
	Null, 
	If[ Abs[#] <= 1,
		#,
		Sign[#] * Exp[Abs[#] - 1]
	],
	{Listable}
];

End[] (* End Private Context *)

EndPackage[]