(* Wolfram Language Package *)

BeginPackage["FunctionRepo`TwoSidedLogTransform`", {"FunctionRepo`", "GeneralUtilities`"}]
(* Exported symbols added here with SymbolName::usage *)
GeneralUtilities`SetUsage[TwoSidedLogTransform, 
	"TwoSidedLogTransform[n$] is a function that log-transforms large numbers (both negative and positive) in an invertible (1 times) differentiable manner.
InverseFunction[TwoSidedLogTransform] returns the inverse of the function."
];

Begin["`Private`"] (* Begin Private Context *) 

SetAttributes[TwoSidedLogTransform, Listable];
TwoSidedLogTransform[n_] := If[ Abs[n] <= 1,
	n,
	Sign[n] * (1 + Log[Abs[n]])
];

TwoSidedLogTransform /: InverseFunction[TwoSidedLogTransform] := Function[n, 
	If[ Abs[n] <= 1,
		n,
		Sign[n] * Exp[Abs[n] - 1]
	],
	{Listable}
];

End[] (* End Private Context *)

EndPackage[]