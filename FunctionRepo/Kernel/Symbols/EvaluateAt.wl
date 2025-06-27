(* Wolfram Language Package *)

BeginPackage["FunctionRepo`EvaluateAt`", {"FunctionRepo`", "GeneralUtilities`"}]
(* Exported symbols added here with SymbolName::usage *)
GeneralUtilities`SetUsage[EvaluateAt,
	"EvaluateAt[expr$, pos$] evaluates a sub-part of a held expression."
];

Begin["`Private`"] (* Begin Private Context *) 

EvaluateAt[expr_, {}, h : RepeatedNull[_, 1]] := expr;
EvaluateAt[expr_, pos_, h : RepeatedNull[_, 1]] := With[{
	eval = Extract[expr, pos, h],
	multipleQ = MatchQ[pos, {__List}]
},
	ReplacePart[expr,
		If[ multipleQ,
			Thread[pos -> eval],
			pos -> eval
		]
	] /; Head[Unevaluated[eval]] =!= Extract
];

End[] (* End Private Context *)

EndPackage[]
