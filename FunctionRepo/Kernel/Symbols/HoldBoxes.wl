(* Wolfram Language Package *)

BeginPackage["FunctionRepo`HoldBoxes`", {"FunctionRepo`"}]
(* Exported symbols added here with SymbolName::usage *)
GeneralUtilities`SetUsage[HoldBoxes,
	"HoldBoxes[boxes$] is a special token that can be wrapped around boxes in the FrontEnd to keep those boxes inert and turn them into a boxes expression when entered as input."
];

Begin["`Private`"] (* Begin Private Context *)

SetAttributes[HoldBoxes, HoldAllComplete];

HoldBoxes[expr_] := Enclose[
	Confirm[
		$Failed,
		StringForm[
			"HoldBoxes did not disappear during the parsing process. Expression with head `1` encountered",
			Head @ Unevaluated @ expr
		]
	]
];

MakeExpression[
	RowBox[{"HoldBoxes", "[", boxes_, "]"}],
	_
] := HoldComplete[boxes];

End[] (* End Private Context *)

EndPackage[]
