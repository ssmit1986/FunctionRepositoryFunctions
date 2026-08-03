(* Wolfram Language Package *)

BeginPackage["FunctionRepo`Inactivate2`", {"FunctionRepo`"}]
(* Exported symbols added here with SymbolName::usage *)

FunctionRepo`Inactivate2;
FunctionRepo`Inactive2;


GU`SetUsage[Inactivate2,
	"Inactivate2[expr$, patt$] is an updated version of Inactivate that respects attributes of the symbols that get inactivated."
];

GU`SetUsage[Inactive2,
	"Inactive2[head$] is the inactivation wrapper that goes along with Inactivate2."
];

Begin["`Private`"] (* Begin Private Context *)

SetAttributes[Inactivate2, HoldFirst];
SetAttributes[Inactive2, {HoldFirst, SubValuesHoldAll}];

Inactivate2[expr_, rest___] := Internal`InheritedBlock[{Inactive},
	SetAttributes[Inactive, SubValuesHoldAll];
	Module[{
		expr2 = Inactivate[expr, rest]
	},
		expr2 //= ReplaceAll[Inactive -> Inactive2];
		expr2
	]
];

$handleAttributes = {HoldAll, HoldFirst, HoldRest, HoldAllComplete, Listable, Flat, Orderless};

SetAttributes[hold, {HoldAll, SubValuesHoldAll}];

Inactive2[fun_Symbol][args___] := With[{
	expr = evaluateArgs[hold[fun][args], Intersection[Attributes[fun], $handleAttributes]]
},
	(expr /. hold -> Inactive2) /; !FailureQ[expr]
];

evaluateArgs[_, {}] := $Failed;
evaluateArgs[expr : hold[fun_][args___], att_] := With[{
	try = Function[Null,
		hold[fun][##],
		att
	][args]
},
	If[ try === Unevaluated[expr],
		$Failed,
		try
	]
];

MakeBoxes[expr : Inactive2[f_][args___], form_] := With[{
	boxes = MakeBoxes[Inactive[f][args], form]
},
	InterpretationBox[boxes, expr]
];


End[] (* End Private Context *)

EndPackage[]
