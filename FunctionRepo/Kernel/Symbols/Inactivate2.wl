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
SetAttributes[iInactive2, {HoldAll, SubValuesHoldAll}];

Inactivate2[expr_, rest___] := Internal`InheritedBlock[{Inactive},
	SetAttributes[Inactive, SubValuesHoldAll];
	
	Module[{newExpr},
		newExpr = ReplaceAll[Inactivate[expr, rest], Inactive -> iInactive2];
		newExpr //= wrapInactive2 /* evaluateSubValues /* unwrapInactive2;
		newExpr //= ReplaceAll[iInactive2 -> Inactive2];
		newExpr
	]
];

notSubValHoldAll[f_Symbol] := !Internal`LiterallyOccurringQ[Attributes[f], SubValuesHoldAll];
notSubValHoldAll[_] := False;

evaluateSubValues[expr_] := ReplaceRepeated[
	expr,
	iInactive2[f_?notSubValHoldAll, args_, subvals__] :> Apply[
		iInactive2[f, args, ##]&,
		{subvals}
	]
]

wrapInactive2[expr_] := ReplaceRepeated[expr,
	iInactive2[args1___][args2___] :> iInactive2[args1, {args2}]
];

unwrapInactive2[expr_] := ReplaceRepeated[
	expr,
	iInactive2[args1___, {args2___}] :> iInactive2[args1][args2]
]

$holdAttributes = {HoldFirst, HoldRest, HoldAll, HoldAllComplete};


(* Make sure that Inactive2[fun] mimics Hold attributes if fun has one of them *)
Inactive2[fun_Symbol][args___] := With[{
	expr = evaluateArgs[Hold[args], Intersection[Attributes[fun], $holdAttributes]]
},
	Apply[Inactive2[fun], expr] /; !FailureQ[expr]
];

(* 
	TODO: this takes care of 1 level of subvalues, but in expressions like Inactive[h][args1___][args2___][args3___], args3 will still 
	remain held. Not a huge deal for now. 
*)
Inactive2[fun_Symbol][args1___][args2___] /; notSubValHoldAll[fun] := With[{
	expr = evaluateArgs[Hold[args2], {}]
},
	Apply[Inactive2[fun][args1], expr] /; !FailureQ[expr]
];

evaluateArgs[expr : Hold[args___], att_] := With[{
	try = Block[{hold},
		SetAttributes[hold, att];
		Hold @@ hold[args]
	]
},
	If[ try === expr,
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
