(* Wolfram Language Package *)

BeginPackage["FunctionRepo`MapAtColumns`", {"FunctionRepo`"}]
(* Exported symbols added here with SymbolName::usage *)
GeneralUtilities`SetUsage[MapAtColumns,
	"MapAtColumns[col$ -> fun$, tab$] applies a function to column values."
];

Begin["`Private`"] (* Begin Private Context *)

MapAtColumns[rules_][tab_] := MapAtColumns[rules, tab];
MapAtColumns[rule_Rule, tab_] := MapAtColumns[{rule}, tab];
MapAtColumns[{}, tab_] := tab;
MapAtColumns[rules : {__Rule}, tab_] := Module[{
	keys = ColumnKeys[tab],
	newRules
},
	Condition[
		newRules = MapApply[
			Function[{key, fun},
				key -> rewriteFunction[fun, key]
			],
			rules
		];
		TransformColumns[tab, newRules]
		,
		TrueQ[ListQ[keys] && ContainsOnly[Keys[rules], keys]]
	]
];
MapAtColumns[f_, tab_] := With[{
	keys = Quiet @ ColumnKeys[tab]
},
	MapAtColumns[Thread[keys -> f], tab] /; ListQ[keys]
];
MapAtColumns[_, _] := Failure["MapAtColumns", <|"MessageParameters" -> "InvalidArguments"|>];


rewriteFunction[fun : HoldPattern[Function[_] | Function[Null, __]], key_] := Activate[
	ReplaceAll[
		fun,
		{
			innerFun : Except[fun, HoldPattern[Function[_] | Function[Null, __]]] :> innerFun,
			Verbatim[Slot][1] :> Inactive[Slot][1][key]
		}
	],
	Slot
];
rewriteFunction[fun : HoldPattern[Function[sym_Symbol, __]], key_] := Replace[
	ReplaceAll[
		Rest[HoldComplete @@ fun],
		{
			innerFun : HoldPattern[Function[sym | {___, sym, ___}, __]] :> innerFun,
			HoldPattern[sym] :> sym[key]
		}
	],
	HoldPattern[HoldComplete[rest__]] :> Function @@ Hold[sym, rest]
];
rewriteFunction[HoldPattern[Function[{sym_Symbol, ___}, body__]], key_] := rewriteFunction[Function[sym, body], key];

rewriteFunction[fun_, key_] := fun[#[key]]&;


End[] (* End Private Context *)

EndPackage[]
