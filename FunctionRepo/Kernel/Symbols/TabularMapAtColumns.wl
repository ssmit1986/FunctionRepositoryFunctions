(* Wolfram Language Package *)

BeginPackage["FunctionRepo`TabularMapAtColumns`", {"FunctionRepo`"}]
(* Exported symbols added here with SymbolName::usage *)
GeneralUtilities`SetUsage[TabularMapAtColumns,
	"TabularMapAtColumns[tab$, col$ -> fun$] applies a function to column values."
];

Begin["`Private`"] (* Begin Private Context *)

TabularMapAtColumns[rules_][tab_] := TabularMapAtColumns[tab, rules];
TabularMapAtColumns[tab_, rule_Rule] := TabularMapAtColumns[tab, {rule}];
TabularMapAtColumns[tab_, {}] := tab;
TabularMapAtColumns[tab_Tabular?TabularQ, rules : {__Rule}] /; Quiet @ TrueQ @ ContainsOnly[Keys[rules], ColumnKeys[tab]] := Module[{
	newRules = MapApply[
		Function[{key, fun},
			key -> Function[fun[#[key]]]
		],
		rules
	]
},
	TransformColumns[tab, newRules]
];
TabularMapAtColumns[_, _] := Failure["TabularMapAtColumns", <||>];

End[] (* End Private Context *)

EndPackage[]
