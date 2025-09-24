(* Wolfram Language Package *)

BeginPackage["FunctionRepo`SelectByColumnValues`", {"FunctionRepo`"}]
(* Exported symbols added here with SymbolName::usage *)
GeneralUtilities`SetUsage[SelectByColumnValues,
	"SelectByColumnValues[tab$, col$ -> val$] selects all rows in tab$ where column col$ has value val$.
SelectByColumnValues[tab$, col$ -> {val$1, val$2, $$}] selects rows where col$ has one of the given values val$i.
SelectByColumnValues[spec$] is an operator form of SelectByColumnValues that can be applied to expressions."
];

Begin["`Private`"] (* Begin Private Context *)


tabularPatt = _Tabular?TabularQ;


SelectByColumnValues[spec_][tab : tabularPatt] := SelectByColumnValues[tab, spec];

SelectByColumnValues[tab : tabularPatt, col_ -> val_] := If[
	TrueQ @ ColumnKeyExistsQ[tab, col]
	,
	If[ Length[tab] === 0,
		tab,
		With[{
			vals = Developer`ToList[val],
			keyColQ = TabularSchema[tab]["KeyColumns"] === {col}
		},
			If[ keyColQ && Length[vals] === 1,
				selectByRowKey[tab, vals], (* Currently this is only worthwhile for single lookups *)
				If[ Length[vals] < 100,
					selectByValue[tab, col, vals],
					selectByValue2[tab, col, vals]
				]
			]
		]
	]
	,
	If[ ColumnKeys[tab] === None,
		Failure["NoColumnHeadings", 
			<|
				"MessageTemplate" -> "SelectByColumnValues requires Tabular data with column headings."
			|>
		],
		Failure["InvalidColumn", 
			<|
				"MessageTemplate" -> "No column with name `1` found in Tabular. Use one of the columns `2` instead.",
				"MessageParameters" -> {col, ColumnKeys[tab]}
			|>
		]
	]
];

SelectByColumnValues[expr_, rest__] := Failure["NotTabular", 
	<|
		"MessageTemplate" -> "SelectByColumnValues requires Tabular data. Expression with head `1` encountered instead.",
		"MessageParameters" -> {Head[expr]}
	|>
];

selectByRowKey[tab_, {}] := tab[{}];
selectByRowKey[tab_, vals_] := Quiet[
	With[{
		try = tab[RowKey[{#}]& /@ vals]
	},
		If[ !FailureQ[try],
			try,
			If[ Length[vals] > 1,
				With[{
					try2 = DeleteCases[
						Map[tab[RowKey[{#}]]&, vals],
						_Failure
					]
				},
					If[ try2 =!= {},
						Tabular[try2, TabularSchema[tab]],
						tab[{}]
					]
				],
				tab[{}]
			]
		]
	],
	{Tabular::rkmiss}
];

selectByValue[tab_, _, {}] := tab[{}];
selectByValue[tab_, col_, vals_] := With[{
	fun = Activate[
		Function[
			Evaluate[
				Or @@ Thread[Inactive[Slot][1][col] == vals]
			]
		],
		Slot
	]
},
	Select[tab, fun]
];

selectByValue2[tab_, _, {}] := tab[{}];
selectByValue2[tab_, col_, vals_] := With[{
	pos = Position[
		Normal @ tab[All, col],
		Alternatives @@ vals,
		{1},
		Heads -> False
	][[All, 1]]
},
	tab[pos]
];


End[] (* End Private Context *)

EndPackage[]
