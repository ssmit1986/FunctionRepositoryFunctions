(* Wolfram Language Package *)

BeginPackage["FunctionRepo`BreakDateAmbiguityByPattern`", {"FunctionRepo`"}]
(* Exported symbols added here with SymbolName::usage *)
GeneralUtilities`SetUsage[BreakDateAmbiguityByPattern,
	"BreakDateAmbiguityByPattern[string$, \"DayFirst\"] interprets string$ as a date or date-time. In case of ambiguity, a day-before-month interpretation is used.
BreakDateAmbiguityByPattern[string$, \"MonthFirst\"] interprets string$ as a date or date-time. In case of ambiguity, a month-before-day interpretation is used.
BreakDateAmbiguityByPattern[string$, patt$] uses the first interpretation where the \"Value\" key in the returned AmbiguityList object matches patt$.
BreakDateAmbiguityByPattern[string$, Function[{dates$, values$}, $$]] applies a function all DateObjects and corresponding \"Value\" elements of the AmbiguityList to pick the correct date."
];

Begin["`Private`"] (* Begin Private Context *)

BreakDateAmbiguityByPattern[input_, patt_] := Block[{
	dummyWrapper,
	result
},
	Replace[
		Interpreter[
			"Date" | "DateTime",
			AmbiguityFunction -> dummyWrapper (* For some reason, AmbiguityFunction -> All doesn't always work. *)
		] @ input,
		dummyWrapper[args___] :> ambiguityBreaker[patt][args],
		{0, 1}
	]
];

ambiguityBreaker["DayFirst"] := ambiguityBreaker[{Except["Month"]..., "Day", ___}];
ambiguityBreaker["MonthFirst"] := ambiguityBreaker[{Except["Day"]..., "Month", ___}];

ambiguityBreaker[fun_Function][AmbiguityList[dates_List, _, assocs : {__?AssociationQ}]] := With[{
	result = fun[
		dates,
		Lookup[assocs, "Value"]
	]
},
	Replace[result, Except[_?DateObjectQ] :> $Failed]
]

ambiguityBreaker[patt_][AmbiguityList[dates_List, _, assocs : {__?AssociationQ}]] := With[{
	pos = FirstPosition[assocs,
		KeyValuePattern[{"Value" -> patt}],
		$Failed,
		{1},
		Heads -> False
	]
},
	Replace[pos,
		{
			{i_Integer} :> dates[[i]],
			_ :> $Failed
		}
	]
];
ambiguityBreaker[_][___] := $Failed;


BreakDateAmbiguityByPattern[___] := $Failed

End[] (* End Private Context *)

EndPackage[]
