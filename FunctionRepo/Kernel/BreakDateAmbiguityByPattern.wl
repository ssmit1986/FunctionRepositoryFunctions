(* Wolfram Language Package *)

BeginPackage["FunctionRepo`BreakDateAmbiguityByPattern`", {"FunctionRepo`"}]
(* Exported symbols added here with SymbolName::usage *)
GeneralUtilities`SetUsage[BreakDateAmbiguityByPattern,
	"BreakDateAmbiguityByPattern[string$, \"DayFirst\"] interprets string$ as a date or date-time. In case of ambiguity, a day-first interpretation is used.
BreakDateAmbiguityByPattern[string$, \"MonthFirst\"] interprets string$ as a date or date-time. In case of ambiguity, a month-first interpretation is used.
BreakDateAmbiguityByPattern[string$, patt$] uses the first interpretation where the \"Value\" key in the returned AmbiguityList object matches patt$."
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

ambiguityBreaker["DayFirst"] := ambiguityBreaker[{___, "Day", ___, "Month", ___}];
ambiguityBreaker["MonthFirst"] := ambiguityBreaker[{___, "Month", ___, "Day", ___}];
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
