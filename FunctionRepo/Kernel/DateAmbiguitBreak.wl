(* Wolfram Language Package *)

BeginPackage["FunctionRepo`DateAmbiguitBreak`", {"FunctionRepo`"}]
(* Exported symbols added here with SymbolName::usage *)
GeneralUtilities`SetUsage[DateAmbiguitBreak,
	"DateAmbiguitBreak[\"DayFirst\"][string$] interprets string$ as a date or date-time. In case of ambiguity, a day-before-month interpretation is used.
DateAmbiguitBreak[\"MonthFirst\"][string$] interprets string$ as a date or date-time. In case of ambiguity, a month-before-day interpretation is used.
DateAmbiguitBreak[patt$][string$] uses the first interpretation where the \"Value\" key in the returned AmbiguityList object matches patt$.
DateAmbiguitBreak[Function[{dates$, values$}, $$]][string$] applies a function all DateObjects and corresponding \"Value\" elements of the AmbiguityList to pick the correct date."
];

Begin["`Private`"] (* Begin Private Context *)

DateAmbiguitBreak[patt_] := DateAmbiguitBreak[Interpreter["Date" | "DateTime"], patt];

DateAmbiguitBreak[Interpreter[args : Shortest[__], opts : OptionsPattern[]], patt_][input_] := Block[{
	dummyWrapper
},
	Replace[
		Interpreter[
			args,
			AmbiguityFunction -> dummyWrapper, (* For some reason, AmbiguityFunction -> All doesn't always work. *)
			opts
		] @ input,
		dummyWrapper[res___] :> ambiguityBreaker[patt][res],
		{0, 1}
	]
];

DateAmbiguitBreak[__][_] := $Failed;

isoPattern = {"Year", "Month", "Day"};
dayFirstPattern = {"Day", "Month", "Year"};
monthFirstPattern = {"Month", "Day", "Year"};

ambiguityBreaker["DayFirst"] := ambiguityBreaker[isoPattern | dayFirstPattern];
ambiguityBreaker["MonthFirst"] := ambiguityBreaker[isoPattern | monthFirstPattern];

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
		KeyValuePattern[{"AmbiguityType" -> DateFormat, "Value" -> patt}],
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

End[] (* End Private Context *)

EndPackage[]
