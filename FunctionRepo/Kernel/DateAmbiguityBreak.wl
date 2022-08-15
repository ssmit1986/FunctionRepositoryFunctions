(* Wolfram Language Package *)

BeginPackage["FunctionRepo`DateAmbiguityBreak`", {"FunctionRepo`"}]
(* Exported symbols added here with SymbolName::usage *)
GeneralUtilities`SetUsage[DateAmbiguityBreak,
	"DateAmbiguityBreak[\"DayFirst\"][string$] interprets string$ as a date or date-time. In case of ambiguity, a day-before-month interpretation is used.
DateAmbiguityBreak[\"MonthFirst\"][string$] interprets string$ as a date or date-time. In case of ambiguity, a month-before-day interpretation is used.
DateAmbiguityBreak[patt$][string$] uses the first interpretation where the \"Value\" key in the returned AmbiguityList object matches patt$.
DateAmbiguityBreak[Function[dateAssoc$, $$]][string$] applies a function to an association holding all DateObjects and corresponding ambiguity data in the AmbiguityList to pick the correct date."
];

Begin["`Private`"] (* Begin Private Context *)

DateAmbiguityBreak[patt_] := DateAmbiguityBreak[Interpreter["Date" | "DateTime"], patt];

DateAmbiguityBreak[Interpreter[args : Shortest[__], opts : OptionsPattern[]], patt_][input_] := Block[{
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

DateAmbiguityBreak[__][_] := $Failed;

isoPattern = {"Year", "Month", "Day"};
dayFirstPattern = {"Day", "Month", "Year"};
monthFirstPattern = {"Month", "Day", "Year"};

ambiguityBreaker["DayFirst"] := ambiguityBreaker[isoPattern | dayFirstPattern];
ambiguityBreaker["MonthFirst"] := ambiguityBreaker[isoPattern | monthFirstPattern];

ambiguityBreaker[fun_Function][AmbiguityList[dates_List, _, assocs : {__?AssociationQ}]] := With[{
	dateAssoc = AssociationThread[
		dates,
		assocs
	]
},
	fun @ dateAssoc
];

$elementRanking = <|"Day" -> 1, "Month" -> 2, "Year" -> 3|>;
rankValue[l : {_, _, _}] := Lookup[$elementRanking, l, 10];
rankValue[other_] := {10, 10, 10}; 

ambiguityBreaker[patt_][AmbiguityList[dates_List, _, assocs : {__?AssociationQ}]] := With[{
	pos = FirstPosition[
		SortBy[ (* This makes sure that dd-mm-yy and mm-dd-yy win out against yy-mm-dd*)
			AssociationThread[dates, assocs],
			rankValue[#Value]&
		],
		KeyValuePattern[{"AmbiguityType" -> DateFormat, "Value" -> patt}],
		$Failed,
		{1},
		Heads -> False
	]
},
	Replace[pos,
		{
			{Key[d_DateObject]} :> d,
			_ :> $Failed
		}
	]
];

ambiguityBreaker[_][___] := $Failed;

End[] (* End Private Context *)

EndPackage[]
