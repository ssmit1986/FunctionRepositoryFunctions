(* Wolfram Language Package *)

BeginPackage["FunctionRepo`ConvertStringsToSymbols`", {"FunctionRepo`", "GeneralUtilities`"}]
(* Exported symbols added here with SymbolName::usage *)
GeneralUtilities`SetUsage[ConvertStringsToSymbols, 
	"ConvertStringsToSymbols[expr$, str$] converts all instances of str$ to a symbol before evaluating expr$.
ConvertStringsToSymbols[expr$, {str$1, str$2, $$}] converts multiple strings.
ConvertStringsToSymbols[expr$, {str$1 -> context$1, $$}] creates the symbols in a specified context."
];

Begin["`Private`"] (* Begin Private Context *) 

toHeldExpression[str_String] := ToExpression[str, InputForm, HoldComplete];
toHeldExpression[str_String -> context_String] := ToExpression[context <> str, InputForm, HoldComplete];
toHeldExpression[str_ -> Automatic] := toHeldExpression[str];
toHeldExpression[_] := $Failed;

symName[str_String] := str;
symName[str_String -> _] := str;

SetAttributes[ConvertStringsToSymbols, HoldFirst];

ConvertStringsToSymbols[expr_, {}] := expr;
ConvertStringsToSymbols[expr_, spec : Except[_List]] := ConvertStringsToSymbols[expr, {spec}];

ConvertStringsToSymbols[expr_, list_List] := With[{
	specList = Flatten[Thread /@ list]
},
	Switch[ specList,
		{},
			expr
		,
		{(_String | _Rule)..},
			Replace[
				Flatten[
					HoldComplete @@ Map[
						toHeldExpression,
						specList
					]
				],
				{
					hold : HoldComplete[__Symbol] /; Length[hold] === Length[specList] :> ReplaceAll[
						Unevaluated[expr],
						List @@ Thread[
							HoldComplete @@ Map[symName, specList] :> hold,
							HoldComplete
						]
					],
					_ :> $Failed
				}
			]
		,
		_, $Failed
	]
];

End[] (* End Private Context *)

EndPackage[]