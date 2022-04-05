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

ConvertStringsToSymbols[expr_, {}] := expr;
ConvertStringsToSymbols[expr_, spec : (_String | Rule[_String, _])] := Replace[
	toHeldExpression[spec],
	{
		HoldComplete[sym_Symbol] :> ReplaceAll[
			Unevaluated[expr],
			symName[spec] :> sym
		],
		_ :> $Failed
	}
];

ConvertStringsToSymbols[expr_, spec : (_List -> _)] := ConvertStringsToSymbols[expr, Thread[spec]];

ConvertStringsToSymbols[expr_, specList_List] := Replace[
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
];

End[] (* End Private Context *)

EndPackage[]