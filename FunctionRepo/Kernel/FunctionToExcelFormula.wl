(* Wolfram Language Package *)

BeginPackage["FunctionRepo`FunctionToExcelFormula`", {"FunctionRepo`", "GeneralUtilities`"}]
(* Exported symbols added here with SymbolName::usage *)
GeneralUtilities`SetUsage[FunctionToExcelFormula,
	"FunctionToExcelFormula[Function[$$], {cellRef$1, cellRef$2, $$}] converts the function into a formula you can paste into Excel to be used with the\
CloudConnector for Excel.
FunctionToExcelFormula[Function[$$], Automatic] populates the Excel argument part of the Wolfram API call with dummy variables automatically."
];


Begin["`Private`"] (* Begin Private Context *) 

FunctionToExcelFormula[fun_] := FunctionToExcelFormula[fun, Automatic];
FunctionToExcelFormula[fun : HoldPattern[Function[_] | Function[Null, __]], Automatic] := With[{
	argNum = Replace[
		FunctionRepo`ArgumentCount[fun],
		Except[_Integer?Positive] -> 0
	]
},
	FunctionToExcelFormula[fun, "arg" <> ToString[#]& /@ Range[argNum]]
];
FunctionToExcelFormula[fun : HoldPattern[Function[_, __]], Automatic] := With[{
	args = List @@ Replace[
		Replace[
			Extract[fun, {1}, HoldComplete],
			{
				HoldComplete[{vars___}] :> HoldComplete[vars]
			}
		],
		s_Symbol :> With[{name = SymbolName[Unevaluated[s]]}, name /; True],
		{1}
	]
},
	FunctionToExcelFormula[fun, args]
];

FunctionToExcelFormula[fun_Symbol, Automatic] := FunctionToExcelFormula[fun, {}];

FunctionToExcelFormula[fun : (_Symbol | _Function), argList : _String | {___String}] := With[{
	sep = " , ",
	trimmedList = StringTrim[argList]
},
	stringCleanup @ StringJoin[
		functionHeaderString[fun],
		If[ MatchQ[trimmedList, "" | {""...}], "", sep],
		Replace[
			trimmedList,
			lst_List :> Riffle[lst, sep]
		],
		")"
	]
];

functionHeaderString[fun_] := StringTemplate["=Wolfram(\"`1`\""][
	StringReplace[
		StringTrim @ ToString[
			fun,
			InputForm
		],
		"\"" -> "\"\""
	]
];

stringCleanup[str_] := str;

End[] (* End Private Context *)

EndPackage[]