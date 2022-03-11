(* Wolfram Language Package *)

BeginPackage["FunctionRepo`FunctionToExcelFormula`", {"FunctionRepo`", "GeneralUtilities`"}]
(* Exported symbols added here with SymbolName::usage *)
GeneralUtilities`SetUsage[FunctionToExcelFormula,
	"FunctionToExcelFormula[Function[$$]] converts the function into a formula you can paste into Excel to be used with the\
CloudConnector for Excel."
];


Begin["`Private`"] (* Begin Private Context *) 

FunctionToExcelFormula[sym_Symbol] := stringCleanup @ StringJoin[
	functionHeaderString[sym],
	")"
];

FunctionToExcelFormula[fun : HoldPattern @ Function[_]] := With[{
	argNum = Replace[
		Max @ Cases[fun, Verbatim[Slot][n_Integer] :> n, Infinity],
		Except[_Integer] :> 0
	],
	sep = " , "
},
	stringCleanup @ StringJoin[
		functionHeaderString[fun],
		Sequence @@ If[ argNum === 0,
			{},
			Prepend[sep] @ Riffle[
				"arg" <> ToString[#]& /@ Range[argNum],
				sep
			]
		],
		")"
	]
];

FunctionToExcelFormula[fun : HoldPattern @ Function[_, _]] := With[{
	args = List @@ Replace[
		Replace[
			Extract[fun, {1}, HoldComplete],
			{
				HoldComplete[{vars___}] :> HoldComplete[vars]
			}
		],
		s_Symbol :> With[{name = SymbolName[Unevaluated[s]]}, name /; True],
		{1}
	],
	sep = " , "
},
	stringCleanup @ StringJoin[
		functionHeaderString[fun],
		Sequence @@ Prepend[sep] @ Riffle[
			args,
			sep
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

stringCleanup[str_] := StringDelete[str, "\r" | "\n"];

End[] (* End Private Context *)

EndPackage[]