(* Wolfram Language Package *)

BeginPackage["FunctionRepo`AddAutomaticConfirmInfo`", {"FunctionRepo`"}]
(* Exported symbols added here with SymbolName::usage *)
GeneralUtilities`SetUsage[AddAutomaticConfirmInfo,
	"AddAutomaticConfirmInfo[sym$] modifies the definitions of sym$ and adds source code information to all of the Confirm-type statements inside."
];

Begin["`Private`"] (* Begin Private Context *)

SetAttributes[stringifyCode, HoldAllComplete];
stringifyCode[expr_] := Block[{
	$ContextPath = Union @ Cases[HoldComplete[expr], s_Symbol :> Context[s], {0, Infinity}, Heads -> True]
},
	ToString[Unevaluated[expr], InputForm]
];


SetAttributes[fullSymbolName, HoldAllComplete];
fullSymbolName[sym_Symbol] := Context[Unevaluated @ sym] <> SymbolName[Unevaluated @ sym];
fullSymbolName[str_String] := Context[str] <> str;

defaultHandler[data_Association] := OpenerView[
	{
		"Error in function: " <> StringDelete[data["Name"], StartOfString ~~ Longest[___] ~~ "`"],
		(* Create an InputForm string of the code, but without any of the explicit contexts to make the code more readable *)
		ClickToCopy @ <|
			"FullSymbolName" -> data["Name"],
			"Code" -> data["CodeString"]
		|>
	},
	Method -> "Active"
];

SetAttributes[makeErrorData, HoldAllComplete];
makeErrorData[expr_, name_] := <|
	"Name" -> name,
	"CodeString" -> stringifyCode[expr],
	"Code" -> HoldComplete[expr]
|>;

SetAttributes[AddAutomaticConfirmInfo, HoldFirst];
AddAutomaticConfirmInfo[sym_Symbol, rest___] := With[{
	symbolName = fullSymbolName[sym]
},
	AddAutomaticConfirmInfo[symbolName, rest]
];

AddAutomaticConfirmInfo[str_String?NameQ /; StringFreeQ[str, "`"], rest___] := AddAutomaticConfirmInfo[
	Evaluate[fullSymbolName[str]],
	rest
];

AddAutomaticConfirmInfo[name_String?NameQ, handler : _ : Automatic] := With[{
	def = Language`ExtendedDefinition[name],
	hFun = Replace[handler, Automatic -> defaultHandler]
},
	If[ FreeQ[def, Confirm | ConfirmAssert | ConfirmQuiet | ConfirmBy | ConfirmMatch]
		,
		def
		,
		Set[
			Language`ExtendedDefinition[name],
			(*
				Go though the definitions of a symbol and add a code string in any Confirm[...] statements
				that do not have info tags yet.
			*)
			ReplaceRepeated[
				def,
				{
					HoldPattern @ (head : Confirm | ConfirmAssert)[expr_] :> With[{
						info = hFun @ makeErrorData[expr, name]
					},
						head[expr, info] /; True
					],
					HoldPattern @ (head : ConfirmQuiet)[expr_] :> With[{
						info = hFun @ makeErrorData[expr, name]
					},
						head[expr, All, info] /; True
					],
					HoldPattern @ (head : ConfirmBy | ConfirmMatch | ConfirmQuiet)[expr_, crit_] :> With[{
						info = hFun @ makeErrorData[expr, name]
					},
						head[expr, crit, info] /; True
					]
				}
			]
		]
	]
];
AddAutomaticConfirmInfo[args___] := Enclose[
	Confirm[
		$Failed,
		StringForm["Invalid arguments `1` for AddAutomaticConfirmInfo", Hold[args]]
	]
];

End[] (* End Private Context *)

EndPackage[]
