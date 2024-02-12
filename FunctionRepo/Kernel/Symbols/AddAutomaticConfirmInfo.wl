(* Wolfram Language Package *)

BeginPackage["FunctionRepo`AddAutomaticConfirmInfo`", {"FunctionRepo`"}]
(* Exported symbols added here with SymbolName::usage *)
GeneralUtilities`SetUsage[AddAutomaticConfirmInfo,
	"AddAutomaticConfirmInfo[sym$] modifies the definitions of sym$ and adds source code information to all of the Confirm-type statements inside."
];

Begin["`Private`"] (* Begin Private Context *)

SetAttributes[stringifyCode, HoldAllComplete];
stringifyCode[expr_, name_] := Block[{
	$ContextPath = Union @ Cases[HoldComplete[expr], s_Symbol :> Context[s], {0, Infinity}, Heads -> True]
},
	OpenerView[
		{
			"Error in function: " <> StringDelete[name, StartOfString ~~ Longest[___] ~~ "`"],
			(* Create an InputForm string of the code, but without any of the explicit contexts to make the code more readable *)
			ClickToCopy @ <|
				"FullSymbolName" -> name,
				"Code" -> ToString[Unevaluated[expr], InputForm]
			|>
		},
		Method -> "Active"
	]
];


SetAttributes[fullSymbolName, HoldAllComplete];
fullSymbolName[sym_Symbol] := Context[Unevaluated @ sym] <> SymbolName[Unevaluated @ sym];
fullSymbolName[str_String] := Context[str] <> str;

SetAttributes[AddAutomaticConfirmInfo, HoldFirst];
AddAutomaticConfirmInfo[sym_Symbol] := With[{
	symbolName = fullSymbolName[sym]
},
	AddAutomaticConfirmInfo[symbolName]
];

AddAutomaticConfirmInfo[str_String?NameQ /; StringFreeQ[str, "`"]] := AddAutomaticConfirmInfo[
	Evaluate[fullSymbolName[str]]
];

AddAutomaticConfirmInfo[name_String?NameQ] := With[{
	def = Language`ExtendedDefinition[name]
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
						info = stringifyCode[expr, name]
					},
						head[expr, info] /; True
					],
					HoldPattern @ (head : ConfirmQuiet)[expr_] :> With[{
						info = stringifyCode[expr, name]
					},
						head[expr, All, info] /; True
					],
					HoldPattern @ (head : ConfirmBy | ConfirmMatch | ConfirmQuiet)[expr_, crit_] :> With[{
						info = stringifyCode[expr, name]
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
