(* Wolfram Language Package *)

BeginPackage["FunctionRepo`DefinitionString`", {"FunctionRepo`"}]
(* Exported symbols added here with SymbolName::usage *)
GeneralUtilities`SetUsage[DefinitionString,
	"DefinitionString[sym$] returns the definition of symbol sym$ as a string with context aliases for readability. Removes ReadProtected attribute and shows internal implementation details."
];

Begin["`Private`"] (* Begin Private Context *)

(* Main function for extracting and formatting symbol definitions *)
SetAttributes[DefinitionString, HoldFirst];
DefinitionString[sym_Symbol] := Block[
	{
		contextPath, symContext,
		$ContextPath, $Context, $ContextAliases, contexts, str, aliases,
		defs
	},
	Internal`InheritedBlock[{sym},
		sym;
		symContext = Context[sym];
		Needs["CodeFormatter`" -> None];
		ClearAttributes[sym, ReadProtected];
		contextPath = DeleteDuplicates @ {"System`"};
		contexts = ReverseSortBy[StringLength] @ DeleteCases[
			DeleteDuplicates @ Cases[
				GeneralUtilities`Definitions[sym],
				s_Symbol :> Context[s],
				{0, Infinity},
				Heads -> True
			],
			Alternatives @@ contextPath
		];
		$Context = SelectFirst[
			contexts,
			StringContainsQ["`private`" | "dump`", IgnoreCase -> True],
			symContext
		];
		$ContextAliases = <||>;
		$ContextPath = contextPath;
		str = ToString[Definition[sym], InputForm];
		defs = GeneralUtilities`Definitions[sym];
		
		aliases = MapIndexed[
			#1 -> "c" <> ToString[First[#2]] <> "`"&,
			DeleteDuplicates @ DeleteCases[
				Prepend[symContext] @ SortBy[contexts, StringLength],
				Alternatives @@ Append[$ContextPath, $Context]
			]
		];
		str //= StringReplace[aliases] /* StringTrim /* CodeFormatter`CodeFormat;
		StringJoin[
			"ContextPath: ", StringRiffle[contextPath, ", "],
			"\nContext: ", $Context,
			"\n\nAliases:\n", If[aliases === {}, "None", StringRiffle[aliases, "\n"]],
			"\n\nDefinition:\n", str,
			If[ FreeQ[defs, GeneralUtilities`PackageScope`$KernelFunctionPlaceholder],
				"",
				"\n\n<<Hidden kernel definitions>>"
			]
		]
	]
];

End[] (* End Private Context *)

EndPackage[]
