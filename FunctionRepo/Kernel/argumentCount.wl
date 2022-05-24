(* Wolfram Language Package *)

BeginPackage["FunctionRepo`ArgumentCount`", {"FunctionRepo`", "GeneralUtilities`"}]
(* Exported symbols added here with SymbolName::usage *)
GeneralUtilities`SetUsage[ArgumentCount,
	"ArgumentCount[Function[$$]] counts how many arguments a function takes."
];

Begin["`Private`"] (* Begin Private Context *) 

Options[ArgumentCount] = {"IgnoreSlotSequence" -> False};

ArgumentCount[HoldPattern[Function[sym : {___Symbol}, __]], opts : OptionsPattern[]] := Length[Unevaluated[sym]];

ArgumentCount[
	HoldPattern[Function[Null, body_, ___] | Function[body_]], 
	opts : OptionsPattern[]
] := With[{
	innerFunctionsDeleted = DeleteCases[
		Hold[body],
		Function[Null, __] | Function[_],
		{0, Infinity},
		Heads -> True
	]
},
	If[ Or[
			TrueQ[OptionValue["IgnoreSlotSequence"]],
			FreeQ[innerFunctionsDeleted, _SlotSequence]
		]
		,
		With[{
			slotIndices = DeleteDuplicates @ Cases[
				innerFunctionsDeleted,
				Verbatim[Slot][s : _Integer | _String] :> Replace[s, _String -> 1],
				{0, Infinity},
				Heads -> True
			]
		},
			Max[slotIndices, 0]
		]
		,
		Infinity
	]
];

ArgumentCount[cf_CompiledFunction, opts : OptionsPattern[]] := Length[cf[[2]]];

ArgumentCount[
	HoldPattern[CompiledCodeFunction[assoc_?AssociationQ, ___]], 
	opts : OptionsPattern[]
] := Replace [ 
	Lookup[
		assoc,
		"Signature"
	],
	{
		TypeSpecifier[Verbatim[Rule][in_List, _]] :> Length[in],
		_ :> $Failed
	}
];

End[] (* End Private Context *)

EndPackage[]