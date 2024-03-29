(* Wolfram Language Package *)

BeginPackage["FunctionRepo`InheritOptions`", {"FunctionRepo`"}]
(* Exported symbols added here with SymbolName::usage *)
GeneralUtilities`SetUsage[InheritOptions,
	"InheritOptions[f$1 -> f$2, opts$] passes options down from function 1 to function 2, taking account that the default options of f$2 might be different from those of the main function f$1."
];

Begin["`Private`"] (* Begin Private Context *)

InheritOptions[f_ -> g_, assoc_Association?AssociationQ] := InheritOptions[f -> g, Normal[assoc]];
InheritOptions[headFun_Symbol -> subFun_Symbol, opts : OptionsPattern[]] := With[{
	headOpts = Options[headFun],
	subOpts = Options[subFun]
},
	FilterRules[
		DeleteDuplicatesBy[
			Flatten[{opts, headOpts, subOpts}],
			First
		],
		subOpts
	]
];

End[] (* End Private Context *)

EndPackage[]
