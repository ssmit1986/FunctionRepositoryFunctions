(* Wolfram Language Package *)

BeginPackage["FunctionRepo`PassOptionsDown`", {"FunctionRepo`"}]
(* Exported symbols added here with SymbolName::usage *)
GeneralUtilities`SetUsage[PassOptionsDown,
	"PassOptionsDown[f$1 -> f$2, opts$] passes options down from function 1 to function 2, taking account that the default options of f$2 might be different from those of the main function f$1."
];

Begin["`Private`"] (* Begin Private Context *)

PassOptionsDown[f_ -> g_, assoc_Association?AssociationQ] := PassOptionsDown[f -> g, Normal[assoc]];
PassOptionsDown[headFun_Symbol -> subFun_Symbol, opts : OptionsPattern[]] := With[{
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
