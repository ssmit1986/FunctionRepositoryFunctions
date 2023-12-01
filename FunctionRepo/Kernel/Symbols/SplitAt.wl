(* Wolfram Language Package *)

BeginPackage["FunctionRepo`SplitAt`", {"FunctionRepo`"}]
(* Exported symbols added here with SymbolName::usage *)
GeneralUtilities`SetUsage[SplitAt,
	"SplitAt[list$, f$] converts list$ into a list of lists such that each element for which f$ returns True starts a new sublist.
SplitAt[list$, f$, After] splits the list after each element that satisfies f$."
];

Begin["`Private`"] (* Begin Private Context *)

(* Version of SplitBy that evaluates only once for each element *)
splitBy[l_, f_] := Module[{
	eval = If[ Length[l] > 0, f @ First[l], Null]
},
	Split[l, Function[SameQ[eval, eval = f @ #2]]]
];


SplitAt[list_, f_] := SplitAt[list, f, Before];

SplitAt[list_List, f_, pos : Before | After] := With[{
	i = CreateDataStructure["Counter", 0],
	operation = Replace[pos,
		{
			Before -> "PreIncrement",
			_ -> "Increment"
		}
	]
},
	splitBy[list,
		Function[
			If[ TrueQ[f[#]],
				i[operation],
				i["Get"]
			]
		]
	]
];

End[] (* End Private Context *)

EndPackage[]
