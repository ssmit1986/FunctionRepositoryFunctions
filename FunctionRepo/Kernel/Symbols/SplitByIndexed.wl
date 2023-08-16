(* Wolfram Language Package *)

BeginPackage["FunctionRepo`SplitAt`", {"FunctionRepo`"}]
(* Exported symbols added here with SymbolName::usage *)
GeneralUtilities`SetUsage[SplitAt,
	"SplitByIndexed[list$, f[val$, index$]$] works like SplitBy[list$, f$], but the function f$ also takes the index of the element in the list."
];

Begin["`Private`"] (* Begin Private Context *)

SplitByIndexed[l_, {}] := l;
SplitByIndexed[l_, {f_}] := SplitByIndexed[l, f];

SplitByIndexed[l_, fList_List] := With[{
	res = SplitByIndexed[l, First @ fList]
},
	If[ !MatchQ[res, _?FailureQ | _Split | _SplitByIndexed],
		Map[SplitByIndexed[#, Rest[fList]] &, res],
		$Failed
	]
];

SplitByIndexed[l_, f_] := Module[{
	i = 1,
	eval
},
	eval = If[ Length[l] > 0, f[First[l], i++], Null];
	Split[l, Function[SameQ[eval, eval = f[#2, i++]]]]
];


End[] (* End Private Context *)

EndPackage[]
