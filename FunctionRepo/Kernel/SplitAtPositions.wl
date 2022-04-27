(* Wolfram Language Package *)

BeginPackage["FunctionRepo`SplitAtPositions`", {"FunctionRepo`"}]
(* Exported symbols added here with SymbolName::usage *)
GeneralUtilities`SetUsage[SplitAtPositions,
	"SplitAtPositions[list$, {i$1, i$2, $$}] splits list$ into a list of lists, splitting it after each indices i$k.
SplitAtPositions[list$, {i$1, i$2, $$}, Before] splits before i$k."
];

Begin["`Private`"] (* Begin Private Context *)

SplitAtPositions[{}, {}, ___] := {};
SplitAtPositions[list_List, {}, ___] := {list};
SplitAtPositions[{}, ___] := $Failed;
SplitAtPositions[list_, indices_] := SplitAtPositions[list, indices, Before];

SplitAtPositions[
	list_List,
	indices : {__Integer},
	beforeAfter : Before | After
] /; AllTrue[MinMax[indices], Between[{1, Length[list]}]] := With[{
	spans = Span @@@ MapAt[
		If[ beforeAfter === Before,
			# - 1&,
			# + 1&
		],
		Partition[
			Flatten @ {1, Union @ indices, All},
			2,
			1
		],
		If[ beforeAfter === Before,
			{1 ;; -2 ;; 1, 2},
			{2 ;; -1 ;; 1, 1}
		]
	]
},
	DeleteCases[
		Part[list, #]& /@ spans,
		{}
	]
];

SplitAtPositions[___] := $Failed;

End[] (* End Private Context *)

EndPackage[]
