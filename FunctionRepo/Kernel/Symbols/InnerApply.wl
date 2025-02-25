BeginPackage["FunctionRepo`InnerApply`", {"FunctionRepo`", "GeneralUtilities`"}]
(* Exported symbols added here with SymbolName::usage *)
GeneralUtilities`SetUsage[InnerApply,
	"InnerApply[{f$1, $$}, {x$1, $$}, {y$1, $$}, $$] gives {f$1[x$1, y$1, $$], $$}.
InnerApply[{f$1, $$}] is an operator form that can be applied to arguments."
];


Begin["`Private`"] (* Begin Private Context *) 

InnerApply[funs_][args___List] := InnerApply[funs, args];
InnerApply[funs_List, args__List] := With[{
	list = {funs, args}
},
	MapThread[Construct, list] /; And[
		SameQ @@ Map[Length, list]
	]
];
InnerApply[(h_)[funs___], args__List] := With[{
	eval = InnerApply[{funs}, args]
},
	h @@ eval /; ListQ[eval]
];

End[] (* End Private Context *)

EndPackage[]

