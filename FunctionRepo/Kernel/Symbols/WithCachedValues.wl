(* Wolfram Language Package *)

BeginPackage["FunctionRepo`WithCachedValues`", {"FunctionRepo`", "GeneralUtilities`"}]
(* Exported symbols added here with SymbolName::usage *)
GeneralUtilities`SetUsage[WithCachedValues, 
	"WithCachedValues[{f$1, f$2, $$}, expr$] evaluates expr$ while automatically memoizing function values of functions f$i without permanently storing the values."
];

Begin["`Private`"] (* Begin Private Context *) 

SetAttributes[WithCachedValues, HoldAll];

WithCachedValues[functions : {__Symbol}, expr_] := Internal`InheritedBlock[functions,
	Scan[
		Function[{fun},
			Module[{outsideQ = True},
				DownValues[fun] = Prepend[ DownValues[fun],
					HoldPattern[fun[args___] /; outsideQ] :> Block[{outsideQ = False},
						With[{value = fun[args]},
							Set[
								fun[args],
								value
							] /; value =!= Unevaluated[fun[args]]
						]
					]
				]
			],
			HoldFirst
		],
		Unevaluated[functions]
	];
	expr
];

End[] (* End Private Context *)

EndPackage[]