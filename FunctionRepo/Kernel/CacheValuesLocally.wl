(* Wolfram Language Package *)

BeginPackage["FunctionRepo`CacheValuesLocally`", {"FunctionRepo`", "GeneralUtilities`"}]
(* Exported symbols added here with SymbolName::usage *)
GeneralUtilities`SetUsage[CacheValuesLocally, 
    "CacheValuesLocally[{f$1, f$2, $$}, expr$] evaluates expr$ while automatically memoizing function values of functions f$i without permanently storing the values."
];

Begin["`Private`"] (* Begin Private Context *) 

SetAttributes[CacheValuesLocally, HoldAll];

CacheValuesLocally[functions : {__Symbol}, expr_] := Internal`InheritedBlock[functions,
    Scan[
        Function[{fun},
            Module[{outsideQ = True},
                fun[args___] /; outsideQ := Set[
                    fun[args],
                    Block[{outsideQ = False}, fun[args]]
                ]
            ];
            DownValues[fun] = RotateRight[DownValues[fun]],
            HoldFirst
        ],
        Unevaluated[functions]
    ];
    expr
];

End[] (* End Private Context *)

EndPackage[]