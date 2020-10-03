(* Wolfram Language Package *)

BeginPackage["FunctionRepo`CacheValuesLocally`", {"FunctionRepo`", "GeneralUtilities`"}]
(* Exported symbols added here with SymbolName::usage *)
GeneralUtilities`SetUsage[CacheValuesLocally, 
    "CacheValuesLocally[{f$1, f$2, $$}, expr$] evaluates expr$ while automatically memoizing function values of functions f$i without permanently storing the values."
];

Begin["`Private`"] (* Begin Private Context *) 

SetAttributes[CacheValuesLocally, HoldAll];

CacheValuesLocally[funs : {__Symbol}, expr_] := Internal`InheritedBlock[funs,
    Module[{insideQ},
        MapIndexed[
            Function[{fun, index},
                fun[args___] /; !TrueQ[insideQ[index]] := Set[
                    fun[args],
                    Internal`InheritedBlock[{insideQ},
                        insideQ[index] = True;
                        fun[args]
                    ]
                ];
                DownValues[fun] = RotateRight[DownValues[fun]],
                HoldFirst
            ],
            Unevaluated[funs]
        ];
        expr
    ]
];


End[] (* End Private Context *)

EndPackage[]