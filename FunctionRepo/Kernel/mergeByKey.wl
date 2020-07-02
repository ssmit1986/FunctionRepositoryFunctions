(* Wolfram Language Package *)

BeginPackage["FunctionRepo`mergeByKey`", {"FunctionRepo`", "GeneralUtilities`"}]
(* Exported symbols added here with SymbolName::usage *)
GeneralUtilities`SetUsage[mergeByKey, "mergeByKey[{assoc$1, assoc$2, $$}, {key$1 -> fun$1, key$2 -> fun$2, $$}, default$] merges the assocations using different merge functions for different keys."];

Begin["`Private`"] (* Begin Private Context *) 

mergeByKey[data : {__?AssociationQ}, rules : {___Rule}, default : _ : Identity] := Module[{
    assocs = With[{
        missingToken = Missing["mergeByKey`Private`KeyUnion"]
    },
        DeleteCases[
            AssociationTranspose[
                KeyUnion[data, missingToken&]
            ],
            missingToken,
            {2}
        ]
    ],
    keys
},
    keys = Keys[assocs];
    Query[
        DeleteCases[
            Thread[Key /@ keys -> Lookup[rules, keys, default]],
            _ -> Identity
        ]
    ] @ assocs
];

End[] (* End Private Context *)

EndPackage[]