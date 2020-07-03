(* Wolfram Language Package *)

BeginPackage["FunctionRepo`mergeByKey`", {"FunctionRepo`", "GeneralUtilities`"}]
(* Exported symbols added here with SymbolName::usage *)
GeneralUtilities`SetUsage[mergeByKey, "mergeByKey[{assoc$1, assoc$2, $$}, {key$1 -> fun$1, key$2 -> fun$2, $$}, default$] merges the assocations using different merge functions for different keys."];

Begin["`Private`"] (* Begin Private Context *) 

mergeByKey[data : {__?AssociationQ}, rules : {___Rule}, default : _ : Identity] := Module[{
    assoc = With[{
        (* random UUID for identifying where the undefined keys were after using AssociationTranspose *)
        missingToken = Missing["mergebykey-0bde4aea-38fd-4a9f-bb4a-d09b00f7d52b"]
    },
        DeleteCases[
            GeneralUtilities`AssociationTranspose[
                KeyUnion[data, missingToken&]
            ],
            missingToken,
            {2}
        ]
    ],
    keys
},
    keys = Keys[assoc];
    Query[
        DeleteCases[
            Thread[Key /@ keys -> Lookup[rules, keys, default]],
            _ -> Identity
        ]
    ] @ assoc
];

End[] (* End Private Context *)

EndPackage[]