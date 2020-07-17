(* Wolfram Language Package *)

BeginPackage["FunctionRepo`mergeByKey`", {"FunctionRepo`", "GeneralUtilities`"}]
(* Exported symbols added here with SymbolName::usage *)
GeneralUtilities`SetUsage[mergeByKey, "mergeByKey[{assoc$1, assoc$2, $$}, {key$1 -> fun$1, key$2 -> fun$2, $$}, default$] merges the assocations using different merge functions for different keys."];

Begin["`Private`"] (* Begin Private Context *) 

mergeByKey[rules : {___Rule}, default : _ : Identity][data : {___?AssociationQ}] := mergeByKey[data, rules, default];

mergeByKey[{}, {___Rule}, Repeated[_, {0, 1}]] := <||>;

mergeByKey[data : {__?AssociationQ}, rules : {___Rule}, default : _ : Identity] := Module[{
    (* random UUID from CreateUUID["mergebykey"] for identifying where the undefined keys were after using AssociationTranspose *)
    missingToken = Missing["mergebykey-0bde4aea-38fd-4a9f-bb4a-d09b00f7d52b"],
    assoc,
    keys,
    queryRules,
    mergeRules = Replace[
        Flatten @ Replace[
            rules,
            Verbatim[Rule][lst_List, fun_] :> Thread[lst -> fun],
            {1}
        ],
        Verbatim[Rule][Key[k_], fun_] :> k -> fun,
        {1}
    ],
    keysSameQ = SameQ @@ Keys[data]
},
    If[ keysSameQ, (* Avoid KeyUnion if it's not necessary *)
        assoc = data,
        assoc = KeyUnion[data, missingToken&]
    ];
    keys = Keys[First @ assoc];
    (* This is essentially how GeneralUtilities`AssociationTranspose works *)
    assoc = AssociationThread[keys,
        If[ keysSameQ,
            Transpose @ Values[assoc],
            DeleteCases[Transpose @ Values[assoc], missingToken, {2}]
        ]
    ];
    keys = Key /@ keys;
    queryRules = DeleteCases[
        Thread[
            keys -> Lookup[mergeRules, keys, default]
        ],
        _ -> Identity
    ];
    If[ MatchQ[queryRules, {__Rule}]
        ,
        Query[queryRules] @ assoc
        ,
        assoc
    ]
];

End[] (* End Private Context *)

EndPackage[]