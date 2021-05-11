(* Wolfram Language Package *)

BeginPackage["FunctionRepo`AssociationTemplate`", {"FunctionRepo`", "GeneralUtilities`"}]
(* Exported symbols added here with SymbolName::usage *)
GeneralUtilities`SetUsage[AssociationTemplate, "AssociationTemplate[assoc$] creates an association in which \
TemplateObjects can be specified with TemplateSlots that query the Association itself."];

Begin["`Private`"] (* Begin Private Context *)

acyclicDependencyQ[refAssoc_?AssociationQ] := With[{
    graph = Graph[
        Flatten @ KeyValueMap[
            Thread[DirectedEdge[##]]&,
            refAssoc
        ]
    ]
},
    TrueQ @ And[
        LoopFreeGraphQ[graph],
        AcyclicGraphQ[graph]
    ]
];
acyclicDependencyQ[_] := False;

packIfSmaller[list_] := With[{
    packedList = Developer`ToPackedArray[list]
},
    If[ ByteCount[packedList] < ByteCount[list],
        packedList,
        list
    ]
];

AssociationTemplate[rules : {__Rule}] := AssociationTemplate[Association @ rules];

AssociationTemplate[] := AssociationTemplate[<||>];
AssociationTemplate[<||>] := AssociationTemplate[<||>, <||>, <||>, {}];

AssociationTemplate[assoc_?AssociationQ] /; AllTrue[Keys[assoc], StringQ] := Module[{
    splitAssoc = Lookup[
        GroupBy[
            assoc,
            Function[expr, 
                MatchQ[
                    Unevaluated[expr],
                    _TemplateExpression| _TemplateObject
                ],
                HoldFirst
            ]
        ],
        {False, True},
        <||>
    ],
    refs,
    posIndex = First /@ PositionIndex[Keys[assoc]]
},
    refs = Association @ KeyValueMap[
        Function[{key, val},
            key -> DeleteDuplicates[ (* Find all dependent template slots that need to be computed to calculate this one *)
                Cases[val, 
                    TemplateSlot[slot_String] :> slot, 
                    Infinity,
                    Heads -> True
                ]
            ]
        ],
        splitAssoc[[2]]
    ];
    If[ acyclicDependencyQ[refs]
        ,
        AssociationTemplate[
            Sequence @@ splitAssoc,
            packIfSmaller /@ Map[posIndex, refs, {2}],
            Keys[assoc] (* Store the original keys to be able to re-assemble the Association in the correct order *)
        ]
        ,
        Failure["ConstructionFailure",
            <|
                "MessageTemplate" -> "Cyclic dependency of keys detected."
            |>
        ]
    ]
];

AssociationTemplate[assoc_?AssociationQ] := Failure["ConstructionFailure",
    <|
        "MessageTemplate" -> "Key(s) `Keys` are not Strings.",
        "MessageParameters" -> <|
            "Keys" -> Select[Keys[assoc], !StringQ[#]&]
        |>
    |>
];

AssociationTemplate[_] := Failure["ConstructionFailure",
    <|"MessageTemplate" -> "Can only construct AssociationTemplate from an Association with String keys."|>
];

cachedQuery[sAssoc_, key_String, extraVals_] /; KeyExistsQ[extraVals, key] := (
    cachedQuery[sAssoc, key, extraVals] = extraVals[key]
);

cachedQuery[
    sAssoc : AssociationTemplate[data_, _, _, _],
    key_String,
    rest___
] /; KeyExistsQ[data, key] := (cachedQuery[sAssoc, key, rest] = data[key]);

cachedQuery[
    sAssoc : AssociationTemplate[data_, exprs_, refs_, keyList_],
    key_String,
    rest___
] /; KeyExistsQ[exprs, key] := (
    cachedQuery[sAssoc, key, rest] = With[{
        keys = keyList[[refs[key]]]
    },
        TemplateApply[
            exprs[key],
            AssociationThread[
                keys,
                Map[cachedQuery[sAssoc, #, rest]&, keys]
            ]
        ]
    ]
);
cachedQuery[_, key_, ___] := Missing["KeyAbsent", key];

(sAssoc : AssociationTemplate[_, _, _, _])[key_, rest : Repeated[_?AssociationQ, {0, 1}]] := Internal`InheritedBlock[{
    cachedQuery
},
    cachedQuery[sAssoc, key, rest]
];

AssociationTemplate /: Normal[sAssoc : AssociationTemplate[data_, expr_, _, keys_List]] := KeyTake[
    Join[data, expr],
    keys
];

AssociationTemplate /: Keys[AssociationTemplate[_, _, _, keys_List]] := keys;

AssociationTemplate /: Values[
    sAssoc : AssociationTemplate[data_, expr_, _, keys_List]
] := Internal`InheritedBlock[{
    cachedQuery
},
    cachedQuery[sAssoc, #]& /@ keys
];

AssociationTemplate /: Length[AssociationTemplate[_, _, _, keys_List]] := Length[keys];

AssociationTemplate /: Part[sAssoc : AssociationTemplate[_, _, _, _], s_String] := sAssoc[s];
AssociationTemplate /: Part[
    sAssoc : AssociationTemplate[_, _, _, _],
    strings : {__String}
] := Internal`InheritedBlock[{
    cachedQuery
},
    AssociationThread[
        strings,
        cachedQuery[sAssoc, #]& /@ strings
    ]
];

AssociationTemplate /: Part[sAssoc : AssociationTemplate[_, _, _, keys_List], i : _Integer | {__Integer}] := Part[
    sAssoc,
    keys[[i]]
];

AssociationTemplate /: Part[
    sAssoc : AssociationTemplate[_, _, _, keys_List],
    All
] := AssociationThread[keys, Values[sAssoc]];

AssociationTemplate /: Part[AssociationTemplate[_, _, _, _], {}] := <||>;

AssociationTemplate /: Join[
    sAssocs : Longest[__AssociationTemplate]
] /; Length[{sAssocs}] > 1 := With[{
    list = Normal /@ {sAssocs}
},
    AssociationTemplate @ Apply[Join, list]
];

AssociationTemplate /: Join[sAssoc_AssociationTemplate, assoc : Longest[__?AssociationQ]] := 
    AssociationTemplate[Join[Normal @ sAssoc, assoc]];
AssociationTemplate /: Join[assoc : Longest[__?AssociationQ], sAssoc_AssociationTemplate] :=
    AssociationTemplate[Join[assoc, Normal @ sAssoc]];

AssociationTemplate /: Append[sAssoc_AssociationTemplate, new_] :=
    AssociationTemplate[Append[Normal @ sAssoc, new]];
AssociationTemplate /: Prepend[sAssoc_AssociationTemplate, new_] :=
    AssociationTemplate[Prepend[Normal @ sAssoc, new]];

AssociationTemplate /: MakeBoxes[
    AssociationTemplate[data_?AssociationQ, expr_?AssociationQ, refs_, keys_], 
    form_
] := BoxForm`ArrangeSummaryBox["AssociationTemplate",
    AssociationTemplate[data, expr],
    "\[LeftAssociation]\[Ellipsis]\[RightAssociation]",
    {
        BoxForm`SummaryItem[{"Number of data keys ", Length[Keys[data]]}],
        BoxForm`SummaryItem[{"Number of templated keys ", Length[Keys[expr]]}],
        BoxForm`SummaryItem[{"Total keys ", Length[keys]}]
    },
    {
        BoxForm`SummaryItem[{"Data keys ", Short[Keys[data], 3]}],
        BoxForm`SummaryItem[{"Templated keys ", Short[Keys[expr], 3]}]
    },
    form
];

End[] (* End Private Context *)

EndPackage[]