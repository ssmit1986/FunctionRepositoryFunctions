(* Wolfram Language Package *)

BeginPackage["FunctionRepo`SparseAssociation`", {"FunctionRepo`", "GeneralUtilities`"}]
(* Exported symbols added here with SymbolName::usage *)
SparseAssociation::usage = "SparseAssociation[array, {{key_11, key_12, ...}, {key_21, key_22, ...}, ...}] creates a datastructure that can be used like a SparseArray, but with string indices.";

Begin["`Private`"] (* Begin Private Context *)

SparseAssociation::part = "Part `1` of SparseAssociation doesn't exist.";
SparseAssociation::badData = "Illegal SparseAssociation encountered.";
SparseAssociation::construct = "Cannot construct SparseAssociation from the given input.";
SparseAssociation::map = "Cannot map `1` over SparseAssociation. Only dimension-preserving maps are allowed.";

constructedDataQ = Function[
    And[
        AssociationQ[#],
        MatchQ[#,
            KeyValuePattern[{
                "Array" -> _SparseArray?ArrayQ | {},
                "Keys" -> {___?AssociationQ},
                "ConstructedQ" -> True
            }]
        ]
    ]
];

verifyDataStructure[SparseAssociation[data_]] := verifyDataStructure[data];
verifyDataStructure[data_?constructedDataQ] := With[{
    dims = Dimensions[data["Array"]],
    keys = data["Keys"]
},
    TrueQ @ And[
        AllTrue[keys, 
            And[
                AllTrue[Keys[#], StringQ],
                AllTrue[Values[#], IntegerQ[#] && Positive[#]&]
            ]&
        ],
        And @@ Thread[Max /@ keys <= dims]
    ]
];
verifyDataStructure[_] := False;

keySpec = _String;

(* constructors *)
SparseAssociation[{}, ___] := SparseAssociation[
    <|
        "Array" -> {},
        "Keys" -> {},
        "ConstructedQ" -> True
    |>
];

SparseAssociation[array_?ArrayQ, keys : {keySpec..}, rest___] :=
    SparseAssociation[array, ConstantArray[keys, ArrayDepth[array]], rest];

SparseAssociation[
    array_?ArrayQ,
    keys : {{keySpec..}..},
    default : _ : Automatic
] /; AllTrue[keys, DuplicateFreeQ] := Module[{
    dims = Dimensions[array],
    assoc
},
    Condition[
        assoc = <|
            "Array" -> SparseArray[array, Automatic, default],
            "Keys" -> Map[
                AssociationThread[#, Range @ Length[#]]&,
                MapThread[
                    Take[#1, UpTo[#2]]&,
                    {keys, dims}
                ]
            ],
            "ConstructedQ" -> True
        |>;
        
        SparseAssociation[assoc] /; verifyDataStructure[assoc]
        ,
        Length[keys] === Length[dims]
    ]
];

(* accessors *)
SparseAssociation /: Normal[SparseAssociation[data_?constructedDataQ]] := data["Array"];
SparseAssociation /: Keys[SparseAssociation[data_?constructedDataQ]] := Keys @ data["Keys"];

Scan[
    Function[
        SparseAssociation /: #[SparseAssociation[data_?constructedDataQ]] := # @ data["Array"]
    ],
    {Length, Dimensions, ArrayDepth, MatrixQ, VectorQ, ArrayQ}
];

SparseAssociation /: Map[fun_, spAssoc : SparseAssociation[_?constructedDataQ], rest___] := With[{
    result = SparseAssociation[
        Map[fun, Normal[spAssoc], rest],
        Keys[spAssoc]
    ]
},
    result /; verifyDataStructure[result]
];
SparseAssociation /: Map[fun_, spAssoc : SparseAssociation[_?constructedDataQ], ___] := (
    Message[SparseAssociation::map, fun];
    $Failed
);

SparseAssociation /: ArrayRules[SparseAssociation[data_?constructedDataQ]] := With[{
    keysIndices = GeneralUtilities`AssociationInvert /@ data["Keys"]
},
    Replace[
        ArrayRules[data["Array"]],
        Verbatim[Rule][ind : {__Integer}, el_] :> MapThread[Lookup, {keysIndices, ind}] -> el,
        {1}
    ]
];

accesskeySpec = keySpec | {keySpec..} | _Integer | {__Integer} | All;

SparseAssociation /: Part[
    SparseAssociation[data_?constructedDataQ],
    keys : (accesskeySpec..)
] /; Length[{keys}] <= Length[data["Keys"]] := Module[{
    dataKeys = TakeDrop[data["Keys"], UpTo[Length[{keys}]]],
    positions,
    result, resultVerified = True
},
    positions = MapThread[
        Replace[#2, s : _String | {__String} :> Lookup[#1, s]]&,
        {dataKeys[[1]], {keys}}
    ];
    Condition[
        result = Replace[
            data[["Array", Sequence @@ positions]],
            arr_SparseArray?ArrayQ :> With[{
                spAssoc = SparseAssociation[
                    <|
                        "Array" -> arr,
                        "Keys" -> Join[
                            Map[
                                AssociationThread[Keys[#], Range @ Length[#]]&,
                                Select[AssociationQ] @ MapThread[Part, {dataKeys[[1]], {keys}}]
                            ],
                            dataKeys[[2]]
                        ],
                        "ConstructedQ" -> True
                    |>
                ]
            },
                If[ TrueQ @ verifyDataStructure[spAssoc],
                    spAssoc,
                    resultVerified = False
                ]
            ]
        ];
        result /; resultVerified
        ,
        MatchQ[positions, {({__Integer} | _Integer | All) ..}]
    ]
];

SparseAssociation /: Part[SparseAssociation[_?constructedDataQ], other__] := (Message[SparseAssociation::part, {other}]; $Failed);
SparseAssociation /: Part[SparseAssociation[Except[_?constructedDataQ], ___], __] := (Message[SparseAssociation::badData]; $Failed);

SparseAssociation[data_?constructedDataQ][keys : (keySpec..)] := Part[SparseAssociation[data], keys];

(* Make sure the summary boxes are loaded *)
ToBoxes[SparseArray[{0, 1}]];

SparseAssociation /: MakeBoxes[SparseAssociation[assoc_?constructedDataQ], form_] /; Head[assoc["Array"]] === SparseArray := Module[{
    sparseArrayArg = Block[{BoxForm`ArrangeSummaryBox = Throw[{##}, "boxes"]&},
        Catch[ToBoxes[assoc["Array"], form], "boxes"]
    ],
    sparseItems
},
    sparseItems = Join @@ sparseArrayArg[[{4, 5}]];
    BoxForm`ArrangeSummaryBox[
        "SparseAssociation",
        SparseAssociation[assoc],
        sparseArrayArg[[3]],
        Take[sparseItems, 3],
        Join[
            Part[sparseItems, {4}],
            {BoxForm`SummaryItem[{"Keys:", ""}]},
            KeyValueMap[
                BoxForm`SummaryItem[
                    If[ MissingQ[#2],
                        {"\[Ellipsis]"},
                        {Row[{"Level ", Row[#2, ","], ": "}], Row[#1, ","]}
                    ]
                ]&,
                Join @@ MapAt[
                    Replace[Except[<||>] :> <|Rest -> Missing[]|>],
                    TakeDrop[
                        PositionIndex[Keys[assoc["Keys"]]],
                        UpTo[3]
                    ],
                    2
                ]
            ]
        ],
        form
    ]
];

SparseAssociation[_, __] := (Message[SparseAssociation::construct]; $Failed);

End[] (* End Private Context *)

EndPackage[]