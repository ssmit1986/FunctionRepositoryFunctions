(* Wolfram Language Package *)

BeginPackage["FunctionRepo`sparseAssociation`", {"FunctionRepo`", "GeneralUtilities`"}]
(* Exported symbols added here with SymbolName::usage *)
sparseAssociation::usage = "";

Begin["`Private`"] (* Begin Private Context *)

sparseAssociation::part = "Part `1` of sparseAssociation doesn't exist.";
sparseAssociation::badData = "Illegal sparseAssociation encountered.";

constructedDataQ = Function[
    And[
        AssociationQ[#],
        MatchQ[#,
            KeyValuePattern[{
                "Array" -> _SparseArray | {},
                "Keys" -> {___?AssociationQ},
                "ConstructedQ" -> True
            }]
        ]
    ]
];

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
sparseAssociation[{}, ___] := sparseAssociation[
    <|
        "Array" -> {},
        "Keys" -> {},
        "ConstructedQ" -> True
    |>
];

sparseAssociation[array_?ArrayQ, keys : {keySpec..}, rest___] :=
    sparseAssociation[array, ConstantArray[keys, ArrayDepth[array]], rest];

sparseAssociation[
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
        
        sparseAssociation[assoc] /; verifyDataStructure[assoc]
        ,
        Length[keys] === Length[dims]
    ]
];

(* accessors *)
Normal[sparseAssociation[data_?constructedDataQ]] ^:= data["Array"];
Keys[sparseAssociation[data_?constructedDataQ]] ^:= Keys @ data["Keys"];

Scan[
    Function[
        #[sparseAssociation[data_?constructedDataQ]] ^:= # @ data["Array"]
    ],
    {Length, Dimensions, ArrayDepth}
];

ArrayRules[sparseAssociation[data_?constructedDataQ]] ^:= With[{
    keysIndices = AssociationInvert /@ data["Keys"]
},
    Replace[
        ArrayRules[data["Array"]],
        Verbatim[Rule][ind : {__Integer}, el_] :> MapThread[Lookup, {keysIndices, ind}] -> el,
        {1}
    ]
];

accesskeySpec = keySpec | {keySpec..} | _Integer | {__Integer} | All;

Part[sparseAssociation[data_?constructedDataQ], keys : (accesskeySpec..)] /; Length[{keys}] <= Length[data["Keys"]] ^:= Module[{
    dataKeys = TakeDrop[data["Keys"], UpTo[Length[{keys}]]],
    positions
},
    positions = MapThread[
        Replace[#2, s : _String | {__String} :> Lookup[#1, s]]&,
        {dataKeys[[1]], {keys}}
    ];
    Replace[
        data[["Array", Sequence @@ positions]],
        arr_?ArrayQ :> sparseAssociation[
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
    ] /; MatchQ[positions, {({__Integer} | _Integer | All) ..}]
];

Part[sparseAssociation[_?constructedDataQ], other__] ^:= (Message[sparseAssociation::part, {other}]; $Failed);
Part[sparseAssociation[Except[_?constructedDataQ], ___], __] ^:= (Message[sparseAssociation::badData]; $Failed);

sparseAssociation[data_?constructedDataQ][keys : (keySpec..)] := Part[sparseAssociation[data], keys];

sparseAssociation /: MakeBoxes[sparseAssociation[assoc_?constructedDataQ], form_] /; Head[assoc["Array"]] === SparseArray := Module[{
    sparseArrayArg = Block[{BoxForm`ArrangeSummaryBox = Throw[{##}, "boxes"]&},
        Catch[ToBoxes[assoc["Array"], form], "boxes"]
    ],
    sparseItems
},
    sparseItems = Join @@ sparseArrayArg[[{4, 5}]];
    BoxForm`ArrangeSummaryBox[
        "sparseAssociation",
        sparseAssociation[assoc],
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

End[] (* End Private Context *)

EndPackage[]