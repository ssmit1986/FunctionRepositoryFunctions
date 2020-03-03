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
                "Array" -> _SparseArray,
                "Keys" -> {__?AssociationQ},
                "ConstructedQ" -> True
            }]
        ]
    ]
];

verifyDataStructure[data_?constructedDataQ] := With[{
    dims = Dimensions[data["Array"]],
    keys = data["Keys"]
},
    And[
        AllTrue[keys, 
            And[
                AllTrue[Keys[#], StringQ],
                AllTrue[Values[#], IntegerQ[#] && Positive[#]&]
            ]&
        ],
        Max /@ keys === dims
    ]
];
verifyDataStructure[_] := False;

keySpec = _String;
accesskeySpec = keySpec | {keySpec..} | All;

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

Part[sparseAssociation[data_?constructedDataQ], keys : (accesskeySpec..)] ^:= Module[{
    dataKeys = TakeDrop[data["Keys"], UpTo[Length[{keys}]]],
    positions
},
    positions = MapThread[
        If[ #2 === All,
            Values[#1],
            Lookup[#1, #2]
        ]&,
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
    ] /; MatchQ[positions, {({__Integer} | _Integer) ..}]
];

Part[sparseAssociation[_?constructedDataQ], other__] /; (Message[sparseAssociation::part, {other}]; False) ^:= $Failed;
Part[sparseAssociation[___], __] /; (Message[sparseAssociation::badData]; False) ^:= $Failed;

sparseAssociation[data_?constructedDataQ][keys : (keySpec..)] := Part[sparseAssociation[data], keys];

sparseAssociation[{}, ___] := <||>;

sparseAssociation[array_?ArrayQ, keys : {keySpec..}, rest___] :=
    sparseAssociation[array, ConstantArray[keys, ArrayDepth[array]], rest];

sparseAssociation[
    array_?ArrayQ,
    keys : {{keySpec..}..},
    default : _ : Automatic
] /; AllTrue[keys, DuplicateFreeQ] := With[{
    assoc = <|
        "Array" -> SparseArray[array, Automatic, default],
        "Keys" -> Map[Map[First] @* PositionIndex, keys],
        "ConstructedQ" -> True
    |>
},
    sparseAssociation[assoc] /; verifyDataStructure[assoc]
];

sparseAssociation /: MakeBoxes[sparseAssociation[assoc_?constructedDataQ], form_] := With[{
    sparseArrayArg = Block[{BoxForm`ArrangeSummaryBox = Throw[{##}, "boxes"]&},
        Catch[ToBoxes[assoc["Array"], form], "boxes"]
    ]
},
    BoxForm`ArrangeSummaryBox[
        "sparseAssociation",
        sparseAssociation[assoc],
        sparseArrayArg[[3]],
        sparseArrayArg[[4]],
        Join[
            sparseArrayArg[[5, ;; 2]],
            MapIndexed[
                BoxForm`SummaryItem[{Row[{"Level ", #2[[1]], ": "}], Row[#, ","]}] &,
                Join @@ MapAt[
                    Replace[{__} :> {{"\[Ellipsis]"}}],
                    TakeDrop[Keys[assoc["Keys"]], UpTo[3]],
                    2
                ]
            ]
        ],
        form
    ]
];

End[] (* End Private Context *)

EndPackage[]