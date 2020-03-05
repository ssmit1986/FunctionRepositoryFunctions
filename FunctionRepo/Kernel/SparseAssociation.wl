(* Wolfram Language Package *)

BeginPackage["FunctionRepo`SparseAssociation`", {"FunctionRepo`", "GeneralUtilities`"}]
(* Exported symbols added here with SymbolName::usage *)
SparseAssociation::usage = "SparseAssociation[array, {{key_11, key_12, ...}, {key_21, key_22, ...}, ...}] creates a datastructure that can be used like a SparseArray, but with string indices.";

Begin["`Private`"] (* Begin Private Context *)

SparseAssociation::part = "Part `1` of SparseAssociation doesn't exist.";
SparseAssociation::badData = "Illegal SparseAssociation encountered.";
SparseAssociation::construct = "Cannot construct SparseAssociation from the given input.";
SparseAssociation::map = "Cannot map `1` over SparseAssociation. Only dimension-preserving maps are currently allowed.";

constructedDataQ[SparseAssociation[data_?AssociationQ] | data_?AssociationQ] := MatchQ[data,
    KeyValuePattern["ValidatedQ" -> True]
];
constructedDataQ[_] := False;

validAssocPattern = _Association?(MatchQ[KeyValuePattern[{"Array" -> _SparseArray?ArrayQ | {}, "Keys" -> {___?AssociationQ}}]])

verifyDataStructure[SparseAssociation[data_?AssociationQ]] := SparseAssociation[verifyDataStructure @ data]
verifyDataStructure[data : validAssocPattern] := With[{
    dims = Dimensions[data["Array"]],
    keys = data["Keys"]
},
    Append[data, "ValidatedQ" -> 
        TrueQ @ And[
            Length[keys] === Length[dims],
            AllTrue[keys, 
                And[
                    AllTrue[Keys[#], StringQ],
                    AllTrue[Values[#], IntegerQ[#] && Positive[#]&]
                ]&
            ],
            And @@ Thread[Max /@ keys <= dims]
        ]
    ]
];
verifyDataStructure[assoc_?AssociationQ] := Append[assoc, "ValidatedQ" -> False];
verifyDataStructure[_] := $Failed;

associationDepth[assoc_?AssociationQ] := Module[{
    vals = Values[assoc],
    i = 1
},
    While[ MatchQ[vals, {(_?AssociationQ | _Missing) ..}]
        ,
        vals = Flatten @ Values[DeleteCases[vals, _Missing]];
        i++
    ];
    i
];

keySpec = _String;

(* Parsing list of rules / rule of lists specs *)
SparseAssociation[rule : _List -> _List, rest___] := With[{rules = Thread[rule, Rule]},
    SparseAssociation[rules, rest] /; MatchQ[rules, {___Rule}]
];

(* Parse the default rule returned by ArrayRules *)
SparseAssociation[{rules___Rule, {Verbatim[_]..} -> default_}] := SparseAssociation[{rules}, Automatic, default];
SparseAssociation[{rules___Rule, {Verbatim[_]..} -> default_}, keys_, Repeated[Automatic, {0, 1}]] := SparseAssociation[{rules}, keys, default];
SparseAssociation[{rules___Rule, {Verbatim[_]..} -> _}, rest___] := SparseAssociation[{rules}, rest];

SparseAssociation[rules : {(_String -> _)..}, rest___] := SparseAssociation[
    MapAt[List, rules, {All, 1}],
    rest
];

(* Finding keys automatically for array rules and association constructors *)
SparseAssociation[rules : {({__String} -> _)..}, Automatic, default : _ : Automatic] := With[{
    allKeys = rules[[All, 1]]
},
    SparseAssociation[
        rules,
        DeleteDuplicates /@ Transpose[allKeys],
        default
    ] /; MatrixQ[allKeys]
];

SparseAssociation[assoc_?AssociationQ, Automatic, default : _ : Automatic] := Module[{
    allKeys,
    elements
},
    elements = {assoc};
    allKeys = Reap[
        While[ MatchQ[elements, {(_?AssociationQ | _Missing)..}],
            elements = DeleteCases[elements, _Missing];
            Sow[DeleteDuplicates @ Flatten @ Keys[elements]];
            elements = Flatten @ Values[elements]
        ]
    ][[2, 1]];
    SparseAssociation[
        assoc,
        allKeys,
        default
    ] /; MatchQ[allKeys, {{keySpec..}..}]
];

(* Normalize key argument *)
SparseAssociation[array : Except[_Rule | {__Rule}, _?ArrayQ], keys : {keySpec..}, rest___] :=
    SparseAssociation[array, ConstantArray[keys, ArrayDepth[array]], rest];

SparseAssociation[rules : {({__String} -> _)..}, keys : {keySpec..}, rest___] :=
    SparseAssociation[rules, ConstantArray[keys, Length[rules[[1, 1]]]], rest]

SparseAssociation[assoc_?AssociationQ, keys : {keySpec..}, rest___] :=
    SparseAssociation[assoc, ConstantArray[keys, associationDepth[assoc]], rest]

SparseAssociation[{}, keys : {keySpec..}, rest___] := SparseAssociation[{}, {keys}, rest];

(* Constructor for list-of-rules spec *)
SparseAssociation[
    rules : {({__String} -> _)...},
    keys : {{keySpec..}..},
    default : _ : Automatic
] /; AllTrue[keys, DuplicateFreeQ] := Module[{
    pos = AssociationThread[#, Range[Length[#]]]& /@ keys,
    assoc,
    ruleKeys = rules[[All, 1]],
    dims
},
    dims = Dimensions[ruleKeys];
    Condition[
        assoc = <|
            "Array" -> SparseArray[
                Rule[
                    Map[
                        MapThread[Lookup, {pos, #}]&,
                        ruleKeys
                    ],
                    rules[[All, 2]]
                ],
                Length /@ keys,
                default
            ],
            "Keys" -> pos
        |>;
        
        SparseAssociation[verifyDataStructure @ assoc]
        ,
        rules === {} || (MatrixQ[ruleKeys] && dims[[2]] === Length[keys])
    ]
];

(* Constructor for array specs *)
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
            ]
        |>;
        
        SparseAssociation[verifyDataStructure @ assoc]
        ,
        Length[keys] === Length[dims]
    ]
];

(* Constructor for nested Association spec *)
SparseAssociation[
    assoc_?AssociationQ,
    keys : {{keySpec..}..},
    default : _ : Automatic
] /; AllTrue[keys, DuplicateFreeQ] := Module[{
    depth = Length[keys],
    rules
},
    rules = ReplaceAll[
        Flatten @ Last @ Reap[
            MapIndexed[
                Sow[#2 -> #1]&,
                DeleteMissing[assoc, depth],
                {depth}
            ];
        ],
        k : {Key[_String]..} :> k[[All, 1]]
    ];
    SparseAssociation[rules, keys, default] /; MatchQ[rules, {({__String} -> _)..}]
];

(* accessors *)
SparseAssociation /: Normal[SparseAssociation[data_?constructedDataQ]] := data["Array"];
SparseAssociation /: Keys[SparseAssociation[data_?constructedDataQ]] := Keys @ data["Keys"];

Scan[
    Function[
        SparseAssociation /: #[SparseAssociation[data_?constructedDataQ]] := # @ data["Array"]
    ],
    {Length, Dimensions, ArrayDepth}
];

Scan[
    Function[
        SparseAssociation /: #[SparseAssociation[data_?constructedDataQ], rest___] := #[data["Array"], rest]
    ],
    {MatrixQ, VectorQ, ArrayQ}
];

SparseAssociation /: Map[fun_][spAssoc : SparseAssociation[_?constructedDataQ]] := Map[fun, spAssoc];

SparseAssociation /: Map[fun_, spAssoc : SparseAssociation[_?constructedDataQ], rest___] := With[{
    result = SparseAssociation[
        Map[fun, Normal[spAssoc], rest],
        Keys[spAssoc]
    ]
},
    result /; constructedDataQ[result]
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
    positions
},
    positions = MapThread[
        Replace[#2, s : _String | {__String} :> Lookup[#1, s]]&,
        {dataKeys[[1]], {keys}}
    ];
    Condition[
        Replace[
            data[["Array", Sequence @@ positions]],
            arr_SparseArray?ArrayQ :> SparseAssociation[
                verifyDataStructure @ <|
                    "Array" -> arr,
                    "Keys" -> Join[
                        Map[
                            AssociationThread[Keys[#], Range @ Length[#]]&,
                            Select[AssociationQ] @ MapThread[Part, {dataKeys[[1]], {keys}}]
                        ],
                        dataKeys[[2]]
                    ]
                |>
            ]
        ]
        ,
        MatchQ[positions, {({__Integer} | _Integer | All) ..}]
    ]
];

SparseAssociation /: Part[SparseAssociation[_?constructedDataQ], other__] := (Message[SparseAssociation::part, {other}]; $Failed);
SparseAssociation /: Part[SparseAssociation[Except[_?constructedDataQ], ___], __] := (Message[SparseAssociation::badData]; $Failed);

SparseAssociation[data_?constructedDataQ][keys : (keySpec..)] := Part[SparseAssociation[data], keys];

(* Converting SparseAssociation to Association *)
SparseAssociation /: Dataset[
    SparseAssociation[data_?constructedDataQ],
    Repeated["IncludeDefaultValues" -> True, {0, 1}]
] := With[{
    array = Normal @ Normal @ data["Array"],
    query = Query @@ data["Keys"]
},
    Dataset[query @ array]
];

SparseAssociation /: Dataset[
    SparseAssociation[data_?constructedDataQ],
    "IncludeDefaultValues" -> False
] := With[{
    rules = MapAt[
        Replace[Verbatim[Rule][{Verbatim[_]..}, _] -> Nothing],
        ArrayRules[SparseAssociation[data]],
        -1
    ],
    depth = ArrayDepth[SparseAssociation[data]]
},
    Dataset @ GroupBy[
        rules,
        Append[
            Map[
                Function[lvl, Function[#[[1, lvl]]]],
                Range[depth - 1]
            ],
            Function[#[[1, -1]]] -> Function[#[[2]]]
        ],
        #[[1]]&
    ]
];

(* Typesetting *)
(* Make sure the summary boxes are loaded *)
ToBoxes[SparseArray[{0, 1}]];

cutoffList[list_List, n_Integer, ___] /; Length[list] <= n := list;
cutoffList[list_List, n_Integer, placeHolder : _ : "\[Ellipsis]"] := Append[Take[list, UpTo[n]], placeHolder];

SparseAssociation /: MakeBoxes[SparseAssociation[assoc_?constructedDataQ], form_] := Module[{
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
                        {Row[{"Level ", Row[cutoffList[#2, 4], ","], ": "}], Row[cutoffList[#1, 4], ","]}
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