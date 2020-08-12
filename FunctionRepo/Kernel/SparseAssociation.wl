(* Wolfram Language Package *)

BeginPackage["FunctionRepo`SparseAssociation`", {"FunctionRepo`", "GeneralUtilities`"}]
(* Exported symbols added here with SymbolName::usage *)
GeneralUtilities`SetUsage[SparseAssociation, "SparseAssociation[array$, {{key$(1,1), key$(1,2), $$}, {key$(2,1), key$(2,2), $$}, $$}, default$] creates a datastructure that can be used like a SparseArray, but with string indices."];

Begin["`Private`"] (* Begin Private Context *)

SparseAssociation::part = "Part `1` of SparseAssociation doesn't exist.";
SparseAssociation::badData = "Illegal SparseAssociation encountered.";
SparseAssociation::construct = "Cannot construct SparseAssociation from the given input.";
SparseAssociation::map = "Cannot map `1` over SparseAssociation. Only dimension-preserving maps are currently allowed.";

constructedAssocPattern = _Association?(#["ValidatedQ"]&);
SparseAssociationQ = MatchQ[SparseAssociation[constructedAssocPattern]];

With[{
    cf = Compile[{
        {lst, _Integer, 1},
        {start, _Integer},
        {step, _Integer}
    },
        Module[{i = start, boole = True},
            Do[
                boole = j == i;
                If[ boole, i = i + step, Break[]],
                {j, lst}
            ];
            boole
        ]
    ]
},
    (* Test if a list is equal to Range[n] for some n *)
    integerRangeQ[{}] := False;
    integerRangeQ[list : {__Integer}] := cf[list, 1, 1];
    integerRangeQ[_] := False
];

validAssocPattern = _Association?(MatchQ[KeyValuePattern[{"Array" -> _SparseArray?ArrayQ | {}, "Keys" -> {___?AssociationQ}}]]);

verifyDataStructure[SparseAssociation[data_?AssociationQ]] := SparseAssociation[verifyDataStructure @ data]
verifyDataStructure[data : validAssocPattern] := With[{
    dims = Dimensions[data["Array"]],
    keys = data["Keys"]
},
    Append[data, "ValidatedQ" -> 
        TrueQ @ And[
            Length[keys] === Length[dims],
            AllTrue[dims, Positive],
            AllTrue[keys, integerRangeQ[Values[#]]&],
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

keyRange[l_List] := AssociationThread[l, Range[Length[l]]];

(* Parsing list of rules / rule of lists specs *)
SparseAssociation[rule : _List -> _List, rest___] := With[{rules = Thread[rule, Rule]},
    SparseAssociation[rules, rest] /; MatchQ[rules, {___Rule}]
];

(* Parse the default rule returned by ArrayRules *)
SparseAssociation[{rules___Rule, {Verbatim[_]..} -> default_}, keys_, Repeated[Automatic, {0, 1}]] := SparseAssociation[{rules}, keys, default];
SparseAssociation[{rules___Rule, {Verbatim[_]..} -> _}, rest__] := SparseAssociation[{rules}, rest];

SparseAssociation[rules : {__Rule} /; !MatchQ[rules, {(_List -> _)..}], rest___] := SparseAssociation[
    MapAt[List, rules, {All, 1}],
    rest
];

(* Finding keys automatically for array rules and association constructors *)
SparseAssociation[rules : {__Rule}, Automatic, default : _ : Automatic] := With[{
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
    ] /; MatchQ[allKeys, {{__}..}]
];

(* Normalize key argument *)
SparseAssociation[array : Except[_Rule | {__Rule}, _?ArrayQ], keys : Except[{{__}..}], rest___] :=
    SparseAssociation[array, ConstantArray[keys, ArrayDepth[array]], rest];

SparseAssociation[rules : {__Rule}, keys : Except[{{__}..}], rest___] :=
    SparseAssociation[rules, ConstantArray[keys, Length[rules[[1, 1]]]], rest]

SparseAssociation[assoc_?AssociationQ, keys : Except[{{__}..}], rest___] :=
    SparseAssociation[assoc, ConstantArray[keys, associationDepth[assoc]], rest]

SparseAssociation[{}, keys : Except[{{__}..}], rest___] := SparseAssociation[{}, {keys}, rest];

(* Constructor for list-of-rules spec *)
SparseAssociation[
    rules : {___Rule},
    keys : {{__}..},
    default : _ : Automatic
] /; AllTrue[keys, DuplicateFreeQ] := Module[{
    pos = keyRange /@ keys,
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
    keys : {{__}..},
    default : _ : Automatic
] /; AllTrue[keys, DuplicateFreeQ] := Module[{
    dims = Dimensions[array],
    assoc
},
    Condition[
        assoc = <|
            "Array" -> SparseArray[array, Automatic, default],
            "Keys" -> keyRange /@ MapThread[
                Take[#1, UpTo[#2]]&,
                {keys, dims}
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
    keys : {{__}..},
    default : _ : Automatic
] /; AllTrue[keys, DuplicateFreeQ] := Module[{
    depth = Length[keys],
    ruleSym = Rule, (* Avoid localization issues associated with Rule *)
    rules
},
    rules = Flatten @ Last @ Reap[
        MapIndexed[
            Sow[ruleSym[Replace[#2, Key[s_] :> s, {1}], #1]]&,
            DeleteMissing[assoc, depth],
            {depth}
        ];
    ];
    SparseAssociation[rules, keys, default] /; MatchQ[rules, {(_List -> _)..}]
];

(* accessors *)
SparseAssociation /: Values[SparseAssociation[data : constructedAssocPattern]] := data["Array"];
SparseAssociation /: Keys[SparseAssociation[data : constructedAssocPattern]] := Keys @ data["Keys"];

(* Converting SparseAssociation to Association *)
SparseAssociation /: Normal[
    SparseAssociation[data : constructedAssocPattern],
    Repeated["IncludeDefaultValues" -> True, {0, 1}]
] := With[{
    array = Normal @ data["Array"],
    query = Query @@ data["Keys"]
},
    query @ array
];

SparseAssociation /: Normal[
    SparseAssociation[data : constructedAssocPattern],
    "IncludeDefaultValues" -> False
] := With[{
    rules = MapAt[
        Replace[Verbatim[Rule][{Verbatim[_]..}, _] -> Nothing],
        ArrayRules[SparseAssociation[data]],
        -1
    ],
    depth = ArrayDepth[SparseAssociation[data]]
},
    GroupBy[
        rules,
        Append[
            Map[
                Function[lvl, Function[#[[1, lvl]]]],
                Range[depth - 1]
            ],
            Function[#[[1, -1]]] -> Function[#[[2]]]
        ],
        First
    ]
];

SparseAssociation /: Dataset[spAssoc : SparseAssociation[constructedAssocPattern], args___] := Dataset @ Normal[spAssoc, args];

Scan[
    Function[
        SparseAssociation /: #[SparseAssociation[data : constructedAssocPattern]] := # @ data["Array"]
    ],
    {Length, Dimensions, ArrayDepth}
];

Scan[
    Function[
        SparseAssociation /: #[SparseAssociation[data : constructedAssocPattern], rest___] := #[data["Array"], rest]
    ],
    {MatrixQ, VectorQ, ArrayQ}
];

SparseAssociation /: Map[fun_][spAssoc : SparseAssociation[constructedAssocPattern]] := Map[fun, spAssoc];

SparseAssociation /: Map[fun_, spAssoc : SparseAssociation[constructedAssocPattern], rest___] := With[{
    result = SparseAssociation[
        Map[fun, Values[spAssoc], rest],
        Keys[spAssoc]
    ]
},
    result /; SparseAssociationQ[result]
];
SparseAssociation /: Map[fun_, spAssoc : SparseAssociation[constructedAssocPattern], ___] := (
    Message[SparseAssociation::map, fun];
    $Failed
);

SparseAssociation /: ArrayRules[SparseAssociation[data : constructedAssocPattern]] := With[{
    keysIndices = GeneralUtilities`AssociationInvert /@ data["Keys"]
},
    Replace[
        ArrayRules[data["Array"]],
        Verbatim[Rule][ind : {__Integer}, el_] :> MapThread[Lookup, {keysIndices, ind}] -> el,
        {1}
    ]
];

partSpec = ({__Integer} | _Integer | All | _Span);
accesskeySpec = Flatten[_String | _Key | {(_String | _Key)..} | partSpec];

SparseAssociation /: Part[SparseAssociation[data : constructedAssocPattern], 0] := SparseAssociation;
SparseAssociation /: Part[
    SparseAssociation[data : constructedAssocPattern],
    keys : (accesskeySpec..)
] /; Length[{keys}] <= Length[data["Keys"]] := Module[{
    dataKeys = TakeDrop[data["Keys"], UpTo[Length[{keys}]]],
    positions
},
    positions = MapThread[
        Replace[#2, s : Except[partSpec] :> Lookup[#1, s]]&,
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
                            keyRange,
                            Keys @ Select[
                                MapThread[Part, {dataKeys[[1]], {keys}}],
                                AssociationQ
                            ]
                        ],
                        dataKeys[[2]]
                    ]
                |>
            ]
        ]
        ,
        MatchQ[positions, {partSpec..}]
    ]
];

SparseAssociation /: Part[SparseAssociation[constructedAssocPattern], other__] := (Message[SparseAssociation::part, {other}]; $Failed);
SparseAssociation /: Part[SparseAssociation[Except[constructedAssocPattern], ___], __] := (Message[SparseAssociation::badData]; $Failed);

SparseAssociation[data : constructedAssocPattern][keys__String] := Part[SparseAssociation[data], keys];
SparseAssociation[data : constructedAssocPattern][keys__] := Part[SparseAssociation[data], Sequence @@ Map[Key, {keys}]];

(* Typesetting *)
(* Make sure the summary boxes are loaded *)
ToBoxes[SparseArray[{0, 1}]];

cutoffList[list_List, n_Integer, ___] /; Length[list] <= n := list;
cutoffList[list_List, n_Integer, placeHolder : _ : "\[Ellipsis]"] := Append[Take[list, UpTo[n]], placeHolder];

With[{patt = constructedAssocPattern},
    SparseAssociation /: MakeBoxes[SparseAssociation[assoc : patt], form_] := Module[{
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
    ]
];

SparseAssociation[_, __] := (Message[SparseAssociation::construct]; $Failed);

End[] (* End Private Context *)

EndPackage[]