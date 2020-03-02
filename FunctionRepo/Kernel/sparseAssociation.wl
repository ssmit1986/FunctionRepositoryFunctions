(* Wolfram Language Package *)

BeginPackage["FunctionRepo`sparseAssociation`", {"FunctionRepo`"}]
(* Exported symbols added here with SymbolName::usage *)
sparseAssociation::usage = "";

Begin["`Private`"] (* Begin Private Context *)

Options[sparseAssociation] = {"Level" -> Automatic};

sparseAssociation[{}, ___] := <||>;

sparseAssociation[array_?ArrayQ, keys : Except[{__List}, _List], default : Except[_List | _Rule] : 0, opts : OptionsPattern[]] :=
    sparseAssociation[array, ConstantArray[keys, ArrayDepth[array]], default, opts];

sparseAssociation[
    array_?ArrayQ,
    keys : {__List},
    default : Except[_List | _Rule] : 0,
    opts : OptionsPattern[]
] := With[{
    dims = Dimensions[array],
    lvl = OptionValue["Level"]
},
    Condition[
        isparseAssociation[
            ArrayRules[array, default],
            keys
        ]
        ,
        lvl === Automatic && checkKeyDims[dims, Length /@ keys]
    ]
];

sparseAssociation[array_?ArrayQ, default : _ : 0, opts : OptionsPattern[]] := With[{
    lvl = OptionValue["Level"]
},
    isparseAssociation[ArrayRules[array, default]] /; lvl === Automatic
];

sparseAssociation[expr_, keys_List, default : _ : 0, opts : OptionsPattern[]] := Module[{
    level = OptionValue["Level"],
    assoc, keyList
},
    Condition[
        keyList = Replace[keys, l : Except[{__List}] :> ConstantArray[l, level]];
        assoc = positionAssociation[expr, Except[default], {level}];
        If[ And[
                AssociationQ[assoc],
                checkKeyDims[
                    Activate[Thread[Inactive[Max] @@ Keys[assoc]]],
                    Length /@ keyList
                ]
            ],
            isparseAssociation[
                Append[Normal[assoc], {_} -> default],
                keyList
            ],
            $Failed
        ]
        ,
        IntegerQ[level]
    ] 
];
sparseAssociation[expr_, default : _ : 0, opts : OptionsPattern[]] := Module[{
    level = OptionValue["Level"],
    assoc
},
    Condition[
        assoc = positionAssociation[expr, Except[default], {level}];
        If[ AssociationQ[assoc],
            isparseAssociation[Append[Normal[assoc], {_} -> default]],
            $Failed
        ],
        IntegerQ[level]
    ]
];

checkKeyDims[arrayDims_List, keyDims_List] := TrueQ @ And[
    Length[arrayDims] === Length[keyDims],
    And @@ Thread[keyDims >= arrayDims]
];
checkKeyDims[___] := False;

isparseAssociation[{{Verbatim[_]..} -> default_}, ___] := <|"Data" -> <||>, "Default" -> default|>;

isparseAssociation[rules_List] := Module[{
    depth = Length[rules[[1, 1]]],
    assoc
},
    Condition[
        assoc = GroupBy[
            Most @ rules,
            Map[ (* generate list of grouping rules *)
                Function[ind,
                    Function[#[[1, ind]]]
                ],
                Range[depth]
            ],
            #[[1, 2]]& (* extract the element at the given position *)
        ];
        <|
            "Data"-> assoc,
            "Default" -> rules[[-1, 2]]
        |>
        ,
        depth > 0
    ]
];

isparseAssociation[rules_, keys : {__List}] := isparseAssociation[indexRulesToKeys[rules, keys]];

indexRulesToKeys[list_, keys_] := Module[{
    rules = list
},
    rules[[;; -2, 1]] = MapIndexed[
        keys[[#2[[2]], #1]] &,
        rules[[;; -2, 1]],
        {2}
    ];
    rules
];

Options[positionAssociation] = {Heads -> False};
positionAssociation[expr_, args__, opts : OptionsPattern[]] := With[{
    pos = Position[expr, args, Heads -> OptionValue[Heads]]
},
    AssociationThread[pos, Extract[expr, pos]] /; ListQ[pos]
];

End[] (* End Private Context *)

EndPackage[]