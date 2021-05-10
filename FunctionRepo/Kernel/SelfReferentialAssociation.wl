(* Wolfram Language Package *)

BeginPackage["FunctionRepo`SelfReferentialAssociation`", {"FunctionRepo`", "GeneralUtilities`"}]
(* Exported symbols added here with SymbolName::usage *)
GeneralUtilities`SetUsage[SelfReferentialAssociation, "SelfReferentialAssociation[assoc$] creates an association in which \
TemplateObjects can be specified with TemplateSlots that query the Association itself."];

Begin["`Private`"] (* Begin Private Context *)

SelfReferentialAssociation[assoc_?AssociationQ] /; AllTrue[Keys[assoc], StringQ] := Apply[
    Function[
        SelfReferentialAssociation[##,
            Association @ KeyValueMap[
                Function[{key, val},
                        key -> DeleteDuplicates[ (* Find all dependent template slots that need to be computed to calculate this one *)
                        Cases[val, 
                            TemplateSlot[slot_String] /; KeyExistsQ[#2, slot] :> slot, 
                            Infinity,
                            Heads -> True
                        ]
                    ]
                ],
                #2
            ],
            Keys[assoc] (* Store the original keys to be able to re-assemble the Association in the correct order *)
        ]
    ],
    Lookup[
        GroupBy[
            assoc,
            Function[expr, 
                MatchQ[
                    Unevaluated[expr],
                    _TemplateExpression| _TemplateObject | _StringTemplate
                ],
                HoldFirst
            ]
        ],
        {False, True},
        <||>
    ]
];

SelfReferentialAssociation[assoc_?AssociationQ] := Failure["ConstructionFailure",
    <|
        "MessageTemplate" -> "Key(s) `Keys` are not Strings.",
        "MessageParameters" -> <|
            "Keys" -> Select[Keys[assoc], !StringQ[#]&]
        |>
    |>
];

SelfReferentialAssociation[_] := Failure["ConstructionFailure",
    <|"MessageTemplate" -> "Can only construct SelfReferentialAssociation from an Association with String keys."|>
];

SelfReferentialAssociation[data_, _, _, _][key_] /; KeyExistsQ[data, key] := data[key];

(sAssoc : SelfReferentialAssociation[data_, exprs_, refs_, _])[key_] /; KeyExistsQ[exprs, key] := TemplateApply[
    exprs[key],
    Append[
        data,
        Map[
            # -> sAssoc[#]&,
            refs[key]
        ]
    ]
];
SelfReferentialAssociation[_, _, _, _][key_] := Missing["KeyAbsent", key];

SelfReferentialAssociation /: MakeBoxes[
    SelfReferentialAssociation[data_?AssociationQ, expr_?AssociationQ, refs_, keys_], 
    form_
] := BoxForm`ArrangeSummaryBox["SelfReferentialAssociation",
    SelfReferentialAssociation[data, expr],
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

SelfReferentialAssociation /: Normal[sAssoc : SelfReferentialAssociation[data_, expr_, _, keys_List]] := KeyTake[
    Join[data, expr],
    keys
];

SelfReferentialAssociation /: AssociationThread[sAssoc : SelfReferentialAssociation[data_, expr_, _, keys_List]] := KeyTake[
    Append[
        data,
        Map[# -> sAssoc[#]&, Keys[expr]]
    ],
    keys
];

SelfReferentialAssociation /: Keys[SelfReferentialAssociation[_, _, _, keys_List]] := keys;

SelfReferentialAssociation /: Values[sAssoc : SelfReferentialAssociation[data_, expr_, _, keys_List]] :=
    Values @ AssociationThread[sAssoc];

SelfReferentialAssociation /: Length[SelfReferentialAssociation[_, _, _, keys_List]] := Length[keys];

SelfReferentialAssociation /: Part[sAssoc : SelfReferentialAssociation[_, _, _, _], s_String] := sAssoc[s];
SelfReferentialAssociation /: Part[sAssoc : SelfReferentialAssociation[_, _, _, _], strings : {__String}] := AssociationThread[
    strings,
    sAssoc /@ strings
];
SelfReferentialAssociation /: Part[sAssoc : SelfReferentialAssociation[_, _, _, keys_List], i : _Integer | {__Integer}] := Part[
    sAssoc,
    keys[[i]]
];
SelfReferentialAssociation /: Part[SelfReferentialAssociation[_, _, _, _], {}] := <||>;

SelfReferentialAssociation /: Join[
    sAssocs : Longest[__SelfReferentialAssociation]
] /; Length[{sAssocs}] > 1 := With[{
    list = Normal /@ {sAssocs}
},
    SelfReferentialAssociation @ Apply[Join, list]
];

SelfReferentialAssociation /: Join[sAssoc_SelfReferentialAssociation, assoc : Longest[__?AssociationQ]] := 
    SelfReferentialAssociation[Join[Normal @ sAssoc, assoc]];
SelfReferentialAssociation /: Join[assoc : Longest[__?AssociationQ], sAssoc_SelfReferentialAssociation] :=
    SelfReferentialAssociation[Join[assoc, Normal @ sAssoc]];

SelfReferentialAssociation /: Append[sAssoc_SelfReferentialAssociation, new_] :=
    SelfReferentialAssociation[Append[Normal @ sAssoc, new]];
SelfReferentialAssociation /: Prepend[sAssoc_SelfReferentialAssociation, new_] :=
    SelfReferentialAssociation[Prepend[Normal @ sAssoc, new]];

End[] (* End Private Context *)

EndPackage[]