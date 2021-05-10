(* Wolfram Language Package *)

BeginPackage["FunctionRepo`SelfReferentialAssociation`", {"FunctionRepo`", "GeneralUtilities`"}]
(* Exported symbols added here with SymbolName::usage *)
GeneralUtilities`SetUsage[SelfReferentialAssociation, "SelfReferentialAssociation[assoc$] creates an association in which \
TemplateObjects can be specified with TemplateSlots that query the Association itself."];

Begin["`Private`"] (* Begin Private Context *)

SelfReferentialAssociation[assoc_?AssociationQ] /; AllTrue[Keys[assoc], StringQ] := SelfReferentialAssociation[##, Keys[assoc]]& @@ Lookup[
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

SelfReferentialAssociation[data_, _, _][key_] /; KeyExistsQ[data, key] := data[key];

(sAssoc : SelfReferentialAssociation[data_, exprs_, _])[key_] /; KeyExistsQ[exprs, key] := TemplateApply[
    exprs[key],
    Append[
        data,
        Map[
            # -> sAssoc[#]&,
            DeleteDuplicates[ (* Find all template slots that need to be computed to calculate this one *)
                Cases[exprs[key], 
                    TemplateSlot[slot_String] /; KeyExistsQ[exprs, slot] :> slot, 
                    Infinity,
                    Heads -> True
                ]
            ]
        ]
    ]
];
SelfReferentialAssociation[_, _, _][key_] := Missing["KeyAbsent", key];

SelfReferentialAssociation /: MakeBoxes[
    SelfReferentialAssociation[data_?AssociationQ, expr_?AssociationQ, keys_], 
    form_
] := BoxForm`ArrangeSummaryBox["SelfReferentialAssociation",
    SelfReferentialAssociation[data, expr],
    "\[LeftAssociation]\[RightAssociation]",
    {
        BoxForm`SummaryItem[{"Number of data keys ", Length[Keys[data]]}],
        BoxForm`SummaryItem[{"Number of Expression keys ", Length[Keys[expr]]}],
        BoxForm`SummaryItem[{"Total keys ", Length[keys]}]
    },
    {
        BoxForm`SummaryItem[{"Data keys ", Short[Keys[data], 3]}],
        BoxForm`SummaryItem[{"Expression keys ", Short[Keys[expr], 3]}]
    },
    form
];

SelfReferentialAssociation /: Keys[SelfReferentialAssociation[_, _, keys_List]] := keys;
SelfReferentialAssociation /: Values[sAssoc : SelfReferentialAssociation[data_, expr_, keys_List]] := Values[
    KeyTake[
        Append[
            data,
            Map[# -> sAssoc[#]&, Keys[expr]]
        ],
        keys
    ]
];
SelfReferentialAssociation /: Normal[sAssoc : SelfReferentialAssociation[data_, expr_, keys_List]] := Thread[
    keys -> Values[sAssoc]
];
SelfReferentialAssociation /: Length[SelfReferentialAssociation[_, _, keys_List]] := Length[keys];

SelfReferentialAssociation /: Part[sAssoc : SelfReferentialAssociation[_, _, _], s_String] := sAssoc[s];
SelfReferentialAssociation /: Part[sAssoc : SelfReferentialAssociation[_, _, _], strings : {__String}] := AssociationThread[
    strings,
    sAssoc /@ strings
];
SelfReferentialAssociation /: Part[sAssoc : SelfReferentialAssociation[_, _, keys_List], i : _Integer | {___Integer}] := Part[
    sAssoc,
    keys[[i]]
];

End[] (* End Private Context *)

EndPackage[]