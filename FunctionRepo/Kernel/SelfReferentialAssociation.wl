(* Wolfram Language Package *)

BeginPackage["FunctionRepo`SelfReferentialAssociation`", {"FunctionRepo`", "GeneralUtilities`"}]
(* Exported symbols added here with SymbolName::usage *)
GeneralUtilities`SetUsage[SelfReferentialAssociation, "SelfReferentialAssociation[assoc$] creates an association in which \
TemplateObjects can be specified with TemplateSlots that query the Association itself."];

Begin["`Private`"] (* Begin Private Context *)

SelfReferentialAssociation[assoc_?AssociationQ] /; AllTrue[Keys[assoc], StringQ] := SelfReferentialAssociation @@ Lookup[
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

SelfReferentialAssociation[data_, _][key_] /; KeyExistsQ[data, key] := data[key];

(sAssoc : SelfReferentialAssociation[data_, exprs_])[key_] /; KeyExistsQ[exprs, key] := TemplateApply[
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
SelfReferentialAssociation[_, _][key_] := Missing["KeyAbsent", key];

SelfReferentialAssociation /: MakeBoxes[
    SelfReferentialAssociation[data_?AssociationQ, expr_?AssociationQ], 
    form_
] := BoxForm`ArrangeSummaryBox["SelfReferentialAssociation",
    SelfReferentialAssociation[data, expr],
    "\[LeftAssociation]\[RightAssociation]",
    {
        BoxForm`SummaryItem[{"Data keys ", Row [ Keys[data], ","]}],
        BoxForm`SummaryItem[{"Expression keys ", Row[Keys[expr], ","]}]
    },
    {},
    form
];

End[] (* End Private Context *)

EndPackage[]