(* Wolfram Language Package *)

BeginPackage["FunctionRepo`expressionToFunction`", {"FunctionRepo`", "GeneralUtilities`"}]
(* Exported symbols added here with SymbolName::usage *)
GeneralUtilities`SetUsage[ExpressionToFunction, "ExpressionToFunction[expr$, var$1, var$2, $$] returns a function that takes var$i as arguments"];

Begin["`Private`"] (* Begin Private Context *) 

Options[ExpressionToFunction] = {Attributes -> {}};
Attributes[ExpressionToFunction] = {HoldAll};

ExpressionToFunction[expr_, vars : (_Symbol | {__Symbol}) .., opts : OptionsPattern[]] := Apply[
    Function[
        Null,
        Block[{##},
            With[{
                symbols = Replace[
                    {vars},
                    lst_List :> Unique["vecVar"],
                    {1}
                ],
                attributes = OptionValue[Attributes]
            },
                With[{
                    rules = Flatten @ Map[
                        Function[
                            Replace[#1,
                                {
                                    Verbatim[Rule][
                                        simsIn : {__Symbol},
                                        symOut_Symbol
                                    ] :> Thread[simsIn -> Array[Indexed[symOut, #] &, Length[simsIn]]],
                                    _ -> Nothing
                                }
                            ]
                        ],
                        Thread[{vars} -> symbols]
                    ]
                },
                    ReleaseHold @ Function[
                        Evaluate[symbols],
                            Evaluate[Hold[expr] /. rules],
                            attributes
                    ]
                ]
            ]
        ],
        {HoldAll}
    ],
    Flatten[Hold @@ Cases[Hold[vars], s_Symbol :> Hold[s], {1, 2}]]
];

ExpressionToFunction[
    expr_,
    vars : Longest[((_Symbol | {__Symbol}) -> (_Integer?Positive | _String)) ..],
    opts : OptionsPattern[]
] := Apply[
    Function[
        Null,
        Block[{##},
            With[{
                attributes = OptionValue[Attributes],
                rules = Flatten @ Map[
                        Function[
                            Replace[#1,
                                {
                                    Verbatim[Rule][
                                        simIn_Symbol,
                                        indexOut_
                                    ] :> (simIn -> Inactive[Slot][indexOut]),
                                    Verbatim[Rule][
                                        simsIn : {__Symbol},
                                        indexOut_
                                    ] :> Thread[simsIn -> 
                                            Array[
                                                Indexed[Inactive[Slot][indexOut], #]&,
                                                Length[simsIn]
                                            ]
                                        ]
                                }
                            ]
                        ],
                        {vars}
                    ]
            },
                Activate[
                    ReleaseHold @ Function[
                        Null,
                        Evaluate[Hold[expr] /. rules],
                        attributes
                    ],
                    Slot
                ]
            ]
        ],
        {HoldAll}
    ],
    Flatten[Hold @@ Cases[Hold[vars], s_Symbol :> Hold[s], {2, 3}]]
];

End[] (* End Private Context *)

EndPackage[]