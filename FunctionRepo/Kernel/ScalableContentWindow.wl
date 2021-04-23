(* Wolfram Language Package *)

BeginPackage["FunctionRepo`ScalableContentWindow`", {"FunctionRepo`", "GeneralUtilities`"}]
(* Exported symbols added here with SymbolName::usage *)
GeneralUtilities`SetUsage[ScalableContentWindow,
    "ScalableContentWindow[expr$] displays expr$ in a new window without scrollbars. The content will be made as large as the window allows.
ScalableContentWindow[expr$, align$] specifies the alignment of the content in the window."
];

Begin["`Private`"] (* Begin Private Context *) 

Options[ScalableContentWindow] = Options[Notebook];

ScalableContentWindow[content_, opts : OptionsPattern[]] :=
    ScalableContentWindow[content, {Center, Center}, opts];

ScalableContentWindow[content_, alignment_, opts : OptionsPattern[]] := CreateDocument[
    {
        Cell[
            BoxData @ ToBoxes[
                DynamicModule[{size},
                    DynamicWrapper[
                        Pane[
                            content,
                            ImageSize -> Dynamic[size],
                            Alignment -> alignment,
                            ImageSizeAction -> "ResizeToFit"
                        ],
                        size = 0.99 * CurrentValue[EvaluationNotebook[], WindowSize]
                    ],
                    Initialization :> (
                        size = 0.99 * CurrentValue[EvaluationNotebook[], WindowSize]
                    )
                ]
            ]
            ,
            "Output",
            CellMargins -> {{0, 0}, {0, 0}}
        ]
    },
    opts,
    WindowElements -> False,
    ShowCellBracket -> False,
    ShowPredictiveInterface -> False,
    "CellInsertionPointCell" -> None
];

End[] (* End Private Context *)

EndPackage[]