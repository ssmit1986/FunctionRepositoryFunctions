(* Wolfram Language Package *)

BeginPackage["FunctionRepo`tukeyMedianPolish`", {"FunctionRepo`"}]
(* Exported symbols added here with SymbolName::usage *)
tukeyMedianPolish::usage = "tukeyMedianPolish[mat] performs the Tukey median polish algorithm to find row and column effects in a data matrix.";


Begin["`Private`"] (* Begin Private Context *)

Options[tukeyMedianPolish] = {
    MaxIterations -> 100,
    "ConvergenceTest" -> Automatic,
    Tolerance -> Scaled[1.*^-4],
    "LocationEstimator" -> Median,
    Compiled -> False
};

tukeyMedianPolish::compileFail = "Failed to create a compiled function with the options given.";

tukeyMedianPolish[mat_List, outputType : ("Matrix" | "PropertyAssociation") : "Matrix", opts : OptionsPattern[]] := With[{
    try = tukeyMedianPolish[opts][mat]
},
    If[ MatchQ[try, _?validOutputQ],
        Switch[ outputType,
            "PropertyAssociation",
                propAssoc[try],
            _, try
        ],
        $Failed
    ]
];

tukeyMedianPolish[opts : OptionsPattern[]] := Module[{
    compilationOpts = Flatten[{OptionValue[Compiled]}],
    cf
},
    Condition[
        With[{
            fun = itukeyMedianPolish[Sequence @@ FilterRules[{opts}, Options[itukeyMedianPolish]]]
        },
            cf = Compile[{{m, _Real, 2}},
                fun[m],
                Evaluate[Sequence @@ Cases[compilationOpts, OptionsPattern[]]]
            ];
            If[ MatrixQ[cf[RandomReal[1, {3, 3}]], NumericQ],
                tukeyMedianPolish[opts] = cf
                ,
                Message[tukeyMedianPolish::compileFail];
                $Failed
            ]
        ],
        MatchQ[compilationOpts, {True, OptionsPattern[]}]
    ]
];

tukeyMedianPolish[opts : OptionsPattern[]][
    mat_?(MatrixQ[#, NumericQ]&)
] /; MatchQ[Dimensions[mat], {_Integer?Positive, _Integer?Positive}] :=
    itukeyMedianPolish[Sequence @@ FilterRules[{opts}, Options[itukeyMedianPolish]]] @ mat;

tukeyMedianPolish[opts : OptionsPattern[]][{} | _?MatrixQ] := {{}};

tukeyMedianPolish[opts : OptionsPattern[]][_] := $Failed;

Options[itukeyMedianPolish] = DeleteCases[
    Options[tukeyMedianPolish],
    Compiled -> _
];
itukeyMedianPolish[opts : OptionsPattern[]] := With[{
    stopQ = parseToleranceOptions[OptionValue["ConvergenceTest"], OptionValue[Tolerance]],
    maxIt = Replace[OptionValue[MaxIterations], Except[_Integer?Positive] -> 100],
    locEst = OptionValue["LocationEstimator"]
},
    Function[mat,
        Module[{
            matrix,
            dims = Dimensions[mat],
            columnMedians,
            rowMedians
        },
            If[ Length[dims] =!= 2 || dims[[1]] === 0 || dims[[2]] === 0, 
                Return[{{}}, Module]
            ];
            matrix = Table[
                If[ i <= dims[[1]] && j <= dims[[2]],
                    mat[[i, j]],
                    0
                ],
                {i, dims[[1]] + 1},
                {j, dims[[2]] + 1}
            ];
            columnMedians = locEst[matrix[[;; dims[[1]]]]];
            Do[
                Do[
                    matrix[[k]] -= columnMedians,
                    {k, dims[[1]]}
                ];
                matrix[[-1]] += columnMedians;
                
                rowMedians = locEst /@ matrix[[All, ;; dims[[2]]]];
                matrix[[All, ;; dims[[2]]]] -= rowMedians;
                matrix[[All, -1]] += rowMedians;
                
                columnMedians = locEst[matrix[[;; dims[[1]]]]];
                If[ stopQ[Join[columnMedians, rowMedians], matrix],
                    Break[]
                ],
                {i, maxIt}
            ];
            matrix
        ]
    ]
];

parseToleranceOptions[Automatic, tol_?Positive] := Function[Max @ Abs[#1] < tol];
parseToleranceOptions[Automatic, Scaled[tol_?Positive]] := Function[Max @ Abs[#1] < tol * Max @ Abs[#2]];
parseToleranceOptions[other_, _] := other;

validOutputQ = MatchQ[_List?(MatrixQ[#, NumericQ]&) | _?FailureQ];

propAssoc[{{}} | {}] := <||>;
propAssoc[mat_?MatrixQ] := With[{
    additivityPlotValues =Transpose[{
        Flatten @ Divide[
            Outer[
                Times,
                mat[[-1, ;; -2]],
                mat[[;; -2, -1]]
            ],
            mat[[-1, -1]]
        ],
        Flatten[mat[[;; -2, ;; -2]]]
    }]
},
    <|
        "Matrix" -> mat,
        "Residuals" -> mat[[;; -2, ;; -2]],
        "RowEffects" -> mat[[;; -2, -1]],
        "ColumnEffects" -> mat[[-1, ;; -2]],
        "OverallEffect" -> mat[[-1, -1]],
        "TukeyAdditivityPlot" -> ListPlot[
            additivityPlotValues,
            AxesLabel -> {"Diagnostic Comparison Values", "Residuals"},
            AxesOrigin -> {0, 0},
            PlotRange -> All
        ],
        "TukeyAdditivityPlotValues" -> additivityPlotValues
    |>
];
propAssoc[_] := $Failed;

End[] (* End Private Context *)

EndPackage[]