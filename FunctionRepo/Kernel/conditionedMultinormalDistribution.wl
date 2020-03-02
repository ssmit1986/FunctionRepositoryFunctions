(* Wolfram Language Package *)

BeginPackage["FunctionRepo`conditionedMultinormalDistribution`", {"FunctionRepo`"}]
(* Exported symbols added here with SymbolName::usage *)
conditionedMultinormalDistribution::usage = "conditionedMultinormalDistribution[dist, {i1 -> val1, ...}, {j1, j2, ...}] gives the {j1, j2, ...} marginal of dist when the indices {i1, ...} are conditioned to values {val1, ...}";

Begin["`Private`"] (* Begin Private Context *)

conditionedMultinormalDistribution::noDim = "Distribution has no dimensions left after conditioning on indices `1`";
conditionedMultinormalDistribution::dupIndex = "Duplicate indices found among the conditioning indices `1` and marginalization indices `2`";
conditionedMultinormalDistribution[dist_, {}] := dist;
conditionedMultinormalDistribution[dist_, {}, All] := dist;

conditionedMultinormalDistribution[dist_, {}, marginals_] := 
    MarginalDistribution[dist, marginals];

conditionedMultinormalDistribution[dist_, rule_Rule, rest___] :=
    conditionedMultinormalDistribution[dist, Flatten @ {Thread[rule]}, rest];

conditionedMultinormalDistribution[Inactive[MultinormalDistribution][cov_?SquareMatrixQ], rest___] := 
    conditionedMultinormalDistribution[
        Inactive[MultinormalDistribution][ConstantArray[0, Length[cov]], cov],
        rest
    ];

conditionedMultinormalDistribution[
    Alternatives[
        (head : MultinormalDistribution)[mu_, cov_]?DistributionParameterQ,
        (head : Inactive[MultinormalDistribution])[mu_ , cov_] /; With[
            {lm = Length[mu]},
            lm === Length[cov] && lm > 1
        ]
    ],
    rules : {(_Integer -> _) ..},
    marginals : (_Integer | {__Integer} | All) : All
] := With[{
    eval = conditionedMultinormalDistribution[{mu, cov}, rules, marginals]
},
    Replace[
        conditionedMultinormalDistribution[{mu, cov}, rules, marginals],
        {
            {m_?VectorQ, c_?MatrixQ} :> head[m, c],
            {m_, var_} :> NormalDistribution[m, Sqrt[var]]
        }
    ] /; ListQ[eval]
];

conditionedMultinormalDistribution[
    {mu_?VectorQ, cov_?SquareMatrixQ},
    rules : {(_Integer -> _) ..},
    marginals : (_Integer | {__Integer} | All) : All
] /; Replace[
    DuplicateFreeQ[Flatten @ {rules[[All, 1]], marginals}],
    False :> (Message[conditionedMultinormalDistribution::dupIndex, rules[[All, 1]], marginals]; False)
]:= Module[{
    dim = Length[mu],
    indexKeep, indexDrop,
    partitionedMu, partionedCov ,
    rulesNoDup, conditionValues,
    inv22, dist,
    sparseQ, symmetrizedQ,
    numericQ
},
    Condition[
        sparseQ = Head[cov] === SparseArray;
        symmetrizedQ = Head[cov] === StructuredArray;
        rulesNoDup = AssociationThread[Mod[rules[[All, 1]], dim, 1], rules[[All, 2]]];
        indexDrop = Keys[rulesNoDup];
        conditionValues = Values[rulesNoDup];
        numericQ = MatrixQ[cov, NumericQ] && VectorQ[conditionValues, NumericQ];
        indexKeep = Replace[
            marginals,
            {
                All :> Complement[Range[dim], indexDrop], 
                i_Integer :> {Mod[i, dim, 1]},
                ints_List :> Mod[ints, dim, 1]
            }
        ];
        partitionedMu = mu[[#]] & /@ {indexKeep, indexDrop};
        partionedCov = {
            {cov[[indexKeep, indexKeep]], cov[[indexKeep, indexDrop]]},
            {cov[[indexDrop, indexKeep]], cov[[indexDrop, indexDrop]]}
        };
        inv22 = Which[
            numericQ && sparseQ,
                LinearSolve[partionedCov[[2, 2]]],
            numericQ && symmetrizedQ, (* LinearSolve is better optimized for sparse numerical arrays *)
                LinearSolve[SparseArray @ partionedCov[[2, 2]]],
            True,
                With[{inv = Inverse[partionedCov[[2, 2]]]},
                    Function[inv . #]
                ]
        ];
        dist = Quiet[
            {
                partitionedMu[[1]] + partionedCov[[1, 2]] . inv22[Subtract[conditionValues, partitionedMu[[2]]]],
                Replace[
                    Subtract[
                        partionedCov[[1, 1]], 
                        partionedCov[[1, 2]] . If[ sparseQ,
                            SparseArray @ inv22[partionedCov[[2, 1]]],
                            inv22[partionedCov[[2, 1]]]
                        ]
                    ],
                    m_?(MatrixQ[#, NumericQ]&) :> Divide[Transpose[m] + m, 2] (* guarantees symmetry of numerical results *)
                ]
            },
            LinearSolve::exanexb
        ];
        If[ IntegerQ[marginals],
            Flatten[dist],
            If[ symmetrizedQ,
                MapAt[SymmetrizedArray[#, Automatic, Symmetric[{1, 2}]]&, dist, 2],
                dist
            ]
        ]
        ,
        And[
            Replace[
                Length[rules] < dim,
                False :> (Message[conditionedMultinormalDistribution::noDim, rules[[All, 1]]]; False)
            ],
            Length[cov] === dim
        ]
    ]
];

End[] (* End Private Context *)

EndPackage[]