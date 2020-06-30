(* Wolfram Language Package *)

BeginPackage["FunctionRepo`conditionalProductDistribution`", {"FunctionRepo`", "GeneralUtilities`"}]
(* Exported symbols added here with SymbolName::usage *)
GeneralUtilities`SetUsage[conditionalProductDistribution, "conditionalProductDistribution[Distributed[var$1, dist$1], Distributed[var$2, dist$2], $$] represents a vector distribution where each dist$i can dependend on var$j for all j$ < i$"];

Begin["`Private`"] (* Begin Private Context *) 

randomVariables[dists__Distributed] := Flatten @ {dists}[[All, 1]];

dependencyOrderedQ[dists : Distributed[_, _]..] := With[{
    ndists = Length[{dists}],
    vars = randomVariables[dists]
},
    Which[
        !DuplicateFreeQ[vars],
            Message[
                conditionalProductDistribution::duplicates,
                Keys @ Select[Counts[vars], GreaterThan[1]]
            ];
            False,
        !TrueQ[
            And @@ Map[
                FreeQ[ (* test if the nth distribution does not depend on any of the (n-m)th variables *)
                    {dists}[[#, 2]],
                    Alternatives @@ DeleteDuplicates @ Flatten[Drop[{dists}[[All, 1]], # - 1]]
                ]&,
                Range[1, ndists]
            ]
        ],
            Message[conditionalProductDistribution::depend];
            False,
        True, True
    ]
];
dependencyOrderedQ[___] := False;

conditionalProductDistribution::depend = "Dependency of distributions is circular or not ordered correctly.";
conditionalProductDistribution::duplicates = "Duplicate variables `1` found.";

conditionalProductDistribution /: Graph[conditionalProductDistribution[dists__Distributed], rest___] := Module[{
    vars = randomVariables[dists],
    edges
},
    edges = Flatten @ Map[
        Outer[
            DirectedEdge,
            Cases[#[[2]], Alternatives @@ vars, {0, DirectedInfinity[1]}],
            Intersection[vars, Flatten @ {#[[1]]}]
        ]&,
        {dists}
    ];
    Graph[vars, edges, rest, VertexLabels -> Automatic]
];

MapThread[
    Function[{fun, accessor, aggregator},
        conditionalProductDistribution /: fun[
            conditionalProductDistribution[dists__Distributed],
            coords_List
        ] := With[{
            vars = randomVariables[dists]
        },
            aggregator @ Map[
                With[{
                    pos = Flatten @ Position[vars, Alternatives @@ Flatten[{#[[1]]}], {1}, Heads -> False]
                },
                    fun[
                        #[[2]],
                        accessor[coords, If[ListQ[#[[1]]], pos, First[pos]]]
                    ]
                ]&,
                {dists}
            ]
        ]
    ],
    {
        {PDF,           Likelihood,     LogLikelihood},
        {#1[[#2]]&,     #1[[All, #2]]&, #1[[All, #2]]&},
        {Apply[Times],  Apply[Times],   Total}
    }
];

conditionalProductDistribution /: RandomVariate[
    conditionalProductDistribution[dists__Distributed],
    opts : OptionsPattern[]
] := Catch[
    Module[{
    },
        Fold[
            Function[
                Replace[
                    {#2[[1]], RandomVariate[#2[[2]] /. #1, opts]},
                    {
                        {var : Except[_List], num_?AssociationQ} /; Length[num] === 1 :> Append[#1, var -> First[num]],
                        {var : Except[_List], num : Except[_RandomVariate]} :> Append[#1, var -> num],
                        {var_List, num_List} /; Length[var] === Length[num] :> Append[#1, AssociationThread[var, num]],
                        {var_List, num_?AssociationQ} /; Length[var] === Length[num] :> Append[#1, AssociationThread[var, Values[num]]],
                        _ :> Throw[$Failed, rvNoNum]
                    }
                ]
            ],
            <||>,
            {dists}
        ]
    ],
    rvNoNum
];

conditionalProductDistribution /: RandomVariate[
    pdist_conditionalProductDistribution,
    n_Integer,
    opts : OptionsPattern[]
] := Table[RandomVariate[pdist, opts], n];

conditionalProductDistribution /: RandomVariate[
    pdist_conditionalProductDistribution,
    spec : {__Integer},
    opts : OptionsPattern[]
] := Table[RandomVariate[pdist, opts], Evaluate[Sequence @@ Map[List, spec]]];

conditionalProductDistribution[dists : Distributed[_, _]..] /; !dependencyOrderedQ[dists] := $Failed;
conditionalProductDistribution[___, Except[Distributed[_, _]], ___] := $Failed;

End[] (* End Private Context *)

EndPackage[]