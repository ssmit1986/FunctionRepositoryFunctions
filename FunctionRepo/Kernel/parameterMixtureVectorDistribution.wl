(* Wolfram Language Package *)

BeginPackage["FunctionRepo`parameterMixtureVectorDistribution`", {"FunctionRepo`", "GeneralUtilities`"}]
(* Exported symbols added here with SymbolName::usage *)
GeneralUtilities`SetUsage[parameterMixtureVectorDistribution, "parameterMixtureVectorDistribution[Distributed[var$1, dist$1], Distributed[var$2, dist$2], $$] represents a vector distribution where each dist$i can dependend on var$j for all j$ < i$"];

Begin["`Private`"] (* Begin Private Context *) 

randomVariables[dists__Distributed] := Flatten @ {dists}[[All, 1]];

dependencyOrderedQ[dists : Distributed[_, _]..] := With[{
    ndists = Length[{dists}],
    vars = randomVariables[dists]
},
    Which[
        !DuplicateFreeQ[vars],
            Message[
                parameterMixtureVectorDistribution::duplicates,
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
            Message[parameterMixtureVectorDistribution::depend];
            False,
        True, True
    ]
];
dependencyOrderedQ[___] := False;

parameterMixtureVectorDistribution::depend = "Dependency of distributions is circular or not ordered correctly.";
parameterMixtureVectorDistribution::duplicates = "Duplicate variables `1` found.";

parameterMixtureVectorDistribution /: Graph[parameterMixtureVectorDistribution[dists__Distributed], rest___] := Module[{
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
        parameterMixtureVectorDistribution /: fun[
            parameterMixtureVectorDistribution[dists__Distributed],
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

parameterMixtureVectorDistribution[dists : Distributed[_, _]..] /; !dependencyOrderedQ[dists] := $Failed;
parameterMixtureVectorDistribution[___, Except[Distributed[_, _]], ___] := $Failed;

End[] (* End Private Context *)

EndPackage[]