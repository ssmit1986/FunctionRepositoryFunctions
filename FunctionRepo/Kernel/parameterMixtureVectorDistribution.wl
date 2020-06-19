(* Wolfram Language Package *)

BeginPackage["FunctionRepo`parameterMixtureVectorDistribution`", {"FunctionRepo`", "GeneralUtilities`"}]
(* Exported symbols added here with SymbolName::usage *)
GeneralUtilities`SetUsage[parameterMixtureVectorDistribution, "parameterMixtureVectorDistribution[{var$1, var$2, $$}, Distributed[var$1, dist$1], Distributed[var$2, dist$2], $$] represents a vector distribution where each dist$i can dependend on var$j for all j$ < i$"];

Begin["`Private`"] (* Begin Private Context *) 

distributionParameters[dists__Distributed] := DeleteDuplicates @ Flatten @ {dists}[[All, 1]];

dependencyOrderedQ[_List, dists : Distributed[_, _]..] := With[{
    ndists = Length[{dists}]
},
    If[ TrueQ[
            And @@ Map[
                FreeQ[ (* test if the nth distribution does not depend on any of the (n-m)th variables *)
                    {dists}[[#, 2]],
                    Alternatives @@ DeleteDuplicates @ Flatten[Drop[{dists}[[All, 1]], # - 1]]
                ]&,
                Range[1, ndists]
            ]
        ]
        ,
        True
        ,
        Message[parameterMixtureVectorDistribution::depend];
        False
    ]
];

dependencyOrderedQ[___] := False;

parameterMixtureVectorDistribution::nonDef = "Parameters `1` remain undefined.";
parameterMixtureVectorDistribution::depend = "Dependency of distributions is circular or not ordered correctly.";

parameterMixtureVectorDistribution[dists : Distributed[_, _]..] := parameterMixtureVectorDistribution[
    distributionParameters[dists],
    dists
];

parameterMixtureVectorDistribution[params_List /; !DuplicateFreeQ[params], dists__] := parameterMixtureVectorDistribution[
    DeleteDuplicates[params],
    dists
];

parameterMixtureVectorDistribution[params_List, dists : Distributed[_, _]..] := With[{
    allParams = distributionParameters[dists]
},
    If[ ContainsAll[allParams, params]
        ,
        parameterMixtureVectorDistribution[
            DeleteDuplicates[Join[params, allParams]],
            dists
        ]
        ,
        Message[parameterMixtureVectorDistribution::nonDef, Complement[params, allParams]];
        $Failed
    ] /; !ContainsAll[params, allParams]
];

parameterMixtureVectorDistribution[params_List, dists : Distributed[_, _]..] /; !dependencyOrderedQ[params, dists] := $Failed;

parameterMixtureVectorDistribution /: Graph[parameterMixtureVectorDistribution[params_List, dists__Distributed], rest___] := Module[{
    edges = Flatten @ Map[
        Outer[
            DirectedEdge,
            Cases[#[[2]], Alternatives @@ params, {0, DirectedInfinity[1]}],
            Intersection[params, Flatten @ {#[[1]]}]
        ]&,
        {dists}
    ]
},
    Graph[params, edges, rest, VertexLabels -> Automatic]
];

parameterMixtureVectorDistribution /: PDF[
    parameterMixtureVectorDistribution[params_List, dists__Distributed],
    coords_List
] /; Length[params] === Length[coords] := Module[{
    factors = Map[
        Replace[{
            #
        }],
        {dists}
    ]
},
    Times @@ factors
];

parameterMixtureVectorDistribution[Except[_List], ___] := $Failed;
parameterMixtureVectorDistribution[_, Except[_Distributed]..] := $Failed;

End[] (* End Private Context *)

EndPackage[]