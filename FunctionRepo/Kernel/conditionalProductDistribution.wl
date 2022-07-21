(* Wolfram Language Package *)

BeginPackage["FunctionRepo`conditionalProductDistribution`", {"FunctionRepo`", "GeneralUtilities`"}]
(* Exported symbols added here with SymbolName::usage *)
GeneralUtilities`SetUsage[conditionalProductDistribution, "conditionalProductDistribution[Distributed[var$1, dist$1], Distributed[var$2, dist$2], $$] represents a vector distribution where each dist$i can dependend on var$j for all i$ < j$"];

Begin["`Private`"] (* Begin Private Context *) 

protectedSymbolQ[sym_Symbol] := MatchQ[Quiet @ Attributes[sym], {___, Protected, ___}];
protectedSymbolQ[_] := False;

canonicalVarQ = MatchQ[_Symbol?protectedSymbolQ | (_Symbol?protectedSymbolQ)[___]];

canonicalQ[Distributed[var_List, dist_]] := var =!= {} && TrueQ[AllTrue[var, canonicalVarQ]];
canonicalQ[Distributed[var_, dist_]] := canonicalVarQ[var];
canonicalQ[_] := False;

canonicalReplacementRules[{}] := {};
canonicalReplacementRules[vars_List] := Block[{
	dupFree = DeleteDuplicates[Flatten @ vars],
	i
},
	i = 1 + Max[0, Cases[dupFree, \[FormalX][n_Integer] :> n]];
	AssociationThread[
		dupFree,
		Replace[
			dupFree,
			v : Except[_?canonicalVarQ] :> \[FormalX][i++],
			{1}
		]
	]
];

randomVariables[dists__Distributed] := Flatten @ {dists}[[All, 1]];

dependencyOrderedQ[dists : Distributed[_, _]..] := With[{
	ndists = Length[{dists}],
	vars = randomVariables[dists],
	list = {dists}
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
				FreeQ[ (* test if the nth distribution does not depend on any of the first n variables *)
					list[[#, 2]],
					Alternatives @@ DeleteDuplicates @ Flatten[list[[;; #, 1]]]
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

conditionalMap[f_, agg_, dists : {__Distributed}] := agg @ Map[
	f[#[[2]], #[[1]]]&,
	dists
];
conditionalMap[f_, agg_, dists : {__Distributed}, wrapper_] := agg @ Map[
	f[#[[2]], wrapper @ #[[1]]]&,
	dists
];
conditionalMap[f_, agg_, dists : {{__Distributed}..}, rest___] := Map[
	conditionalMap[f, agg, #, rest]&,
	dists
];
conditionalMap[___] := $Failed

MapThread[
	Function[{fun, wrapper, aggregator},
		conditionalProductDistribution /: fun[
			conditionalProductDistribution[dists__Distributed],
			coords_List
		] := Module[{
			vars = randomVariables[dists],
			assoc,
			nvars
		},
			nvars = Length[vars];
			Which[
				MatchQ[Replace[coords, {l_List :> Length[l], _ -> $Failed}, {1}], {nvars..}],
					assoc = AssociationThread[vars, #]& /@ coords,
				Length[coords] === nvars,
					assoc = AssociationThread[vars, coords],
				True,
					Return[$Failed, Module]
			];
			Replace[fun,
				{
					Likelihood | LogLikelihood :> aggregator,
					_ :> Identity
				}
			] @ conditionalMap[fun, aggregator, {dists} /. assoc, Replace[wrapper, None :> Sequence[]]]
		]
	],
	{
		{PDF,		   Likelihood,	 LogLikelihood   },
		{None,		  List,		   List			},
		{Apply[Times],  Apply[Times],   Total		   }
	}
];

conditionalProductDistribution /: RandomVariate[
	conditionalProductDistribution[dists__Distributed],
	opts : OptionsPattern[]
] := Catch[
	Values @ Fold[
		Function[
			Prepend[#1, 
				Replace[
					{#2[[1]], RandomVariate[#2[[2]] /. #1, opts]},
					{
						{var : Except[_List], num : Except[_RandomVariate]} :> var -> num,
						{var_List, num_List} /; Length[var] === Length[num] :> AssociationThread[var, num],
						_ :> Throw[$Failed, rvNoNum]
					}
				]
			]
		],
		<||>,
		Reverse @ {dists}
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
conditionalProductDistribution[___, Except[Distributed[_, _]] | Distributed[{}, _], ___] := $Failed;
conditionalProductDistribution[dists__Distributed] /; !AllTrue[{dists}, canonicalQ] := Module[{
	rules = canonicalReplacementRules[randomVariables[dists]],
	newDists
},
	newDists = ReplaceAll[{dists}, rules];
	If[ AllTrue[newDists, canonicalQ],
		conditionalProductDistribution @@ newDists,
		$Failed
	]
];


End[] (* End Private Context *)

EndPackage[]