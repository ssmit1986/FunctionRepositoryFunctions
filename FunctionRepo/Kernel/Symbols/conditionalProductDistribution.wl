(* Wolfram Language Package *)

BeginPackage["FunctionRepo`ConditionalProductDistribution`", {"FunctionRepo`", "GeneralUtilities`"}]
(* Exported symbols added here with SymbolName::usage *)
GeneralUtilities`SetUsage[ConditionalProductDistribution, "ConditionalProductDistribution[Distributed[var$1, dist$1], Distributed[var$2, dist$2], $$] represents a vector distribution where each dist$i can dependend on var$j for all i$ < j$"];

Begin["`Private`"] (* Begin Private Context *) 

protectedSymbolQ[sym_Symbol] := MatchQ[Quiet @ Attributes[sym], {___, Protected, ___}];
protectedSymbolQ[_] := False;

canonicalVarQ = MatchQ[_Symbol?protectedSymbolQ | (_Symbol?protectedSymbolQ)[___]];

$head = Distributed | Equal;

$patt = Repeated[$head[_, _]];

canonicalQ[$head[var_List, dist_]] := var =!= {} && TrueQ[AllTrue[var, canonicalVarQ]];
canonicalQ[$head[var_, dist_]] := canonicalVarQ[var];
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

randomVariables[dists__] := Flatten @ {dists}[[All, 1]];

dependencyOrderedQ[dists : $head[_, _]..] := With[{
	ndists = Length[{dists}],
	vars = randomVariables[dists],
	list = {dists}
},
	Which[
		!DuplicateFreeQ[vars],
			Message[
				ConditionalProductDistribution::duplicates,
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
			Message[ConditionalProductDistribution::depend];
			False,
		True, True
	]
];
dependencyOrderedQ[___] := False;

ConditionalProductDistribution::depend = "Dependency of distributions is circular or not ordered correctly.";
ConditionalProductDistribution::duplicates = "Duplicate variables `1` found.";

ConditionalProductDistribution /: Graph[ConditionalProductDistribution[dists : $patt], rest___] := Module[{
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


ConditionalProductDistribution /: Normal[expr : ConditionalProductDistribution[__Distributed]] := expr;

ConditionalProductDistribution /: Normal[ConditionalProductDistribution[spec : $patt]] := With[{
	dists = Cases[{spec}, _Distributed],
	rules = Flatten @ Cases[{spec}, e_Equal :> Thread[Rule @@ e]]
},
	If[ MatchQ[rules, {___Rule}],
		ConditionalProductDistribution @@ (dists /. rules),
		$Failed
	]
];


MapApply[
	Function[{fun, wrapper, aggregator},
		ConditionalProductDistribution /: fun[
			ConditionalProductDistribution[dists__Distributed],
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
		];
		ConditionalProductDistribution /: fun[
			dist : ConditionalProductDistribution[dists : $patt],
			coords_List
		] := With[{
			normal = Normal[dist]
		},
			fun[normal, coords] /; !FailureQ[normal]
		]
	],
	{
		{PDF, None, Apply[Times]},
		{Likelihood, List, Apply[Times]},
		{LogLikelihood, List, Total}
	}
];


rvStep[{opts___}][assoc_, Distributed[var_, dist_]] := Prepend[assoc, 
	Replace[
		{var, RandomVariate[dist /. assoc, opts]},
		{
			{v : Except[_List], num : Except[_RandomVariate]} :> v -> num,
			{v_List, num_List} /; Length[v] === Length[num] :> AssociationThread[v, num],
			_ :> Throw[$Failed, rvNoNum]
		}
	]
];

rvStep[{opts___}][assoc_, Equal[var_, expr_]] := With[{
	val = expr /. assoc
},
	Prepend[
		assoc,
		If[ ListQ[var],
			Check[
				AssociationThread[var, val],
				Throw[$Failed, rvNoNum],
				{AssociationThread::idim}
			],
			var -> val
		]
	]
];

ConditionalProductDistribution /: RandomVariate[
	ConditionalProductDistribution[dists : $patt],
	opts : OptionsPattern[]
] := Catch[
	If[ TrueQ @ Association[opts][TableHeadings],
		Identity,
		Values
	] @ Fold[
		rvStep[FilterRules[{opts}, Options[RandomVariate]]],
		<||>,
		Reverse @ {dists}
	],
	rvNoNum
];

ConditionalProductDistribution /: RandomVariate[
	pdist_ConditionalProductDistribution,
	n_Integer,
	opts : OptionsPattern[]
] := Table[RandomVariate[pdist, opts], n];

ConditionalProductDistribution /: RandomVariate[
	pdist_ConditionalProductDistribution,
	spec : {__Integer},
	opts : OptionsPattern[]
] := Table[RandomVariate[pdist, opts], Evaluate[Sequence @@ Map[List, spec]]];

ConditionalProductDistribution[dists : $patt] /; !dependencyOrderedQ[dists] := $Failed;
ConditionalProductDistribution[___, Except[$head[_, _]] | Distributed[{}, _], ___] := $Failed;
ConditionalProductDistribution[dists : $patt] /; !AllTrue[{dists}, canonicalQ] := Module[{
	rules = canonicalReplacementRules[randomVariables[dists]],
	newDists
},
	newDists = ReplaceAll[{dists}, rules];
	If[ AllTrue[newDists, canonicalQ],
		ConditionalProductDistribution @@ newDists,
		$Failed
	]
];

ConditionalProductDistribution /: HoldPattern @ DistributionParameterQ[
	ConditionalProductDistribution[dists__]
] := TrueQ @ And[
	MatchQ[{dists}, {$patt..}],
	AllTrue[
		Cases[{dists}, _Distributed][[All, 2]],
		DistributionParameterQ
	]
];

ConditionalProductDistribution /: HoldPattern @ Mean[
	expr : ConditionalProductDistribution[__]?DistributionParameterQ
] := With[{
	normal = Normal[expr]
},
	Fold[
		Function[
			Expectation[#1, #2]
		],
		Flatten[(List @@ normal)[[All, 1]]],
		List @@ normal
	] /; !FailureQ[normal]
];

End[] (* End Private Context *)

EndPackage[]