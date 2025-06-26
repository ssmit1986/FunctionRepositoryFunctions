(* Wolfram Language Package *)

BeginPackage["FunctionRepo`GraphPathAggregate`", {"FunctionRepo`"}]
(* Exported symbols added here with SymbolName::usage *)
GeneralUtilities`SetUsage[GraphPathAggregate,
	"GraphPathsAggregate[gr$, f$, path$] computes f[weight$1, weight$2, ...] for each EdgeWeight in path in path$ on graph gr$."
];

Begin["`Private`"] (* Begin Private Context *)


Options[GraphPathAggregate] = {
	"EdgeProperty" -> EdgeWeight,
	"VertexProperty" -> None
};

GraphPathAggregate[gr_?GraphQ, f_, opts : OptionsPattern[]][path_List] := GraphPathAggregate[gr, f, path, opts];

GraphPathAggregate[gr_?GraphQ, f_, l_List, opts : OptionsPattern[]] := Module[{
	graph = gr,
	path = l,
	fun = f,
	allEdges = EdgeList[gr],
	edgePropName = OptionValue["EdgeProperty"],
	vertexPropName = OptionValue["VertexProperty"],
	listResultQ = True,
	pathEdges, edgePropValues, vertexPropValues,
	allValues
},
	Enclose[
		Which[
			ContainsOnly[path, allEdges],
				pathEdges = {path};
				listResultQ = False
				,
			AllTrue[path, Comap[ListQ && ContainsOnly[allEdges]]],
				pathEdges = path
				,
			True,
				pathEdges = ConfirmBy[FunctionRepo`FindPathEdges[graph, path], ListQ]
		];
		edgePropValues = Replace[
			Map[AnnotationValue[{graph, #}, edgePropName]&, pathEdges],
			$Failed -> Missing["NotDefined", edgePropName],
			{2}
		];
		If[ vertexPropName =!= None,
			vertexPropValues = vertexValues[graph, vertexPropName] /@ pathEdges;
			allValues = MapThread[Riffle, {vertexPropValues, edgePropValues}]
			,
			allValues = edgePropValues
		];
		If[ TrueQ @ listResultQ,
			fun @@@ allValues,
			fun @@ First[allValues, {}]
		]
	]
];


vertexValues[gr_, prop_][{}] := {};
vertexValues[gr_, prop_][path_] := Replace[
	AnnotationValue[
		{gr, Append[path[[All, 1]], path[[-1, 2]]]},
		prop
	],
	$Failed -> Missing["NotDefined", prop],
	{1}
];


End[] (* End Private Context *)

EndPackage[]
