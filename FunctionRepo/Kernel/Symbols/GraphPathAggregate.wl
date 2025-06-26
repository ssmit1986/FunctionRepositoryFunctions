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


pathSort[{}] := {};
pathSort[path_List] := FixedPoint[
	SequenceReplace[{
		{de : DirectedEdge[x_, y_, ___], UndirectedEdge[z_, y_, r___]} :>
			Splice @ {de, UndirectedEdge[y, z, r]},
		{UndirectedEdge[x_, y_, r___], de : DirectedEdge[x_, z_, ___]} :>
			Splice @ {UndirectedEdge[y, x, r], de},
		{UndirectedEdge[x_, y_, r1___], UndirectedEdge[z_, y_, r2___]} :>
			Splice @ {UndirectedEdge[x, y, r1], UndirectedEdge[y, z, r2]},
		{UndirectedEdge[y_, x_, r1___], UndirectedEdge[y_, z_, r2___]} :>
			Splice @ {UndirectedEdge[x, y, r1], UndirectedEdge[y, z, r2]},
		{UndirectedEdge[y_, x_, r1___], UndirectedEdge[z_, y_, r2___]} :>
			Splice @ {UndirectedEdge[x, y, r1], UndirectedEdge[y, z, r2]}
	}],
	path
];

pathVertices[{}] := {};
pathVertices[path_] := With[{sorted = pathSort[path]},
	Append[sorted[[All, 1]], sorted[[-1, 2]]]
];

vertexValues[_, _][{}] := {};
vertexValues[gr_, prop_][path_] := With[{
	verts = pathVertices[path]
},
	Replace[
		AnnotationValue[
			{gr, verts},
			prop
		],
		$Failed -> Missing["NotDefined", prop],
		{1}
	]
];


End[] (* End Private Context *)

EndPackage[]
