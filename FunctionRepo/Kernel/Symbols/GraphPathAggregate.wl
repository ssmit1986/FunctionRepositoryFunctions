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
	allVertices = VertexList[gr],
	edgePropName = OptionValue["EdgeProperty"],
	vertexPropName = OptionValue["VertexProperty"],
	listResultQ = True,
	verticesOnPath = Missing[],
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
			ContainsOnly[path, allVertices],
				pathEdges = ConfirmBy[FunctionRepo`FindPathEdges[graph, path], ListQ];
				verticesOnPath = path,
			True,
				Confirm @ Failure["NotAPath",
					<|
						"MessageTemplate" -> "The provided argument `1` does not specify a valid path",
						"MessageParameters" -> {path}
					|>
				]
		];
		edgePropValues = Replace[
			Map[AnnotationValue[{graph, #}, edgePropName]&, pathEdges],
			$Failed -> Missing["NotDefined", edgePropName],
			{2}
		];
		If[ vertexPropName =!= None,
			vertexPropValues = If[ ListQ[verticesOnPath],
				ConstantArray[
					Replace[
						AnnotationValue[{gr, verticesOnPath}, vertexPropName],
						$Failed -> Missing["NotDefined", vertexPropName],
						{1}
					],
					Length[pathEdges]
				],
				Confirm @* vertexValues[graph, vertexPropName] /@ pathEdges
			];
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


pathVertices[{}] := {};
pathVertices[path_List] := With[{
	gr = PathGraph[UndirectedEdge[#1, #2]& @@@ path]
},
	If[ PathGraphQ[gr],
		With[{list = FindHamiltonianPath[gr]},
			If[ MemberQ[path[[1, {1, 2}]], First[list]],
				list,
				Reverse[list]
			]
		],
		Failure["NotAPath",
			<|
				"MessageTemplate" -> "The provided edges `1` do not form a valid path",
				"MessageParameters" -> {path}
			|>
		]
	]
];


vertexValues[_, _][{}] := {};
vertexValues[gr_, prop_][path_] := With[{
	verts = pathVertices[path]
},
	If[ FailureQ[verts],
		verts,
		Replace[
			AnnotationValue[
				{gr, verts},
				prop
			],
			$Failed -> Missing["NotDefined", prop],
			{1}
		]
	]
];


End[] (* End Private Context *)

EndPackage[]
