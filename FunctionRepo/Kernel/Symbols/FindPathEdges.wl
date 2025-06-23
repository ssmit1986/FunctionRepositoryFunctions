(* Wolfram Language Package *)

BeginPackage["FunctionRepo`FindPathEdges`", {"FunctionRepo`"}]


GeneralUtilities`SetUsage[FindPathEdges,
	"FindPathEdges[gr$, s$, t$, $$] works like FindPath[$$] but returns a list of edges instead of vertices.
FindPathEdges[gr$, {v$1, v$2, $$}] converts a vertex path v$i to an edge path.
FindPathEdges[gr$, {p$1, p$2, $$}] converts multiple paths p$i.
"
];


Begin["`Private`"] (* Begin Private Context *)


tagPattern = RepeatedNull[_, 1];


FindPathEdges[gr_?GraphQ, s_, t_, rest___] := Module[{
	paths = FindPath[gr, s, t, rest],
	selfPaths
},
	If[ MatchQ[paths, {___List}],
		selfPaths = If[ s === t,
			List /@ EdgeList[gr, _[s, t, tagPattern]],
			{}
		];
		paths = Join @@ Map[convertToEdges[gr], paths];
		paths = Join[paths, selfPaths];
		paths
		,
		Failure["FindPathEdges", <|"MessageTemplate" -> "No valid paths found"|>]
	]
];

FindPathEdges[gr_?GraphQ, paths : {__List}] := With[{
	vertices = VertexList[gr]
},
	Map[convertToEdges[gr], paths] /; AllTrue[paths, ContainsOnly[vertices]]
];

FindPathEdges[gr_?GraphQ, path_List] := If[
	ContainsOnly[path, VertexList[gr]],
	convertToEdges[gr] @ path,
	Failure["FindPathEdges", <|"MessageTemplate" -> "Paths provided should only contain valid vertices"|>]
];


findEdges[gr_][{a_, b_}] := Join[
	EdgeList[gr, DirectedEdge[a, b, tagPattern]],
	EdgeList[gr, UndirectedEdge[OrderlessPatternSequence[a, b], tagPattern]]
];


convertToEdges[gr_][path_] := With[{
	pairs = Partition[path, 2, 1]
},
	Tuples[findEdges[gr] /@ pairs]
];



End[] (* End Private Context *)

EndPackage[]
