(* Wolfram Language Package *)

BeginPackage["FunctionRepo`graphicsToGraph`", {"FunctionRepo`", "GeneralUtilities`"}]
(* Exported symbols added here with SymbolName::usage *)
GeneralUtilities`SetUsage[graphicsToGraph,
    "graphicsToGraph[gr$] converts the points, lines and arrows in graphics gr$ to a graph by joining the endpoints of the lines and arrows to the closes points."
];

Begin["`Private`"] (* Begin Private Context *) 

graphicsToGraph[gr : _Graphics | _Graphics3D] := Module[{
    pts = Join @@ Cases[gr,
        Point[arg_] :> Replace[Setting[arg], lst : {__?NumericQ} :> {lst}],
        DirectedInfinity[1]
    ],
    edges1 = Cases[gr,
        Line[lst_] :> UndirectedEdge @@ Setting[lst][[{1, -1}]],
        DirectedInfinity[1]
    ],
    edges2 = Cases[gr,
        Arrow[lst_] :> DirectedEdge @@ Setting[lst][[{1, -1}]],
        DirectedInfinity[1]
    ],
    nf, vertices
},
    vertices = Range[Length[pts]];
    
    Condition[
        nf = Nearest[pts -> "Index"];
        Graph[
            vertices,
            Map[First @ nf[#, 1]&, Join[edges1, edges2], {2}],
            VertexCoordinates -> Thread[vertices -> pts]
        ]
        ,
        Length[pts] > 0
    ]
];


End[] (* End Private Context *)

EndPackage[]