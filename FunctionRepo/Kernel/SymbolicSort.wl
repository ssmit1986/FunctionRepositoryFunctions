(* Wolfram Language Package *)

BeginPackage["FunctionRepo`SymbolicSort`", {"FunctionRepo`", "GeneralUtilities`"}]
(* Exported symbols added here with SymbolName::usage *)
GeneralUtilities`SetUsage[SymbolicSort, 
    "SymbolicSort[{expr$1, expr$2, $$}, vars$] sorts expr$i that depend on vars$i.
SymbolicSort[list$, vars$, assum$] tries to sort expression expr$i using assumptions assum$.
SymbolicSort[list$, vars$, assum$, dom$] specifies the domain of vars$.
SymbolicSort[list$, vars$, assum$, dom&, prop$] specifies what property should be returned."
];

Begin["`Private`"] (* Begin Private Context *) 

Options[SymbolicSort] = {
    TimeConstraint -> 5
};

SymbolicSort::timeout = "Timeout while comparing `1` and `2`. Consider increasing the TimeConstraint option.";
SymbolicSort::noOrder = "No ordering of expressions `1` could be found."

SymbolicSort[{}, _, _, Graph, ___] := Graph[{}];
SymbolicSort[{}, __] := {};

SymbolicSort[list_List, varSpec_, opts : OptionsPattern[]] :=
    SymbolicSort[list, varSpec, True, Reals, opts];

SymbolicSort[list_List, varSpec_, assum_, opts : OptionsPattern[]] :=
    SymbolicSort[list, varSpec, assum, Reals, opts];

SymbolicSort[list_List, varSpec_, assum_, dom_, Graph, opts : OptionsPattern[]] := With[{
    graph = symbolicSortGraph[list, varSpec, assum, dom, OptionValue[TimeConstraint]]
},
    pruneGraph[graph] /; GraphQ[graph]
];

SymbolicSort[list_List, varSpec_, assum_, dom_, opts : OptionsPattern[]] := With[{
    sort = With[{timeCons = OptionValue[TimeConstraint]},
        Catch[
            Sort[
                list,
                Function[
                    Replace[
                        expressionOrder[#1, #2, varSpec, assum, dom, timeCons],
                        0 :> Throw[$Failed, symsort]
                    ]
                ]
            ],
            symsort
        ]
    ]
},
    sort /; Replace[
        ListQ[sort],
        False :> (
            Message[SymbolicSort::noOrder, Short[list]];
            False
        )
    ]
];

expressionOrder[ex1_, ex2_, varSpec_, assum_, dom_, timeCons_] := Catch[
    Which[
        TrueQ[
            TimeConstrained[
                Resolve[ForAll[varSpec, assum, ex1 <= ex2], dom],
                timeCons
                ,
                Message[SymbolicSort::timeout, ex1, ex2];
                Throw[0, timeout]
            ]
        ],
            1,
        TrueQ[
            TimeConstrained[
                Resolve[ForAll[varSpec, assum, ex1 >= ex2], dom],
                timeCons
                ,
                Message[SymbolicSort::timeout, ex1, ex2];
                Throw[0, timeout]
            ]
        ],
            -1,
        True,
            0
    ],
    timeout
];

symbolicSortGraph[list_List, varSpec_, assum_, dom_, timeCons_] := Module[{
    nItems = Length[list],
    orderings
},
    orderings = Flatten @ Table[
        Replace[
            expressionOrder[list[[i]], list[[j]], varSpec, assum, dom, timeCons],
            {
                1 :> DirectedEdge @@ list[[{i, j}]],
                -1 :> DirectedEdge @@ list[[{j, i}]],
                _ :> UndirectedEdge @@ list[[{i, j}]]
            }
        ],
        {i, nItems},
        {j, 1, i - 1}
    ];
    Graph[list, orderings]
];

pruneGraph[gr_] := With[{
    directedEdges = EdgeList[
        TransitiveReductionGraph @ EdgeDelete[gr, UndirectedEdge[_, _]]
    ],
    undirectedEdges = EdgeList @ EdgeDelete[gr, DirectedEdge[_, _]]
},
    Graph[
        VertexList[gr],
        Join[
            directedEdges,
            undirectedEdges
        ],
        VertexLabels -> Automatic
    ]
];

End[] (* End Private Context *)

EndPackage[]