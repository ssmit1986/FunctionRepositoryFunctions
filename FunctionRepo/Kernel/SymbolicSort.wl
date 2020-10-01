(* Wolfram Language Package *)

BeginPackage["FunctionRepo`SymbolicSort`", {"FunctionRepo`", "GeneralUtilities`"}]
(* Exported symbols added here with SymbolName::usage *)
GeneralUtilities`SetUsage[SymbolicSort, 
    "SymbolicSort[{expr$1, expr$2, $$}, vars$, assum$] tries to sort expression expr$i using assumptions assum$.
SymbolicSort[list$, vars$, assum$, prop$] specifies what property should be returned"
];

Begin["`Private`"] (* Begin Private Context *) 

Options[SymbolicSort] = {
    TimeConstraint -> 5
};

SymbolicSort[list_List, varSpec_, assum_, opts : OptionsPattern[]] := Module[{
    vars = Replace[varSpec, var : Except[_List] :> {var}],
    timeCons = OptionValue[TimeConstraint],
    nItems = Length[list],
    orderings
},
    orderings = Flatten @ Table[
        orderExpressions[list[[i]], list[[j]], vars, assum, timeCons],
        {i, nItems},
        {j, 1, i - 1}
    ];
    Graph[orderings, VertexLabels -> Automatic]
];

orderExpressions[ex1_, ex2_, vars_, assum_, timeCons_] := Which[
    TrueQ[
        TimeConstrained[
            Resolve[ForAll[vars, assum, ex1 <= ex2]],
            timeCons
        ]
    ],
        DirectedEdge[ex1, ex2],
    TrueQ[
        TimeConstrained[
            Resolve[ForAll[vars, assum, ex1 > ex2]],
            timeCons
        ]
    ],
        DirectedEdge[ex2, ex1],
    True,
        UndirectedEdge[ex1, ex2]
];

End[] (* End Private Context *)

EndPackage[]