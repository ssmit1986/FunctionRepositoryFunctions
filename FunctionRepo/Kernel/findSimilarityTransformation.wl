(* Wolfram Language Package *)

BeginPackage["FunctionRepo`mergeByKey`", {"FunctionRepo`", "GeneralUtilities`"}]
(* Exported symbols added here with SymbolName::usage *)
GeneralUtilities`SetUsage[findSimilarityTransformation, "findSimilarityTransformation[A$, B$] finds a matrix P$ such that B == Inverse[P$].A$.P$"];

Begin["`Private`"] (* Begin Private Context *) 

numMatQ = Function[MatrixQ[#, NumericQ]];

findSimilarityTransformation[Amat_List?numMatQ, Bmat_List?numMatQ] /; Dimensions[Amat] === Dimensions[Bmat] := With[{
    eigA = Eigensystem[Amat],
    eigB = Eigensystem[Bmat]
},
    Condition[
        Transpose[
            Inverse[eigB[[2]]] . eigA[[2]]
        ],
        TrueQ[eigA[[1]] == eigB[[1]]]
    ]
];

End[] (* End Private Context *)

EndPackage[]