(* Wolfram Language Package *)

BeginPackage["FunctionRepo`multiSet`", {"FunctionRepo`", "GeneralUtilities`"}]
(* Exported symbols added here with SymbolName::usage *)
SetUsage[multiSet, "multiSet[list$] converts a list to a multiset. It can be used with multiset operations like Union and Intersection."];


Begin["`Private`"] (* Begin Private Context *)

multiSet[assoc_?AssociationQ /; AnyTrue[assoc, NonPositive]] := multiSet[Select[assoc, !TrueQ[NonPositive[#]]&]];
multiSet[assoc_?AssociationQ /; !OrderedQ[Keys[assoc]]] := multiSet[KeySort @ assoc];

multiSet[list_List] := multiSet[KeySort @ Counts[list]];
multiSet /: Normal[multiSet[assoc_?AssociationQ]] := Join @@ KeyValueMap[ConstantArray, assoc];

combiners = <|
    Union -> Max,
    Intersection -> Min,
    Plus -> Total,
    Complement -> Function[First[#] - Total[Rest[#]]]
|>;

KeyValueMap[
    Function[{fun, comb},
        multiSet /: fun[sets : multiSet[_?AssociationQ]..] := multiSet[
            Merge[
                KeyUnion[{sets}[[All, 1]], 0&],
                comb
            ]
        ]
    ],
    combiners
];

Map[
    Function[fun,
        multiSet /: fun[multiSet[assoc_?AssociationQ]] := fun[assoc]
    ],
    {Keys, Values, Length, Total}
];

End[] (* End Private Context *)

EndPackage[]