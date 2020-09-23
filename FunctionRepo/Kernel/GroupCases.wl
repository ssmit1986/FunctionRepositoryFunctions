(* Wolfram Language Package *)

BeginPackage["FunctionRepo`GroupCases`", {"FunctionRepo`", "GeneralUtilities`"}]
(* Exported symbols added here with SymbolName::usage *)
GeneralUtilities`SetUsage[GroupCases, "GroupCases[{el$1, el$2, $$}, patt$] groups elements el$i by wether or not the match patt$ and creates an association <|patt$ -> $$, _ -> $$|>
GroupCases[list$, {patt$1, patt$2, $$}] groups using multiple patterns.
GroupCases[patt$] represents an operator form of GroupCases. 
"];

Begin["`Private`"] (* Begin Private Context *) 

GroupCases[spec_][list_List] := GroupCases[list, spec];

GroupCases[{}, spec_] := With[{
    keys = patternList[spec]
},
    AssociationThread[keys, ConstantArray[{}, Length[keys]]]
];
GroupCases[list_List, Verbatim[_]] := <|_ -> list|>;
GroupCases[list_List, spec_] := Module[{
    pattSpec = patternList[spec],
    rules,
    results
},
    rules = MapIndexed[
        Function[
            With[{tag = #2[[1]]},
                match : #1 :> Sow[match, tag]
            ] 
        ],
        pattSpec
    ];
    results = Last @ Reap[
        Replace[list, rules, {1}],
        Range[Length[rules]]
    ];
    AssociationThread[pattSpec, Join @@@ results]
];

patternList[spec_] := DeleteDuplicates @ Replace[spec,
    {
        l_List :> Append[l, _],
        patt_ :> {patt, _}
    }
];

End[] (* End Private Context *)

EndPackage[]