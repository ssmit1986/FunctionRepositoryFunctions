(* Wolfram Language Package *)

BeginPackage["FunctionRepo`firstMatchingValue`", {"FunctionRepo`"}]
(* Exported symbols added here with SymbolName::usage *)
firstMatchingValue::usage = "firstMatchingValue[{expr_1, expr_2, ...}, pattern] evalutates held expr_i in turn, returning the value of the first expression that evaluates to a result matching the pattern.";

Begin["`Private`"] (* Begin Private Context *)

SetAttributes[firstMatchingValue, HoldAll];
Options[firstMatchingValue] = Options[FirstCase];

firstMatchingValue[expr_, patt_, opt : OptionsPattern[]] := firstMatchingValue[expr, patt, Missing["NotFound"], {1}, opt];
firstMatchingValue[expr_, patt_, default_, opt : OptionsPattern[]] := firstMatchingValue[expr, patt, default, {1}, opt];

firstMatchingValue[
    expr_,
    (head : (Rule | RuleDelayed))[patt_, transformation_],
    default_,
    lvl_,
    opts : OptionsPattern[]
] := Module[{
    matched
},
    FirstCase[
        Unevaluated[expr],
        possibleMatch_ :> With[{
            try = Replace[
                matched = True;
                possibleMatch
                ,
                {
                    head[patt, transformation],
                    _ :> (matched = False)
                }
            ]
        },
            try /; TrueQ[matched]
        ],
        default,
        lvl,
        opts
    ]
];

firstMatchingValue[
    expr_,
    otherPattern_,
    default_,
    lvl_,
    opts : OptionsPattern[]
] := FirstCase[
    Unevaluated[expr],
    possibleMatch_ :> With[{try = possibleMatch},
        try /; MatchQ[try, otherPattern]
    ],
    default,
    lvl,
    opts
];

End[] (* End Private Context *)

EndPackage[]