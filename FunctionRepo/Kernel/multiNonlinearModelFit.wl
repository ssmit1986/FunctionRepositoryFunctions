(* Wolfram Language Package *)

BeginPackage["FunctionRepo`multiNonlinearModelFit`", {"FunctionRepo`"}]
(* Exported symbols added here with SymbolName::usage *)
multiNonlinearModelFit::usage = "multiNonlinearModelFit[{dat1, dat2, ...}, {fun1, fun2, ...}, params, vars] fits fun_i to data_i simultaneously.";

Begin["`Private`"] (* Begin Private Context *)

Options[multiNonlinearModelFit] = Join[
    Options[NonlinearModelFit],
    {
        "DatasetIndexSymbol" -> \[FormalN]
    }
];

multiNonlinearModelFit[datasets_, form_, fitParams_, independents : Except[_List], opts : OptionsPattern[]] := 
    multiNonlinearModelFit[datasets, form, fitParams, {independents}, opts];
 
multiNonlinearModelFit[datasets_, form : Except[{__Rule}, _List], fitParams_, independents_, opts : OptionsPattern[]] := 
    multiNonlinearModelFit[datasets, <|"Expressions" -> form, "Constraints" -> True|>, fitParams, independents, opts];
 
multiNonlinearModelFit[
    datasets : {__?(MatrixQ[#1, NumericQ] &)}, 
    KeyValuePattern[{"Expressions" -> expressions_List, "Constraints" -> constraints_}], 
    fitParams_List, 
    independents_List,
    opts : OptionsPattern[]
] := Module[{
    fitfun, weights,
    numSets = Length[expressions], 
    augmentedData = Join @@ MapIndexed[
        Join[ConstantArray[N[#2], Length[#1]], #1, 2]&,
        datasets
    ], 
    indexSymbol = OptionValue["DatasetIndexSymbol"]
},
    Condition[
        fitfun = With[{
            conditions = Join @@ ({#1, expressions[[#1]]} & ) /@ Range[numSets]
        }, 
            Switch @@ Prepend[conditions, Round[indexSymbol]]
        ]; 
        weights = Replace[
            OptionValue[Weights],
            {
                (list_List)?(VectorQ[#1, NumericQ]& ) /; Length[list] === numSets :> 
                    Join @@ MapThread[ConstantArray, {list, Length /@ datasets}], 
                list : {__?(VectorQ[#1, NumericQ] & )} /; Length /@ list === Length /@ datasets :>
                    Join @@ list, 
                "InverseLengthWeights" :> Join @@ (ConstantArray[1./#1, #1] & ) /@ Length /@ datasets
            }
        ]; 
        NonlinearModelFit[
            augmentedData,
            If[TrueQ[constraints], fitfun, {fitfun, constraints}], 
            fitParams,
            Flatten[{indexSymbol, independents}],
            Weights -> weights, 
            Sequence @@ FilterRules[{opts}, Options[NonlinearModelFit]]
        ]
        ,
        numSets === Length[datasets]
    ]
];

End[] (* End Private Context *)

EndPackage[]