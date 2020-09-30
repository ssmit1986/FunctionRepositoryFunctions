(* Wolfram Language Package *)

BeginPackage["FunctionRepo`MultiNonlinearModelFit`", {"FunctionRepo`"}]
(* Exported symbols added here with SymbolName::usage *)
MultiNonlinearModelFit::usage = "MultiNonlinearModelFit[{dat1, dat2, ...}, {fun1, fun2, ...}, params, vars] fits fun_i to data_i simultaneously.";

Begin["`Private`"] (* Begin Private Context *)

Options[MultiNonlinearModelFit] = Join[
    Options[NonlinearModelFit],
    {
        "DatasetIndexSymbol" -> \[FormalN]
    }
];

MultiNonlinearModelFit[datasets_, form_, fitParams_, independents : Except[_List], opts : OptionsPattern[]] := 
    MultiNonlinearModelFit[datasets, form, fitParams, {independents}, opts];
 
MultiNonlinearModelFit[datasets_, form : Except[{__Rule}, _List], fitParams_, independents_, opts : OptionsPattern[]] := 
    MultiNonlinearModelFit[datasets, <|"Expressions" -> form, "Constraints" -> True|>, fitParams, independents, opts];
 
MultiNonlinearModelFit[
    datasets : {__?(MatrixQ[#1, NumericQ] &)}, 
    KeyValuePattern[{"Expressions" -> expressions_List, "Constraints" -> constraints_}], 
    fitParams_List, 
    independents_List,
    opts : OptionsPattern[]
] := Module[{
    fitfun, weights,
    numSets = Length[expressions],
    precision = Precision @ datasets,
    augmentedData,
    indexSymbol = OptionValue["DatasetIndexSymbol"]
},
    Condition[
        augmentedData = Join @@ MapIndexed[
            Join[ConstantArray[N[#2, precision], Length[#1]], #1, 2]&,
            datasets
        ];
        fitfun = With[{
            conditions = Join @@ Transpose[{Range[numSets], expressions}]
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
                "InverseLengthWeights" :> Join @@ (ConstantArray[1/#1, #1] & ) /@ Length /@ datasets
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