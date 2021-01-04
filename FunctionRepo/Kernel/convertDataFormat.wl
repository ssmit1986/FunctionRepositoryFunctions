(* Wolfram Language Package *)

BeginPackage["FunctionRepo`convertDataFormat`", {"FunctionRepo`"}]
(* Exported symbols added here with SymbolName::usage *)
convertDataFormat::usage = "convertDataFormat[data, type] attempts to convert machine learning data to a different format to make it easier to switch out fitting methods.";

Begin["`Private`"] (* Begin Private Context *)

convertDataFormat::uneqLen = "Input and output lists are of unequal length";
convertDataFormat::outDim = "Output data has to be 1D for matrix or vector output";
convertDataFormat::convertFail = "Failed to convert data from type `1` to `2`";
convertDataFormat::vectIncompatible = "The provided inputs are incompatible with the Vector output type.\nThe input data should be of the form Range[n] for some value n";
convertDataFormat::notImplemented = "Output type `1` has not been implemented for this data format";

$dataTypes = <|
    "Matrix" -> _?MatrixQ,
    "ListOfRules" -> {__Rule},
    "Vector" -> Except[{__Rule}, _?VectorQ],
    "RuleOfLists" -> (_List -> _List),
    "Association" -> _Association?(MatchQ[KeyValuePattern[{"Input" -> _List, "Output" -> _List}]])
|>;
emptyDataQ = MatchQ @ Alternatives[
    {}, {{}}, <||> ,
    ({} -> {}), 
    _Association(MatchQ[KeyValuePattern[{"Input" -> {}, "Output" -> {}}]])
];

convertDataFormat[type_String][data_] := convertDataFormat[data, type];

convertDataFormat[_?emptyDataQ, "Matrix" | "Vector" | "ListOfRules"] := {};
convertDataFormat[_?emptyDataQ, "RuleOfLists"] := {} -> {};
convertDataFormat[_?emptyDataQ, "Assocation"] := <|"Input" -> {}, "Output" -> {}|>;

convertDataFormat[data_, typeOut_String] /; MemberQ[Keys[$dataTypes], typeOut] := Catch[
    Module[{
        typeIn = Catch @ KeyValueMap[
            Function[
                If[ MatchQ[data, #2],
                    Throw[#1]
                ]
            ],
            $dataTypes
        ],
        dataOut
    },
        Condition[
            If[ typeIn === typeOut, Throw[data, convertDataFormat]];
            
            dataOut = Developer`ToPackedArray /@ convertToRuleOfLists[data, typeIn];
            If[ UnsameQ @@ Map[Length, dataOut],
                Message[convertDataFormat::uneqLen];
                Throw[$Failed, convertDataFormat]
            ];
            If[ And[
                    MatchQ[typeOut, "Matrix" | "Vector"],
                    MatchQ[dataOut, _ -> _?MatrixQ],
                    MatchQ[Dimensions[dataOut[[2]]], {_, _?(# > 1&)}]
                ]
                ,
                Message[convertDataFormat::outDim];
                Throw[$Failed, convertDataFormat]
            ];
            dataOut = convertToTargetType[dataOut, typeOut];
            If[ MatchQ[dataOut, $dataTypes[typeOut]]
                ,
                dataOut
                ,
                Message[convertDataFormat::convertFail, typeIn, typeOut];
                $Failed
            ]
            ,
            StringQ[typeIn]
        ]
    ],
    convertDataFormat
];
convertDataFormat[_, out_] := (
    Message[convertDataFormat::notImplemented, out];
    $Failed
);

With[{
    cf = Compile[{
        {lst, _Integer, 1}
    },
        Module[{
            i = 1,
            boole = True
        },
            Do[
                boole = j == i++;
                If[ !boole, Break[]],
                {j, lst}
            ];
            boole
        ]
    ]
},
    (* Test if a list is equal to Range[n] for some n *)
    rangeQ[{}] := True;
    rangeQ[list : {__Integer}] := cf[list];
    rangeQ[list_?MatrixQ] /; MatchQ[Dimensions[list], {_, 1}] := rangeQ[list[[All, 1]]];
    rangeQ[_] := False
];

convertToRuleOfLists[data_, "Matrix"] := Switch[
    Dimensions[data]
    ,
    {_, 1},
        Range[Length[data]] -> data[[All, 1]],
    {_, 2},
        data[[All, 1]] -> data[[All, 2]],
    _,
        data[[All, 1 ;; -2 ;; 1]] -> data[[All, -1]]
];
convertToRuleOfLists[data_, "Vector"] := Range[Length[data]] -> data;
convertToRuleOfLists[data_, "ListOfRules"] := data[[All, 1]] -> data[[All, 2]];
convertToRuleOfLists[data_, "RuleOfLists"] := data;
convertToRuleOfLists[data_, "Association"] := data["Input"] -> data["Output"];

convertToTargetType[in_ -> out_, "Matrix"] := Join[##, 2]& @@ Replace[
    {in, out},
    vec_List?VectorQ :> List /@ vec,
    {1}
];
convertToTargetType[in_ -> out_?VectorQ, "Vector"] /; rangeQ[in] := out;
convertToTargetType[in_ -> out_?MatrixQ, "Vector"] /; And[
    MatchQ[Dimensions[out], {_, 1}],
    rangeQ[in]
] := out[[All, 1]];
convertToTargetType[_, "Vector"] := (Message[convertDataFormat::vectIncompatible]; $Failed);
convertToTargetType[data_, "ListOfRules"] := Thread[data];
convertToTargetType[data_, "RuleOfLists"] := data
convertToTargetType[in_ -> out_, "Association"] := <|
    "Input" -> in,
    "Output" -> out
|>;

End[] (* End Private Context *)

EndPackage[]