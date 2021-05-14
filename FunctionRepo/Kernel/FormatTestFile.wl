(* Wolfram Language Package *)

BeginPackage["FunctionRepo`FormatTestFile`", {"FunctionRepo`", "CodeFormatter`"}]
(* Exported symbols added here with SymbolName::usage *)
GeneralUtilities`SetUsage[FormatTestFile,
    "FormatTestFile[file$] makes a .wlt file produced from a testing notebook more readable.
FormatTestFile[fileIn$, fileOut$] writes the result to a new file."
];

Begin["`Private`"] (* Begin Private Context *)

SetAttributes[toInputFormString, HoldAllComplete];
toInputFormString[expr_] := ToString[Unevaluated[InputForm[expr]], CharacterEncoding -> "ASCII"];

FormatTestFile[file_String?FileExistsQ, fileOut : (_String | Automatic) : Automatic] := Enclose @ Module[{
    expressions = Confirm @ Import[file, {"Package", "HeldExpressions"}],
    stringOut,
    indentStr = "    "
},
    stringOut = StringRiffle[
        List /@ Replace[
            expressions,
            {
                HoldComplete[VerificationTest[args__]] :> StringJoin[
                    "VerificationTest[\n    ",
                    StringRiffle[
                        StringReplace[
                            Map[
                                CodeFormat[#, 
                                    "IndentationString" -> indentStr,
                                    "NewlineString" -> "\n"
                                ]&,
                                Map[toInputFormString, Unevaluated[{args}]]
                            ],
                            "\n" ~~ indentStr :> "\n" <> indentStr <> indentStr
                        ],
                        "\n    ,\n    "
                    ],
                    "\n]"
                ],
                HoldComplete[expr_] :> CodeFormat[toInputFormString[expr]]
            },
            {1}
        ],
        "\n\n",
        " "
    ];
    ConfirmBy[stringOut, SyntaxQ];
    Export[
        Replace[fileOut, Automatic :> file],
        stringOut,
        "String"
    ]
];


End[] (* End Private Context *)

EndPackage[]
