(* Wolfram Language Package *)

BeginPackage["FunctionRepo`FormatTestFile`", {"FunctionRepo`", "CodeFormatter`"}]
(* Exported symbols added here with SymbolName::usage *)
GeneralUtilities`SetUsage[FormatTestFile,
    "FormatTestFile[file$] makes a .wlt (or .mt) file produced from a testing notebook more readable.
FormatTestFile[fileIn$, fileOut$] writes the result to a new file."
];

GeneralUtilities`SetUsage[ConvertTestNotebooks,
    "ConvertTestNotebooks[nb$] converts a .nb testing notebook file to a .wlt file with the same name in the same directory.
ConvertTestNotebooks[dir$] converts all notebooks in directory dir$."
];

Begin["`Private`"] (* Begin Private Context *)

SetAttributes[toInputFormString, HoldAllComplete];
toInputFormString[expr_] := ToString[Unevaluated[InputForm[expr]], CharacterEncoding -> "ASCII"];

FormatTestFile[] := FormatTestFile[SystemDialogInput["FileOpen"]];

FormatTestFile[$Canceled] := $Canceled;

FormatTestFile[dir_String?DirectoryQ,
    depth : (_Integer | {__} | _DirectedInfinity) : 1,
    opts : OptionsPattern[FileNames]
] := Map[
    FormatTestFile[#, Automatic]&,
    Join @@ Map[FileNames[#, dir, depth, opts]&, {"*.wlt", "*.mt"}]
];

FormatTestFile[file_String?FileExistsQ, fileOut : (_String | Automatic) : Automatic] := Enclose @ Module[{
    expressions = Confirm @ Import[file, {"Package", "HeldExpressions"}],
    stringOut,
    indentStr = StringRepeat[" ", 4]
},
    stringOut = StringRiffle[
        List /@ Replace[
            expressions,
            {
                HoldComplete[VerificationTest[args__]] :> StringJoin[
                    "VerificationTest[\n" <> indentStr,
                    StringRiffle[
                        StringReplace[
                            Map[
                                CodeFormatter`CodeFormat[#, 
                                    "IndentationString" -> indentStr,
                                    "NewlineString" -> "\n"
                                ]&,
                                Map[toInputFormString, Unevaluated[{args}]]
                            ],
                            "\n" ~~ indentStr :> "\n" <> indentStr <> indentStr
                        ],
                        "\n" <> indentStr <> ",\n" <> indentStr
                    ],
                    "\n]"
                ],
                HoldComplete[expr_] :> CodeFormatter`CodeFormat[toInputFormString[expr]]
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

FormatTestFile[___] := $Failed;

ConvertTestNotebooks[file_String?FileExistsQ] /; ToLowerCase @ FileExtension[file] === "nb" := Module[{
	nb, str, fileOut
},
	Block[{$Context, $ContextPath}, Needs["MUnit`"]];
	fileOut = StringReplace[file, ".nb" ~~ EndOfString :> ".wlt"];
	nb = NotebookOpen[file];
	str = MUnit`NotebookToTests[nb, "PreserveDataInSections" -> False];
	NotebookClose[nb];
	Export[fileOut, str, "String"];
	FormatTestFile[fileOut];
	fileOut
];

ConvertTestNotebooks[testDir_?DirectoryQ] := Map[
	ConvertTestNotebooks[#]&,
	FileNames["*.nb", testDir]
];

End[] (* End Private Context *)

EndPackage[]
