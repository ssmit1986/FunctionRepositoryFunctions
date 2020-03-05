(* Wolfram Language Package *)

BeginPackage["FunctionRepo`importAGSData`", {"FunctionRepo`", "GeneralUtilities`"}]
(* Exported symbols added here with SymbolName::usage *)
importAGSData::usage = "importAGSData[file] imports data in AGS format and converts it to a Wolfram Association.";

Begin["`Private`"] (* Begin Private Context *) 

importAGSData[file_?FileExistsQ] /; StringMatchQ[FileExtension[file], "AGS", IgnoreCase -> True] := Module[
    {strings = Import[file, "Text"], data},
    strings = Map[
        Function[
            Map[Function[Join @@ ImportString[#, "CSV"]],
                StringTrim @ StringSplit[#, EndOfLine]
            ]
        ],
        StringTrim[
            StringCases[strings,
                StringExpression[StartOfLine,
                    s : StringExpression["\"GROUP\"", Shortest[___]],
                    "\"GROUP\"" | EndOfString
                ] :> s,
                Overlaps -> True
            ]
        ]
    ];
    data = Map[
        Function[
            <|
                #[[1]] -> Replace[#[[2 ;;]], "" -> Missing["NoData"], {1}]
            |>
        ],
        strings, {2}
    ];
    data = Map[
        Function[
            With[
                {
                    assoc = Replace[
                        Apply[Join, Cases[#, Except[KeyValuePattern["DATA" -> _]]]],
                        {el_} :> el,
                        {1}
                    ]
                },
                Join[assoc,
                    Map[
                        AssociationThread[assoc["HEADING"], #]&,
                        Select[KeyDrop[assoc, "HEADING"], VectorQ]
                    ],
                    Map[
                        AssociationThread[assoc["HEADING"], #]&,
                        AssociationTranspose[Cases[#, KeyValuePattern["DATA" -> _]]],
                        {2}
                    ]
                ]
            ]
        ],
        data
    ];
    AssociationThread[Lookup[data, "GROUP"], KeyDrop[data,"GROUP"]]
];
importAGSData[___] := $Failed;

ImportExport`RegisterImport["AGS",
    {
        "Elements" :> Function[{"Elements" -> {"Data", "Groups"}}],
        "Groups" :> Function[
            {
                "Groups" -> StringCases[
                    Import[#, "Text"],
                    str : (StartOfLine ~~ "\"GROUP\"" ~~ Shortest[___] ~~ EndOfLine) :> 
                        Last[Flatten[ImportString[str, "CSV"]], Missing[]]
                ]
            }
        ], 
        "Data" :> Function[
            {
                "Data" -> importAGSData[#]
            }
        ]
    },
    "DefaultElement" :> "Data"
];

End[] (* End Private Context *)

EndPackage[]