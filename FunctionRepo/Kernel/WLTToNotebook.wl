(* Wolfram Language Package *)

BeginPackage["FunctionRepo`WLTToNotebook`", {"FunctionRepo`", "GeneralUtilities`"}]
(* Exported symbols added here with SymbolName::usage *)
GeneralUtilities`SetUsage[WLTToNotebook,
	"WLTToNotebook[testfile$] converts a a .wlt file to a testing notebook."
];

Begin["`Private`"] (* Begin Private Context *) 

WLTToNotebook[
	file_?FileExistsQ
] /; MatchQ[ToLowerCase @ FileExtension[file] , "wlt" | "mt"] := Enclose @ Module[{
	heldContents,
	cellids = CreateDataStructure["HashSet"]
},
	Block[{$Context, $ContextPath},
		Needs["MUnit`"];
		heldContents = Confirm[Import[file, {"WL", "HeldExpressions"}], "Import error"];
		heldContents = testToCellGroup[#, cellids]& /@ heldContents;
	];
		
	NotebookPut @ Notebook[
		Cases[heldContents, _Cell],
		ShowGroupOpener -> True,
		TaggingRules -> Association["$testsRun" -> False],
		StyleDefinitions -> FrontEnd`FileName[
			{"MUnit"}, "MUnit.nb",
			CharacterEncoding -> "UTF-8"
		]
	]
];

generateUniqueID[max_, hashTable_] := Module[{i = 0},
	TimeConstrained[
		While[True,
			i = RandomInteger[max];
			If[ TrueQ @ hashTable["Insert", i],
				Break[]
			]
		],
		2
	];
	i
];

SetAttributes[testToCellGroup, HoldAllComplete];

testToCellGroup[
	HoldComplete[test : VerificationTest[fst_, args___]],
	cellids_
] /; Quiet @ CheckArguments[test, 1] := testToCellGroup[VerificationTest[fst, True, {}, args], cellids];

testToCellGroup[
	HoldComplete[test : VerificationTest[fst_, snd_, args___]],
	cellids_
] /; Quiet @ CheckArguments[test, 2] := testToCellGroup[VerificationTest[fst, snd, {}, args], cellids];

testToCellGroup[
	HoldComplete[test_VerificationTest],
	cellids_
] /; Quiet @ CheckArguments[test, 3] := testToCellGroup[test, cellids]

testToCellGroup[
	test : VerificationTest[in_, out_, msgs_, opts___],
	cellids_
] := With[{
	imax = 10^9
},
	Cell @ CellGroupData[
		{
			Cell[
				BoxData @ MakeBoxes[in, StandardForm],
				"VerificationTest",
				CellID -> generateUniqueID[imax, cellids]
			],
			Cell[
				BoxData @ MakeBoxes[out, StandardForm],
				"ExpectedOutput",
				CellID -> generateUniqueID[imax, cellids]
			],
			Cell[
				BoxData @ MakeBoxes[msgs, StandardForm],
				"ExpectedMessage",
				CellID -> generateUniqueID[imax, cellids]
			],
			Cell[
				BoxData @ MakeBoxes[{opts}, StandardForm],
				"TestOptions",
				CellID -> generateUniqueID[imax, cellids]
			],
			Cell[
				BoxData @ ToBoxes @ MUnit`bottomCell[],
				"BottomCell",
				CellID -> generateUniqueID[imax, cellids]
			]
		},
		Open
	]
];

testToCellGroup[HoldComplete[MUnit`BeginTestSection[section_String]], _] := Cell[section, "Subsection"];

testToCellGroup[other_, _] := Nothing;

End[] (* End Private Context *)

EndPackage[]