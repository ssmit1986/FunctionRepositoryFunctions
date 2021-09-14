(* Wolfram Language Package *)

BeginPackage["FunctionRepo`WLTToNotebook`", {"FunctionRepo`", "GeneralUtilities`"}]
(* Exported symbols added here with SymbolName::usage *)
GeneralUtilities`SetUsage[WLTToNotebook,
	"WLTToNotebook[testfile$] converts a a .wlt file to a testing notebook."
];

Begin["`Private`"] (* Begin Private Context *) 

WLTToNotebook[file_] := Enclose @ Module[{
	heldContents,
	cellids
},
	ConfirmBy[file, FileExistsQ, "File does not exist"];
	ConfirmBy[ToLowerCase @ FileExtension[file], MatchQ["wlt" | "mt"], "File extension is not .wlt or .mt"];
	Block[{$Context, $ContextPath},
		Needs["MUnit`"];
		heldContents = Confirm[Import[file, {"WL", "HeldExpressions"}], "Import error"];
		cellids = CreateDataStructure["HashSet"];
		heldContents = testToCellGroup[#, cellids]& /@ heldContents;
	];
		
	NotebookPut @ Notebook[
		Cases[Flatten @ heldContents, _Cell],
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

(* Handle verification tests terminated by a ; *)
testToCellGroup[
	HoldComplete[CompoundExpression[expressions__]],
	rest___
] := Map[
	testToCellGroup[#, rest]&,
	Thread @ HoldComplete[{expressions}]
];

(* Handle 1-arg tests *)
testToCellGroup[
	HoldComplete[test : VerificationTest[fst_, args___]],
	cellids_
] /; Quiet @ CheckArguments[test, 1] := testToCellGroup[VerificationTest[fst, True, {}, args], cellids];

(* Handle 2-arg tests *)
testToCellGroup[
	HoldComplete[test : VerificationTest[fst_, snd_, args___]],
	cellids_
] /; Quiet @ CheckArguments[test, 2] := testToCellGroup[VerificationTest[fst, snd, {}, args], cellids];

(* Handle 3-arg tests *)
testToCellGroup[
	HoldComplete[test_VerificationTest],
	cellids_
] /; Quiet @ CheckArguments[test, 3] := testToCellGroup[test, cellids]

(* Convert test to Cells *)
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