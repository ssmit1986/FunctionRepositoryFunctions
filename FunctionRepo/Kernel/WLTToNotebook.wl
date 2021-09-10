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
	heldContents = Confirm[Import[file, {"WL", "HeldExpressions"}], "Import error"]
},
	Block[{$Context, $ContextPath},
		Needs["MUnit`"];
		heldContents = Confirm[Import[file, {"WL", "HeldExpressions"}], "Import error"]
	];
	heldContents = testToCellGroup /@ heldContents;
	
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

SetAttributes[testToCellGroup, HoldAllComplete];

testToCellGroup[
	HoldComplete[test : VerificationTest[fst_, args___]]
] /; Quiet @ CheckArguments[test, 1] := testToCellGroup[VerificationTest[fst, True, {}, args]];

testToCellGroup[
	HoldComplete[test : VerificationTest[fst_, snd_, args___]]
] /; Quiet @ CheckArguments[test, 2] := testToCellGroup[VerificationTest[fst, snd, {}, args]];

testToCellGroup[
	HoldComplete[test_VerificationTest]
] /; Quiet @ CheckArguments[test, 3] := testToCellGroup[test]

testToCellGroup[
	test : VerificationTest[in_, out_, msgs_, opts___]
] := With[{
	imax = 10^9
},
	Cell @ CellGroupData[
		{
			Cell[
				BoxData @ MakeBoxes[in, StandardForm],
				"VerificationTest",
				CellID -> RandomInteger[imax]
			],
			Cell[
				BoxData @ MakeBoxes[out, StandardForm],
				"ExpectedOutput",
				CellID -> RandomInteger[imax]
			],
			Cell[
				BoxData @ MakeBoxes[msgs, StandardForm],
				"ExpectedMessage",
				CellID -> RandomInteger[imax]
			],
			Cell[
				BoxData @ MakeBoxes[{opts}, StandardForm],
				"TestOptions",
				CellID -> RandomInteger[imax]
			],
			Cell[
				BoxData @ ToBoxes @ MUnit`bottomCell[],
				"BottomCell",
				CellID -> RandomInteger[imax]
			]
		},
		Open
	]
];

testToCellGroup[HoldComplete[MUnit`BeginTestSection[section_String]]] := Cell[section, "Subsection"];

testToCellGroup[other_] := Nothing;

End[] (* End Private Context *)

EndPackage[]