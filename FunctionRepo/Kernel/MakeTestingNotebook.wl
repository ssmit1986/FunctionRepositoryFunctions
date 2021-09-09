(* Wolfram Language Package *)

BeginPackage["FunctionRepo`MakeTestingNotebook`", {"FunctionRepo`", "GeneralUtilities`"}]
(* Exported symbols added here with SymbolName::usage *)
GeneralUtilities`SetUsage[MakeTestingNotebook,
	"MakeTestingNotebook[testfile$] converts a a .wlt file to a testing notebook."
];

Begin["`Private`"] (* Begin Private Context *) 

Options[MakeTestingNotebook] = {};

MakeTestingNotebook[file_?FileExistsQ, opts : OptionsPattern[]] /; MatchQ[FileExtension[file] , "wlt" | "mt"] := Enclose @ Module[{
	str = Confirm[Import[file, "String"], "Import error"],
	heldContents
},
	Block[{$Context, $ContextPath}, Needs["MUnit`"]];
	heldContents = Replace[
		Confirm[
			ToExpression[str, StandardForm, HoldComplete],
			"Interpretation error"
		],
		{
			HoldComplete[Times[args__]] :> HoldComplete[{args}],
			HoldComplete[test_VerificationTest] :> HoldComplete[{test}]
		}
	];
	heldContents = First @ Replace[
		heldContents,
		test_VerificationTest :> testToCellGroup[test],
		{2}
	];
	
	NotebookPut @ Notebook[
		Cases[Flatten @ heldContents, _Cell],
		
		TaggingRules -> Association["$testsRun" -> False],
		StyleDefinitions -> FrontEnd`FileName[
			{"MUnit"}, "MUnit.nb",
			CharacterEncoding -> "UTF-8"
		]
	]
];

MakeTestingNotebook[]

SetAttributes[testToCellGroup, HoldAllComplete];

testToCellGroup[
	test : VerificationTest[fst_, args___]
] /; Quiet @ CheckArguments[test, 1] := testToCellGroup[VerificationTest[fst, True, {}, args], True];

testToCellGroup[
	test : VerificationTest[fst_, snd_, args___]
] /; Quiet @ CheckArguments[test, 2] := testToCellGroup[VerificationTest[fst, snd, {}, args], True];

testToCellGroup[
	test_VerificationTest
] /; Quiet @ CheckArguments[test, 3] := testToCellGroup[test, True]

testToCellGroup[
	test : VerificationTest[in_, out_, msgs_, opts___],
	True
] := Cell @ CellGroupData[
	{
		Cell[
			BoxData @ MakeBoxes[in, StandardForm],
			"VerificationTest"
		],
		Cell[
			BoxData @ MakeBoxes[out, StandardForm],
			"ExpectedOutput"
		],
		Cell[
			BoxData @ MakeBoxes[msgs, StandardForm],
			"ExpectedMessage"
		],
		Cell[
			BoxData @ MakeBoxes[{opts}, StandardForm],
			"TestOptions"
		],
		Cell[
			BoxData @ ToBoxes @ MUnit`bottomCell[],
			"BottomCell"
		]
	},
	Open
];


End[] (* End Private Context *)

EndPackage[]