(* Wolfram Language Package *)

BeginPackage["FunctionRepo`LoadPacletFromFile`", {"FunctionRepo`"}]
(* Exported symbols added here with SymbolName::usage *)
GeneralUtilities`SetUsage[LoadPacletFromFile,
	"BlockDirectory[file$, context$] loads a paclet from a .paclet file."
];

Begin["`Private`"] (* Begin Private Context *) 

pacletFileQ[file_] := And[
	StringQ[file],
	FileExistsQ[file],
	StringEndsQ[file, ".paclet"]
];

LoadPacletFromFile[file_?pacletFileQ] := LoadPacletFromFile[file, StringDelete[FileNameTake[file], "-" ~~ ___] <> "`"];

LoadPacletFromFile[file_?pacletFileQ, context_String] := Module[{
	dir
},
	Enclose[
		WithCleanup[
			SetDirectory[$TemporaryDirectory];
			dir = CreateDirectory[];
			ResetDirectory[]
			,
			ConfirmAssert[DirectoryQ[dir]];
			Confirm @ ExtractArchive[file, dir];
			PacletDirectoryLoad[dir];
			Get[context]
			,
			Quiet @ PacletDirectoryUnload[dir];
			Quiet @ DeleteDirectory[dir, DeleteContents -> True]
		]
	]
];

End[] (* End Private Context *)

EndPackage[]