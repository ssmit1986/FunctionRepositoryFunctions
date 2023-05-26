(* Wolfram Language Package *)

BeginPackage["FunctionRepo`ExcelClipboardData`", {"FunctionRepo`"}]
(* Exported symbols added here with SymbolName::usage *)
GeneralUtilities`SetUsage[ExcelClipboardData, 
	"ExcelClipboardData[] returns tabular data corresponding to the current Excel data on the clipboard."
];

Begin["`Private`"] (* Begin Private Context *) 

ExcelClipboardData[] := ExcelClipboardData["TSV"];

ExcelClipboardData[elem_, rest___] := Module[{nb, get},
	UsingFrontEnd @ PreemptProtect[
		WithCleanup[
			nb = NotebookPut[
				Notebook[{Cell[BoxData[""], "Code"]}],
				Visible -> True (* Unfortunately, pasting doesn't work otherwise *)
			]
			,
			SelectionMove[nb, Before, CellContents];
			FrontEndTokenExecute["Paste"];
			get = NotebookGet[nb]
			,
			NotebookClose[nb]
		]
	];
	Replace[
		FirstCase[get, Cell[cont_, ___] :> processContent[cont], $Failed, Infinity],
		s_String :> If[ elem === None,
			s,
			ImportString[s, elem, rest]
		]
	]
];

processContent[content_] := Replace[
	Flatten[{
		content //. (RowBox | BoxData)[arg_] :> arg
	}],
	list : {___String} :> StringJoin[list]
];

End[] (* End Private Context *)

EndPackage[]