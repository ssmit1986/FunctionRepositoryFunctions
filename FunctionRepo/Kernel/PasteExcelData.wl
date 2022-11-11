(* Wolfram Language Package *)

BeginPackage["FunctionRepo`PasteExcelData`", {"FunctionRepo`"}]
(* Exported symbols added here with SymbolName::usage *)
GeneralUtilities`SetUsage[PasteString, 
	"PasteExcelData[] returns tabular data corresponding to the current Excel data on the clipboard."
];

Begin["`Private`"] (* Begin Private Context *) 

PasteExcelData[] := Module[{nb, get},
	PreemptProtect[
		nb = NotebookPut[
			Notebook[{Cell[BoxData[""], "Code"]}],
			Visible -> True (* Unfortunately, pasting doesn't work otherwise *)
		];
		SelectionMove[nb, Before, CellContents];
		FrontEndTokenExecute["Paste"];
		get = NotebookGet[nb];
		NotebookClose[nb]
	];
	Replace[
		FirstCase[get, Cell[cont_, ___] :> processContent[cont], $Failed, Infinity],
		s_String :> ImportString[s, "TSV"]
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