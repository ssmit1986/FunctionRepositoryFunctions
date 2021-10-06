(* Wolfram Language Package *)

BeginPackage["FunctionRepo`ConvertToTemplateNotebook`", {"FunctionRepo`"}]
(* Exported symbols added here with SymbolName::usage *)
GeneralUtilities`SetUsage[ConvertToTemplateNotebook,
	"ConvertToTemplateNotebook[nb$] converts notebook nb$ to a template notebook."
];

Begin["`Private`"] (* Begin Private Context *)

templateNotebookOpts := Enclose @ Module[{
	nb = ConfirmMatch[
		CreateNotebook["Template", Visible -> False],
		 _NotebookObject
	],
	opts
},
	opts = Options[nb];
	NotebookClose[nb];
	ConfirmMatch[opts, {__Rule}];
	templateNotebookOpts = Normal @ KeyDrop[opts, {Visible, WindowSize, WindowMargins, FrontEndVersion}]
];

ConvertToTemplateNotebook[] := Enclose @ Module[{
	nb = ConfirmMatch[EvaluationNotebook[], _NotebookObject],
	nbCopy
},
	nbCopy = ConfirmMatch[NotebookPut @ NotebookGet[nb], _NotebookObject];
	ConvertToTemplateNotebook[nbCopy]
];

ConvertToTemplateNotebook[nb_NotebookObject] := Enclose @ SetOptions[
	nb,
	Confirm @ templateNotebookOpts
];

End[] (* End Private Context *)

EndPackage[]