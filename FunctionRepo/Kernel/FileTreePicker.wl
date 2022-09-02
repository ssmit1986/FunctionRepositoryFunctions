(* Wolfram Language Package *)

BeginPackage["FunctionRepo`FileTreePicker`", {"FunctionRepo`", "CodeFormatter`"}]
(* Exported symbols added here with SymbolName::usage *)
GeneralUtilities`SetUsage[FileTreePicker,
	"FileTreePicker[Dynamic[files$], dir$] creates a dynamic file tree to dynamically select files."
];

Begin["`Private`"] (* Begin Private Context *)

FileTreePicker[files_] := FileTreePicker[files, SystemDialogInput["Directory"]];

FileTreePicker[files_, dir_] := FileTreePicker[files, dir, Infinity];

FileTreePicker[Dynamic[files_], dir_?DirectoryQ, depth_] := Enclose[
	If[ !ListQ[files], files = {}];
	viewer[
		ConfirmBy[
			FileSystemMap[
				Function[file,
					Checkbox[
						Dynamic[
							MemberQ[files, file],
							Function[
								files = If[ MemberQ[files, file],
									DeleteCases[files, file],
									Append[files, file]
								]
							]
						]
					]
				],
				dir,
				depth
			],
			AssociationQ
		] //. {
			a_Association :> Normal[a], 
			Verbatim[Rule][s_String, el_Checkbox] :> Row[{s, " ", el}]
		}
	]
];

FileTreePicker[___] := $Failed;

viewer[list_List] := Column[
	Replace[
		list,
		r_Rule :> OpenerView[{First[r], viewer[Last[r]]}],
		{1}
	]
];

End[] (* End Private Context *)

EndPackage[]
