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
	Module[{
		fileAssoc
	},
		fileAssoc = ConfirmBy[FileSystemMap[#&, dir, depth], AssociationQ];
		If[ !ListQ[files], files = {}];
		files = Intersection[files, Cases[fileAssoc, _String, {-1}]];
		viewer[
			fileAssoc //. {
				a_Association :> Normal[a], 
				Verbatim[Rule][s_String, file_String] :> With[{
					checkbox = Checkbox[
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
				},
					Row[{checkbox, " ", s}]
				]
			}
		]
	]
];

FileTreePicker[___] := $Failed;

viewer[list_List] := Column[
	Replace[
		list,
		r_Rule :> OpenerView[{First[r], viewer[Last[r]]}, Method -> "Active"],
		{1}
	]
];

End[] (* End Private Context *)

EndPackage[]
