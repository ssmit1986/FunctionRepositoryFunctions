Unprotect["FunctionRepo`*", "FunctionRepo`*`*"];
ClearAll["FunctionRepo`*", "FunctionRepo`*`*"];

BeginPackage["FunctionRepo`"]

Begin["`Private`"]

With[{
	files = FileNames["*.wl", FileNameJoin[{DirectoryName[$InputFileName], "Symbols"}]]
},
	Scan[
		Function[file,
			Symbol @ StringJoin[
				"FunctionRepo`",
				StringReplace[FileBaseName[file], StartOfString ~~ u_ ~~ rest__ :> ToUpperCase[u] <> rest]
			];
			Get[file]
		],
		files
	]
];

End[]

EndPackage[]