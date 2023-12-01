Unprotect["FunctionRepo`*", "FunctionRepo`*`*"];
ClearAll["FunctionRepo`*", "FunctionRepo`*`*"];

BeginPackage["FunctionRepo`"]

Begin["`Private`"]

With[{
	files = FileNames["*.wl", FileNameJoin[{DirectoryName[$InputFileName], "Symbols"}]]
},
	Scan[
		Function[file,
			Symbol["FunctionRepo`" <> FileBaseName[file]];
			Get[file]
		],
		files
	]
];

End[]

EndPackage[]