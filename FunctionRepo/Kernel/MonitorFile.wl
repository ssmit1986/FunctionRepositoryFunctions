(* Wolfram Language Package *)

BeginPackage["FunctionRepo`MonitorFile`", {"FunctionRepo`", "GeneralUtilities`"}]
(* Exported symbols added here with SymbolName::usage *)
GeneralUtilities`SetUsage[MonitorFile, 
	"MonitorFile[Dynamic[contents$], fileName$] creates a dynamic that shows the contents of file fileName$ and updates contents$."
];

Begin["`Private`"] (* Begin Private Context *) 

Options[MonitorFile] = {
	"FileCheckFunction" -> Function[FileHash[#, "SHA512"]],
	"ImportFunction" -> Import,
	"ContentDisplayFunction" -> Function[
		StringForm["`1` bytes imported", ByteCount[#]]
	],
	"LayoutFunction" -> Automatic,
	"PurgeContentsQ" -> False,
	UpdateInterval -> 1.
};

MonitorFile[
	Dynamic[fileContents_],
	file_,
	opts : OptionsPattern[]
] := With[{
	fileCheckFun = OptionValue["FileCheckFunction"],
	importFunction = OptionValue["ImportFunction"],
	formatFun = OptionValue["ContentDisplayFunction"],
	layoutFunction = Replace[
		OptionValue["LayoutFunction"],
		Automatic -> defaultGrid
	],
	updateInterval = OptionValue[UpdateInterval],
	purgeContentsQ = OptionValue["PurgeContentsQ"]
},
	DynamicModule[{
		display = "",
		hash = "",
		modificationDate = ""
	}, 
		DynamicWrapper[
			DynamicWrapper[
				Dynamic[
					layoutFunction[file, modificationDate, display],
					TrackedSymbols :> {modificationDate, display}
				]
				,
				hash; (* This triggers the update *)
				If[ FileExistsQ[file]
					,
					modificationDate = Information[File[file], "LastModificationDate"];
					fileContents = importFunction[file];
					display = formatFun[fileContents]
				],
				SynchronousUpdating -> False, 
				TrackedSymbols :> {hash}
			],
			If[ FileExistsQ[file],
				hash = fileCheckFun[file]
				,
				hash = "";
				modificationDate = Missing[];
				display = "";
				If[ purgeContentsQ,
					fileContents = Missing["FileMissing"]
				]
			]
			,		
			SynchronousUpdating -> True,
			UpdateInterval -> updateInterval,
			TrackedSymbols -> {}
		]
	]
];

defaultGrid[file_, _Missing, _] := Tooltip[
	Style[StringForm["File `1` doesn't exist", FileNameTake[file]], "Text"],
	file
];

defaultGrid[file_, modificationDate_, display_] := Grid[
	{
		{"File", Tooltip[FileNameTake[file], file]},
		{"Last modified:", Replace[modificationDate, d_?DateObjectQ :> DateString[d]]}, 
		{display, SpanFromLeft}
	},
	Alignment -> Left,
	BaseStyle -> "Text"
];

End[] (* End Private Context *)

EndPackage[]