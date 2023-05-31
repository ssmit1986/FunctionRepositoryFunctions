(* Wolfram Language Package *)

BeginPackage["FunctionRepo`MonitorFile`", {"FunctionRepo`", "GeneralUtilities`"}]
(* Exported symbols added here with SymbolName::usage *)
GeneralUtilities`SetUsage[MonitorFile, 
	"MonitorFile[Dynamic[contents$], fileName$] creates a dynamic that shows the contents of file fileName$ and updates contents$."
];

Begin["`Private`"] (* Begin Private Context *) 

Options[MonitorFile] = {
	"FileCheckFunction" -> "LastModificationDate",
	"ImportFunction" -> Import,
	"ContentDisplayFunction" -> Automatic,
	"LayoutFunction" -> Automatic,
	"PurgeContentsQ" -> False,
	UpdateInterval -> 1.,
	"Placeholder" -> ProgressIndicator[Appearance -> "Necklace"]
};

MonitorFile[
	Dynamic[fileContents_],
	file_String,
	opts : OptionsPattern[]
] := With[{
	fileCheckFun = Replace[
		OptionValue["FileCheckFunction"],
		prop_String :> Function[Information[File[#], prop]]
	],
	importFunction = OptionValue["ImportFunction"],
	formatFun = Replace[
		OptionValue["ContentDisplayFunction"],
		Automatic :> Function[
			StringForm["`1` bytes imported", ByteCount[#]]
		]
	],
	layoutFunction = Replace[
		OptionValue["LayoutFunction"],
		Automatic :> defaultGrid
	],
	updateInterval = OptionValue[UpdateInterval],
	purgeContentsQ = OptionValue["PurgeContentsQ"],
	placeholder = OptionValue["Placeholder"]
},
	DynamicModule[{
		display = Replace[placeholder, None -> ""],
		hash = "",
		modificationDate = ""
	}, 
		DynamicWrapper[
			DynamicWrapper[
				Dynamic[display, TrackedSymbols :> {display}]
				,
				hash; (* This triggers the update *)
				If[ FileExistsQ[file]
					,
					If[ placeholder =!= None,
						display = placeholder
					];
					modificationDate = Information[File[file], "LastModificationDate"];
					fileContents = importFunction[file];
					display = layoutFunction[file, formatFun[fileContents], modificationDate]
				],
				SynchronousUpdating -> False, 
				TrackedSymbols :> {hash}
			],
			If[ FileExistsQ[file],
				hash = fileCheckFun[file]
				,
				hash = "";
				modificationDate = Missing[];
				display = layoutFunction[file, "", modificationDate];
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

defaultGrid[file_, _, _Missing] := Tooltip[
	Style[StringForm["File `1` doesn't exist", FileNameTake[file]], "Text"],
	file
];

defaultGrid[file_, display_, modificationDate_] := Grid[
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