(* Wolfram Language Package *)

BeginPackage["FunctionRepo`importAGSData`", {"FunctionRepo`", "GeneralUtilities`"}]
(* Exported symbols added here with SymbolName::usage *)
GeneralUtilities`SetUsage[importAGSData, "importAGSData[file$] imports data in AGS format and converts it to a Wolfram Association."];

Begin["`Private`"] (* Begin Private Context *) 

importAGSData[file_?FileExistsQ] /; StringMatchQ[FileExtension[file], "AGS", IgnoreCase -> True] := importAGSData[Import[file, "Text"]]

importAGSData[importString_String] := Module[{
	groupStrings, groupBlockData
},
	groupStrings = extractGroupBlocks[importString];
	If[ groupStrings === {},
		Return[$Failed, Module]
	];
	
	groupBlockData = ImportString[#, "CSV"]& /@ groupStrings;
	
	Replace[
		Apply[Join] @ Select[AssociationQ] @ Map[
			processGroupBlock,
			groupBlockData
		],
		{
			<||> -> $Failed,
			Except[_?AssociationQ] -> $Failed
		}
	]
];
importAGSData[___] := $Failed;

$groupLabel = "\"GROUP\"";

findGroupNames[string_String] := StringCases[
	string,
	str : (StartOfLine ~~ $groupLabel ~~ Shortest[___] ~~ EndOfLine) :> 
		Last[Flatten[ImportString[str, "CSV"]], Missing[]]
];

extractGroupBlocks[string_String] := StringTrim[
	StringCases[string,
		StringExpression[
			StartOfLine,
			s : StringExpression[$groupLabel, Shortest[___]],
			$groupLabel | EndOfString
		] :> s,
		Overlaps -> True
	]
];

processGroupBlock[{{"GROUP", groupName_String}, rest__List}] := Module[{
	data = Replace[{rest}, "" -> Missing["NoData"], {2}],
	headers,
	headingLabel = "HEADING",
	dataLabel = "DATA",
	assoc
},
	If[ !(MatrixQ[data] && MatchQ[data, {{_String, __}..}]),
		Return[$Failed, Module]
	];
	
	headers = FirstCase[data, {headingLabel, s__String} :> {s}];
	If[ MissingQ[headers], Return[$Failed, Module]];
	
	assoc = GroupBy[
		DeleteCases[data, {headingLabel, __}],
		First -> Rest,
		Query[All, AssociationThread[headers, Range @ Length[headers]]]
	];
	<|
		groupName -> Prepend[
			MapAt[ (* Only the DATA label can have multiple rows *)
				First,
				assoc,
				{Key[#]}& /@ DeleteCases[Keys[assoc], dataLabel]
			],
			headingLabel -> headers
		]
	|>
];
processGroupBlock[_] := $Failed;

ImportExport`RegisterImport["AGS",
	{
		"Elements" :> Function[{"Elements" -> {"Data", "Groups"}}],
		"Groups" :> Function[
			{
				"Groups" -> findGroupNames[Import[#, "Text"]]
			}
		], 
		"Data" :> Function[
			{
				"Data" -> importAGSData[#]
			}
		]
	},
	"DefaultElement" :> "Data"
];

End[] (* End Private Context *)

EndPackage[]