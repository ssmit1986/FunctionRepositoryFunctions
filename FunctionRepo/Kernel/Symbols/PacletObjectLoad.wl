(* Wolfram Language Package *)

BeginPackage["FunctionRepo`PacletObjectLoad`", {"FunctionRepo`"}]
(* Exported symbols added here with SymbolName::usage *)
GeneralUtilities`SetUsage[PacletObjectLoad, "
PacletObjectLoad[obj$] loads the paclet object obj$ with Get, even if other versions of the same paclet are installed.
PacletObjectLoad[spec$, opts$$] finds paclets with PacletFind[spec$, opts$$] and loads the result if it is a single paclet.
* obj$ must be enabled and installed, or PacletObjectLoad fails.
* The option Method selects how competing paclets are kept out of the way while the first context of obj$ is loaded:
| 'DisablePaclets' | disable all other enabled paclets with the same name or context (default) |
| 'TemporaryCopy' | load a copy with a higher version number from $TemporaryDirectory |
| 'RaiseVersion' | raise the version number of the paclet in place while it is loaded |
* With 'DisablePaclets', the disabled paclets are enabled again afterwards, also when loading fails or is aborted.
* With 'TemporaryCopy', the paclet is copied to a new directory in $TemporaryDirectory, its version is prefixed with 9s \
until it is newer than every other paclet with the same name (or, failing that, set to the newest of those versions \
with a 9 in front), and the copy is put on the path with PacletDirectoryLoad. \
The copy is unloaded and deleted afterwards. Paclets that read files from their location after loading, \
for example through lazy autoloading, may not work then.
* Method -> {'TemporaryCopy', 'Cleanup' -> False} keeps the copy on the path and on disk after a successful load, \
so that files can still be loaded from it later. PacletFind then returns the copy with its raised version first \
for the rest of the session. The copy is always removed when loading fails.
* With 'RaiseVersion', the PacletInfo file in the location of obj$ is backed up and rewritten with a raised version, \
chosen as for 'TemporaryCopy', and PacletDataRebuild makes the paclet manager pick it up. Afterwards the original \
PacletInfo file is restored and PacletDataRebuild runs again, also when loading fails or is aborted. \
PacletObjectLoad fails without changing anything when it cannot write to the paclet location. \
If the restore fails, a message gives the location of the backup.
* Before loading, FindFile must resolve the context to a file inside the location of the paclet being loaded, or PacletObjectLoad fails.
* The return value is obj$, or a Failure."
];


Begin["`Private`"] (* Begin Private Context *)


PacletObjectLoad::notfound = "Paclet `1` version `2` at `3` is not among the enabled paclets found by PacletFind.";
PacletObjectLoad::noctx = "Paclet `1` does not define a valid context to load.";
PacletObjectLoad::nofile = "FindFile could not find a file for context `1`.";
PacletObjectLoad::wrongfile = "Context `1` resolves to `2`, which is not inside the paclet location `3`.";
PacletObjectLoad::getfail ="Get failed to load context `1`.";
PacletObjectLoad::notunique = "PacletFind found `1` paclets instead of exactly one.";
PacletObjectLoad::method = "Value `1` of option Method should be \"DisablePaclets\", \"TemporaryCopy\", \
{\"TemporaryCopy\", subopts} or \"RaiseVersion\".";
PacletObjectLoad::cleanup = "Value `1` of the \"Cleanup\" suboption of Method should be True or False.";
PacletObjectLoad::copyfail = "Could not copy paclet location `1` to the temporary directory `2`.";
PacletObjectLoad::version = "Could not find a version for paclet `1` that is newer than `2`.";
PacletObjectLoad::dirload = "PacletDirectoryLoad did not put the temporary copy of paclet `1` at `2` on the path.";
PacletObjectLoad::noinfo = "No PacletInfo.wl or PacletInfo.m file found in paclet location `1`.";
PacletObjectLoad::backup = "Could not back up the PacletInfo file of paclet location `1` to `2`.";
PacletObjectLoad::rebuild = "After raising its version to `2`, paclet `1` at `3` is not the first paclet found by PacletFind.";
PacletObjectLoad::nowrite = "Cannot write the PacletInfo file in paclet location `1`.";
PacletObjectLoad::restore ="Could not restore the PacletInfo file in `1`. The original is backed up in `2`.";

Options[PacletObjectLoad] = {Method -> "DisablePaclets"};

PacletObjectLoad[obj_PacletObject, opts : OptionsPattern[]] := Enclose[
	Module[{
		name = obj["Name"],
		location = obj["Location"],
		method = OptionValue[Method],
		cleanup,
		context,
		found,
		others
	},
		(* Normalize "name" and {"name", subopts} to {"name", subopts} *)
		method = ConfirmMatch[
			Replace[method, name_String :> {name}],
			{"DisablePaclets"} | {"TemporaryCopy", ___?OptionQ} | {"RaiseVersion"},
			failure["method", {method}]
		];
		cleanup = ConfirmBy[
			Lookup[Flatten[Rest[method]], "Cleanup", True],
			BooleanQ,
			failure["cleanup", {Lookup[Flatten[Rest[method]], "Cleanup"]}]
		];
		context = ConfirmBy[
			First[Flatten[{obj["Context"]}], Missing["NoContext"]],
			contextQ,
			failure["noctx", {name}]
		];
		found = Join[
			ConfirmMatch[PacletFind[name], {___PacletObject}],
			(* Paclets with other names that provide the same context are included to prevent context clashes *)
			ConfirmMatch[PacletFind["*", <|"Context" -> context|>], {___PacletObject}]
		];
		ConfirmAssert[
			MemberQ[Comap[found, "Location"], location],
			failure["notfound", {name, obj["Version"], location}]
		];
		others = DeleteDuplicatesBy[
			Select[found, #["Location"] =!= location &],
			#["Location"] &
		];
		Confirm @ Switch[First[method],
			"DisablePaclets",
				loadWithDisabledPaclets[context, location, others]
			,
			"TemporaryCopy",
				loadTemporaryCopy[obj, context, Select[others, #["Name"] === name &], cleanup]
			,
			"RaiseVersion",
				loadWithRaisedVersion[obj, context, Select[others, #["Name"] === name &]]
		];
		obj
	],
	unwrapFailure
];

(* Method goes to PacletObjectLoad, all other options are paclet properties for PacletFind *)
PacletObjectLoad[spec : Except[_PacletObject], opts___] := Replace[
	PacletFind[spec, Sequence @@ DeleteCases[{opts}, (Rule | RuleDelayed)[Method, _]]],
	{
		{paclet_PacletObject} :> PacletObjectLoad[paclet, Cases[{opts}, (Rule | RuleDelayed)[Method, _]]],
		list_List :> failure["notunique", {Length[list]}],
		other_ :> Failure["PacletFindFailure", <|
			"MessageTemplate" -> "PacletFind did not return a list of paclets.",
			"Result" -> other
		|>]
	}
];

PacletObjectLoad[args___] := Failure["InvalidArguments", <|
	"MessageTemplate" -> "PacletObjectLoad called with invalid arguments `1`.",
	"MessageParameters" -> {HoldForm[{args}]}
|>];

loadWithDisabledPaclets[context_String, location_String, others : {___PacletObject}] := Enclose[
	Module[{disabled = {}},
		WithCleanup[
			(* PacletDisable returns only the paclets it actually disabled *)
			disabled = Flatten[PacletDisable /@ others]
			,
			Confirm[loadContextFrom[context, location]]
			,
			(* Enable the exact objects from PacletFind: a name could resolve to a different version *)
			Scan[PacletEnable, disabled]
		]
	],
	unwrapFailure
];

loadTemporaryCopy[obj_PacletObject, context_String, others : {___PacletObject}, cleanup : True | False] := Enclose[
	Module[{
		name = obj["Name"],
		location = obj["Location"],
		tempDir,
		copy,
		version,
		loaded = False,
		success = False
	},
		WithCleanup[
			tempDir = ConfirmBy[CreateDirectory[], DirectoryQ]
			,
			copy = ConfirmBy[
				Quiet @ CopyDirectory[location, FileNameJoin[{tempDir, FileNameTake[location]}]],
				DirectoryQ,
				failure["copyfail", {location, tempDir}]
			];
			version = ConfirmBy[
				newerVersion[obj["Version"], Comap[others, "Version"]],
				StringQ,
				failure["version", {name, Comap[others, "Version"]}]
			];
			Confirm @ writePacletInfo[copy, Append[KeyDrop[First[obj], "Location"], "Version" -> version]];
			PacletDirectoryLoad[copy];
			loaded = True;
			ConfirmAssert[
				MatchQ[PacletFind[name], {first_PacletObject, ___} /; first["Version"] === version],
				failure["dirload", {name, copy}]
			];
			Confirm[loadContextFrom[context, copy]];
			success = True
			,
			(* A failed load leaves nothing that could need the copy later, so it is always cleaned up *)
			If[Or[cleanup, Not[success]],
				If[loaded, PacletDirectoryUnload[copy]];
				(* Deletion can fail, for example when the paclet loaded a library that is still in use *)
				If[StringQ[tempDir], Quiet @ DeleteDirectory[tempDir, DeleteContents -> True]]
			]
		]
	],
	unwrapFailure
];

loadWithRaisedVersion[obj_PacletObject, context_String, others : {___PacletObject}] := Enclose[
	Module[{
		name = obj["Name"],
		location = obj["Location"],
		infoFiles,
		backupDir,
		version,
		backedUp = False
	},
		infoFiles = ConfirmMatch[
			FileNames[{"PacletInfo.wl", "PacletInfo.m"}, location],
			{__String},
			failure["noinfo", {location}]
		];
		ConfirmAssert[writableQ[location, infoFiles], failure["nowrite", {location}]];
		version = ConfirmBy[
			newerVersion[obj["Version"], Comap[others, "Version"]],
			StringQ,
			failure["version", {name, Comap[others, "Version"]}]
		];
		WithCleanup[
			backupDir = ConfirmBy[CreateDirectory[], DirectoryQ]
			,
			ConfirmAssert[
				AllTrue[infoFiles, StringQ[Quiet @ CopyFile[#, FileNameJoin[{backupDir, FileNameTake[#]}]]] &],
				failure["backup", {location, backupDir}]
			];
			backedUp = True;
			Confirm @ writePacletInfo[location, Append[KeyDrop[First[obj], "Location"], "Version" -> version]];
			PacletDataRebuild[];
			ConfirmAssert[
				MatchQ[
					PacletFind[name],
					{first_PacletObject, ___} /; And[first["Version"] === version, first["Location"] === location]
				],
				failure["rebuild", {name, version, location}]
			];
			Confirm[loadContextFrom[context, location]]
			,
			Which[
				Not[StringQ[backupDir]],
					Null
				,
				Not[backedUp],
					Quiet @ DeleteDirectory[backupDir, DeleteContents -> True]
				,
				restorePacletInfo[location, backupDir, FileNameTake /@ infoFiles],
					PacletDataRebuild[];
					Quiet @ DeleteDirectory[backupDir, DeleteContents -> True]
				,
				True,
					(* Keep the backup so that the user can restore the paclet by hand *)
					PacletDataRebuild[];
					Message[PacletObjectLoad::restore, location, backupDir]
			]
		]
	],
	unwrapFailure
];

(* Check before changing anything: files can be created in dir, and each existing file can be opened for writing *)
writableQ[dir_String, files : {___String}] := With[{
	probe = FileNameJoin[{dir, StringJoin["PacletObjectLoad-", CreateUUID[], ".tmp"]}]
},
	And[
		WithCleanup[
			StringQ[Quiet @ CreateFile[probe]],
			Quiet @ If[FileExistsQ[probe], DeleteFile[probe]]
		],
		AllTrue[
			files,
			Function[file, Replace[Quiet @ OpenAppend[file], {stream_OutputStream :> (Close[stream]; True), _ -> False}]]
		]
	]
];

(* Put back exactly the original PacletInfo files and check their contents *)
restorePacletInfo[dir_String, backupDir_String, names : {__String}] := (
	Quiet @ DeleteFile[FileNames[{"PacletInfo.m", "PacletInfo.wl"}, dir]];
	AllTrue[
		names,
		Function[name,
			With[{original = FileNameJoin[{backupDir, name}], target = FileNameJoin[{dir, name}]},
				And[
					StringQ[Quiet @ CopyFile[original, target, OverwriteTarget -> True]],
					FileHash[original] === FileHash[target]
				]
			]
		]
	]
);

(* Make sure Get loads from location and not from another paclet or $Path entry that provides the same context *)
loadContextFrom[context_String, location_String] := Enclose[
	Module[{file},
		file = ConfirmBy[FindFile[context], StringQ, failure["nofile", {context}]];
		ConfirmAssert[
			fileInDirectoryQ[file, location],
			failure["wrongfile", {context, file, location}]
		];
		Confirm[Get[file], failure["getfail", {context}]]
	],
	unwrapFailure
];

newerVersion[version_String, {}] := StringJoin["9", version];

(* Prefix 9s until the version is newer than the newest competitor. Ten tries cover any realistic version number.
	If that fails, fall back to the newest version with a 9 in front *)
newerVersion[version_String, others : {__String}] := With[{
	newest = Extract[others, Ordering[others, 1, PacletNewerQ]]
},
	SelectFirst[
		{
			NestWhile[StringJoin["9", #] &, StringJoin["9", version], Not[PacletNewerQ[#, newest]] &, 1, 10],
			StringJoin["9", newest]
		},
		PacletNewerQ[#, newest] &,
		Missing["NoNewerVersion"]
	]
];

(* The PacletInfo.wl file takes precedence, so remove a PacletInfo.m file that could contradict it *)
writePacletInfo[dir_String, data_Association] := Enclose[
	Quiet @ DeleteFile[FileNames[{"PacletInfo.m", "PacletInfo.wl"}, dir]];
	ConfirmMatch[Put[PacletObject[data], FileNameJoin[{dir, "PacletInfo.wl"}]], Null];
	ConfirmMatch[PacletObject[File[dir]], _PacletObject]
];

contextQ[context_String] := StringEndsQ[context, "`"];
contextQ[_] := False;

fileInDirectoryQ[file_String, dir_String] := With[{
	dirParts = FileNameSplit[ExpandFileName[dir]],
	fileParts = FileNameSplit[ExpandFileName[file]]
},
	And[
		Length[fileParts] > Length[dirParts],
		(* Windows paths are case-insensitive *)
		If[$OperatingSystem === "Windows",
			ToLowerCase[Take[fileParts, Length[dirParts]]] === ToLowerCase[dirParts],
			Take[fileParts, Length[dirParts]] === dirParts
		]
	]
];
fileInDirectoryQ[_, _] := False;

(* Return the specific Failure instead of the generic ConfirmationFailed wrapper: either the one passed as information
	to Confirm, or the one that Confirm rejected, as returned by the helper functions *)
unwrapFailure[fail_Failure] := Replace[
	fail["Information"],
	Except[_Failure] :> Replace[fail["Expression"], Except[_Failure] :> fail]
];

failure[tag_String, params_List] := Failure[tag, <|
	"MessageTemplate" :> MessageName[PacletObjectLoad, tag],
	"MessageParameters" -> params
|>];

End[] (* End Private Context *)

EndPackage[]
