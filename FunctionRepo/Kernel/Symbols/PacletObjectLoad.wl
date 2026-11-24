(* Wolfram Language Package *)

BeginPackage["FunctionRepo`PacletObjectLoad`", {"FunctionRepo`"}]
(* Exported symbols added here with SymbolName::usage *)
GeneralUtilities`SetUsage[PacletObjectLoad, "
PacletObjectLoad[obj$] loads the paclet object obj$ with Get, even if other versions of the same paclet are installed.
PacletObjectLoad[spec$, opts$$] finds paclets with PacletFind[spec$, opts$$] and loads the result if it is a single paclet.
* All other enabled paclets with the same name or the same context are disabled temporarily while the first context of obj$ is loaded. \
They are enabled again afterwards, also when loading fails or is aborted.
* obj$ must be enabled and installed, or PacletObjectLoad fails.
* Before loading, FindFile must resolve the context to a file inside the location of obj$, or PacletObjectLoad fails.
* The return value is the PacletObject that was loaded, or a Failure."
];


Begin["`Private`"] (* Begin Private Context *)


PacletObjectLoad::notfound = "Paclet `1` version `2` at `3` is not among the enabled paclets found by PacletFind.";
PacletObjectLoad::noctx = "Paclet `1` does not define a valid context to load.";
PacletObjectLoad::nofile = "FindFile could not find a file for context `1`.";
PacletObjectLoad::wrongfile = "Context `1` resolves to `2`, which is not inside the paclet location `3`.";
PacletObjectLoad::getfail ="Get failed to load context `1`.";
PacletObjectLoad::notunique = "PacletFind found `1` paclets instead of exactly one.";

PacletObjectLoad[obj_PacletObject] := Enclose[
	Module[{
		name = obj["Name"],
		location = obj["Location"],
		context,
		found,
		others,
		file,
		disabled = {}
	},
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
		WithCleanup[
			(* PacletDisable returns only the paclets it actually disabled *)
			disabled = Flatten[PacletDisable /@ others]
			,
			(* Make sure Get loads from obj and not from another paclet or $Path entry that provides the same context *)
			file = ConfirmBy[FindFile[context], StringQ, failure["nofile", {context}]];
			ConfirmAssert[
				fileInDirectoryQ[file, location],
				failure["wrongfile", {context, file, location}]
			];
			Confirm[Get[file], failure["getfail", {context}]]
			,
			(* Enable the exact objects from PacletFind: a name could resolve to a different version *)
			Scan[PacletEnable, disabled]
		];
		obj
	],
	unwrapFailure
];

PacletObjectLoad[spec : Except[_PacletObject], opts___] := Replace[
	PacletFind[spec, opts],
	{
		{paclet_PacletObject} :> PacletObjectLoad[paclet],
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

(* Return the specific Failure passed as information to Confirm, instead of the generic ConfirmationFailed wrapper *)
unwrapFailure[fail_Failure] := Replace[fail["Information"], Except[_Failure] :> fail];

failure[tag_String, params_List] := Failure[tag, <|
	"MessageTemplate" :> MessageName[PacletObjectLoad, tag],
	"MessageParameters" -> params
|>];

End[] (* End Private Context *)

EndPackage[]
