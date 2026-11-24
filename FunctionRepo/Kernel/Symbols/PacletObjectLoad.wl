(* Wolfram Language Package *)

BeginPackage["FunctionRepo`PacletObjectLoad`", {"FunctionRepo`"}]
(* Exported symbols added here with SymbolName::usage *)
GeneralUtilities`SetUsage[PacletObjectLoad, "
PacletObjectLoad[obj$] loads the paclet object obj$ with Get, even if other versions of the same paclet are installed.
PacletObjectLoad[spec$, opts$$] finds paclets with PacletFind[spec$, opts$$] and loads the result if it is a single paclet.
* All other enabled paclets with the same name are disabled temporarily while the first context of obj$ is loaded. \
They are enabled again afterwards, also when loading fails or is aborted.
* obj$ must be enabled and installed, or PacletObjectLoad fails.
* The return value is the PacletObject that was loaded, or a Failure."
];

PacletObjectLoad::notfound = "Paclet `1` version `2` at `3` is not among the enabled paclets found by PacletFind.";
PacletObjectLoad::noctx = "Paclet `1` does not define a valid context to load.";
PacletObjectLoad::getfail = "Get failed to load context `1`.";
PacletObjectLoad::notunique = "PacletFind found `1` paclets instead of exactly one.";

Begin["`Private`"] (* Begin Private Context *)

PacletObjectLoad[obj_PacletObject] := Enclose[
	Module[{
		name = obj["Name"],
		location = obj["Location"],
		context,
		found,
		others,
		disabled = {}
	},
		found = ConfirmMatch[PacletFind[name], {___PacletObject}];
		ConfirmAssert[
			MemberQ[Comap[found, "Location"], location],
			failure["notfound", {name, obj["Version"], location}]
		];
		context = ConfirmBy[
			First[Flatten[{obj["Context"]}], Missing["NoContext"]],
			contextQ,
			failure["noctx", {name}]
		];
		others = Select[found, #["Location"] =!= location &];
		WithCleanup[
			(* PacletDisable returns only the paclets it actually disabled *)
			disabled = Flatten[PacletDisable /@ others]
			,
			Confirm[Get[context], failure["getfail", {context}]]
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

(* Return the specific Failure passed as information to Confirm, instead of the generic ConfirmationFailed wrapper *)
unwrapFailure[fail_Failure] := Replace[fail["Information"], Except[_Failure] :> fail];

failure[tag_String, params_List] := Failure[tag, <|
	"MessageTemplate" :> MessageName[PacletObjectLoad, tag],
	"MessageParameters" -> params
|>];

End[] (* End Private Context *)

EndPackage[]
