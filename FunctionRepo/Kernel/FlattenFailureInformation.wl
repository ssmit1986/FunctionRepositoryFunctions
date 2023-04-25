(* Wolfram Language Package *)

BeginPackage["FunctionRepo`FlattenFailureInformation`", {"FunctionRepo`"}]
(* Exported symbols added here with SymbolName::usage *)
GeneralUtilities`SetUsage[FlattenFailureInformation,
	"FlattenFailureInformation[fail$] extracts information from nested failures and puts all the info in the outer failure object."
];

Begin["`Private`"] (* Begin Private Context *)

FlattenFailureInformation[fail_Failure?FailureQ] := With[{
	allInfo = Apply[Join] @ Last @ Reap[
		ReplaceRepeated[
			fail,
			f_Failure :> (
				Sow[f["Information"], failureInfo];
				f["Expression"]
			)
		],
		failureInfo
	]
},
	appendToFailure[fail, <|"Information" -> allInfo|>]
];
FlattenFailureInformation[other_] := other;

appendToFailure[Failure[tag_, assoc_?AssociationQ, rest___], append_] := Failure[tag, Append[assoc, append], rest];

End[] (* End Private Context *)

EndPackage[]
