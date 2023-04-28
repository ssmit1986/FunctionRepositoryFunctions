(* Wolfram Language Package *)

BeginPackage["FunctionRepo`FlattenFailure`", {"FunctionRepo`"}]
(* Exported symbols added here with SymbolName::usage *)
GeneralUtilities`SetUsage[FlattenFailure,
	"FlattenFailure[fail$] extracts information from nested failures and puts all the info in the outer failure object."
];

Begin["`Private`"] (* Begin Private Context *)

FlattenFailure[failIn_Failure?FailureQ, h : _ : "Information"] := With[{
	fail = makeMessagesReadable @ failIn,
	handler = Replace[h, s_String :> (#[s]&)]
},
	With[{
		allInfo = deleteDuplicatesFromEnd @ Flatten @ Last @ Reap[
			ReplaceRepeated[
				fail,
				f_Failure :> (
					Sow[handler[f], failureInfo];
					f["Expression"]
				)
			],
			failureInfo
		]
	},
		appendToFailure[fail, <|"Information" -> allInfo|>]
	]
];
FlattenFailure[other_] := other;

deleteDuplicatesFromEnd[list_] := Reverse @ DeleteDuplicates[Reverse[list]];

appendToFailure[Failure[tag_, assoc_?AssociationQ, rest___], append_] := Failure[tag, Append[assoc, append], rest];

elidedFailure[f_] := StringForm["Failure[`1`, `2`]", f["Tag"], Skeleton[1]];

SetAttributes[hasFailureQ, HoldAllComplete];
hasFailureQ[expr_] := !FreeQ[Unevaluated[expr], _Failure];

makeMessagesReadable[expr_] := ReplaceRepeated[
	expr,
	Failure[tag_,
		Association[
			fst2___,
			"MessageParameters" :> list_?hasFailureQ,
			rest2___
		],
		rest1___
	] :> With[{
		newRule = Replace[
			HoldComplete[list] //. f_Failure :> With[{el = elidedFailure[f]}, el /; True],
			HoldComplete[l_] :> ("MessageParameters" :> l)
		]
	},
		Failure[tag,
			Association[
				fst2,
				newRule,
				rest2
			],
			rest1
		] /; True
	]
];

End[] (* End Private Context *)

EndPackage[]
