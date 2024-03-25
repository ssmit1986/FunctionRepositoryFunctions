(* Wolfram Language Package *)

BeginPackage["FunctionRepo`FlattenFeatures`", {"FunctionRepo`"}]
(* Exported symbols added here with SymbolName::usage *)
GeneralUtilities`SetUsage[FlattenFeatures,
	"FlattenFeatures[{assoc$1, $$}] converts a list of associations with nested data into a flat structure suitable for Classify and Predict."
];

Begin["`Private`"] (* Begin Private Context *)

flatDataQ[list_] := MatchQ[list, {___Association?AssociationQ}?(AllTrue[#, FreeQ[_Association], 2]&)];

FlattenFeatures[l : {___Association?AssociationQ}] := Module[{
	data = l,
	pos,
	tag, catchTag
},
	Catch[
		Enclose[
			If[ flatDataQ[data], Throw[data, catchTag]];
			tag[a_Association] := tag /@ a;
			data = pushListsDown /@ data;
			pos = Position[Map[tag, data, {2}], _tag, Heads -> False];
			data = Map[
				AssociationThread[
					Replace[#[[All, 2 ;;]], Key[k_] :> k, {2}],
					Extract[data, #]
				]&,
				GatherBy[pos, First]
			];
			ConfirmBy[data, flatDataQ]
		],
		catchTag
	]
];

assocTranspose = GeneralUtilities`AssociationTranspose;

pushListsDown[assoc_Association] := pushListsDown /@ assoc;
pushListsDown[{}] := {};
pushListsDown[list : {__Association}] := pushListsDown @ assocTranspose[list];
pushListsDown[list : {__List}] := Replace[
	Map[pushListsDown, list],
	assocs : {__Association} :> pushListsDown[assocs]
];
pushListsDown[other_] := other;


End[] (* End Private Context *)

EndPackage[]
