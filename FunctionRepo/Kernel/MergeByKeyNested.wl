(* Wolfram Language Package *)

BeginPackage["FunctionRepo`MergeByKeyNested`", {"FunctionRepo`", "GeneralUtilities`"}]
(* Exported symbols added here with SymbolName::usage *)
FunctionRepo`MergeByKey; (* Make sure MergeByKey doesn't accidentally get created in the wrong place *)

GeneralUtilities`SetUsage[MergeByKeyNested, "MergeByKeyNested[{assoc$1, assoc$2, $$}, {key$1 -> f$1, $$}, default$] merges the assocations like MergeByKey, but continues merging if the \
merged values are associations again."];

Begin["`Private`"] (* Begin Private Context *) 

associationOfAssociationQ[assoc_]:= TrueQ @ And[
	AssociationQ[assoc],
	Length[assoc] > 0,
	AllTrue[assoc, AssociationQ]
];

MergeByKeyNested[rules : {___Rule}, default : _ : Identity][data : {___?AssociationQ}] := MergeByKeyNested[data, rules, default];

MergeByKeyNested[{<||>...}, {___Rule}, Repeated[_, {0, 1}]] := <||>;

MergeByKeyNested[data : {__?AssociationQ}, funs : {___Rule}] := MergeByKeyNested[data, funs, Identity];

MergeByKeyNested[
	data : {__Association?associationOfAssociationQ},
	funs : {___Rule},
	default_
] := Merge[
	data,
	MergeByKeyNested[#, funs, default]&
];

MergeByKeyNested[
	data : {__Association?AssociationQ},
	funs : {___Rule},
	default_
] := MergeByKey[data,funs, default];

End[] (* End Private Context *)

EndPackage[]

