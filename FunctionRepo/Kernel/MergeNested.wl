(* Wolfram Language Package *)

BeginPackage["FunctionRepo`MergeNested`", {"FunctionRepo`", "GeneralUtilities`"}]
(* Exported symbols added here with SymbolName::usage *)
GeneralUtilities`SetUsage[MergeNested, "MergeNested[{assoc$1, assoc$2, $$}, f$] merges the assocations like Merge, but continues merging if the\
merged values are associations again."];

Begin["`Private`"] (* Begin Private Context *) 

MergeNested[f_][data_] := MergeNested[data, f];

MergeNested[{<||>...}, _] := <||>;
MergeNested[data : {__?AssociationQ}, f_] := Merge[data, MergeNested[f]];
MergeNested[data_, f_]:= f[data];

End[] (* End Private Context *)

EndPackage[]