(* Wolfram Language Package *)

BeginPackage["FunctionRepo`AssociationSet`", {"FunctionRepo`"}]
(* Exported symbols added here with SymbolName::usage *)
GeneralUtilities`SetUsage[AssociationSet, 
	"AssociationSet[assoc$, {key$1, key$2, $$}, value$] sets the value of the key(s) in the association to value$."
];

Begin["`Private`"] (* Begin Private Context *) 

SetAttributes[AssociationSet, HoldFirst];

AssociationSet[expr_][val_] := AssociationSet[expr, val];
AssociationSet[expr_][seq_, val_] := AssociationSet[expr, seq, val];
AssociationSet[sym_Symbol?AssociationQ[keySeq__], value_] := iAssociationSet[sym, {keySeq}, value];
AssociationSet[sym_Symbol?AssociationQ, {keySeq__}, value_] := iAssociationSet[sym, {keySeq}, value];

SetAttributes[iAssociationSet, HoldFirst];

iAssociationSet[sym_, {key_}, value_] := Set[sym[key], value];

iAssociationSet[sym_, {keySeq__, key_}, value_] := 
	If[
		AssociationQ[sym[keySeq]],
		Set[sym[keySeq, key], value],
		iAssociationSet[sym, {keySeq}, <|key -> value|>]
	];

End[] (* End Private Context *)

EndPackage[]