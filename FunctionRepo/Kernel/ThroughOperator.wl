(* Wolfram Language Package *)

BeginPackage["FunctionRepo`ThroughOperator`", {"FunctionRepo`"}]
(* Exported symbols added here with SymbolName::usage *)
GeneralUtilities`SetUsage[ThroughOperator, 
	"ThroughOperator[{f$1, f$2, $$}] is an operator form for Through that can be applied to arguments."
];

Begin["`Private`"] (* Begin Private Context *) 

ThroughOperator[assoc_?AssociationQ][args___] := AssociationThread[
	Keys[assoc],
	ThroughOperator[Values[assoc]][args]
];
ThroughOperator[{elements___}, f_][args___] := Through @ Unevaluated[f[elements][args]];
ThroughOperator[expr_List][args___] := Through @ Unevaluated[expr[args]];
ThroughOperator[op__][___] := $Failed;

End[] (* End Private Context *)

EndPackage[]