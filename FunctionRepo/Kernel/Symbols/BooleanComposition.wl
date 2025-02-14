(* Wolfram Language Package *)

BeginPackage["FunctionRepo`BooleanComposition`", {"FunctionRepo`", "GeneralUtilities`"}]
(* Exported symbols added here with SymbolName::usage *)
GeneralUtilities`SetUsage[BooleanFunction,
	"BooleanComposition[fun$1 && ! fun$2 || $$] converts a boolean combination of functions into a function that can be directly applied to arguments."
];

Begin["`Private`"] (* Begin Private Context *) 

booleHead = HoldPattern @ Alternatives[
	And, Or, Not, Nor, Nand, Xor, Xnor, Majority, Implies,Equivalent,
	BooleanCountingFunction[__], BooleanConsecutiveFunction[__], BooleanFunction[__]
];

BooleanComposition[expr_] := Replace[expr, e : booleHead[__] :> Comap[e], {0, Infinity}, Heads -> True];

End[] (* End Private Context *)

EndPackage[]
