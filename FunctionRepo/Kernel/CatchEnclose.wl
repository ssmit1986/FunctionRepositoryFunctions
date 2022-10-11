(* Wolfram Language Package *)

BeginPackage["FunctionRepo`CatchEnclose`", {"FunctionRepo`"}]
(* Exported symbols added here with SymbolName::usage *)
GeneralUtilities`SetUsage[CatchEnclose,
	"CatchEnclose[expr$] acts like Enclose, but it also catches untagged Throws lexically.
CatchEnclose[expr$, f$] applies f$ to any value thrown by a lexically enclosed Throw.
CatchEnclose[expr$, f$, g$] applies g$ to any failure caught by Enclose."
];

Begin["`Private`"] (* Begin Private Context *) 

SetAttributes[CatchEnclose, HoldFirst];

CatchEnclose[expr_] := CatchEnclose[expr, Function[#], Function[#]];

CatchEnclose[expr_, f_] := CatchEnclose[expr, f, Function[#]];

CatchEnclose[expr_, catchFun_, failFun_] := Module[{
	catchEncloseTag
},
	Replace[
		ReplaceAll[
			HoldComplete[expr],
			HoldPattern[Throw[throw_]] :> Throw[throw, catchEncloseTag]
		],
		HoldComplete[expr2_] :> Enclose[
			Catch[expr2, catchEncloseTag, catchFun],
			failFun
		]
	]
];

End[] (* End Private Context *)

EndPackage[]