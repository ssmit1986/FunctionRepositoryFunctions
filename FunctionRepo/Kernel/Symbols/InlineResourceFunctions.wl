(* Wolfram Language Package *)

BeginPackage["FunctionRepo`InlineResourceFunctions`", {"FunctionRepo`"}]

Needs["GeneralUtilities`" -> "GU`"]

(* Exported symbols added here with SymbolName::usage *)
GU`SetUsage[InlineResourceFunctions,
	"InlineResourceFunctions[expr$] is a macro that replaces ResourceFunction['name$'] with ResourceFunction['name$', \"Function\"]."
];

Begin["`Private`"] (* Begin Private Context *)

GU`DefineMacro[
	InlineResourceFunctions,
	InlineResourceFunctions[body_] := mInlineResourceFunctions[body]
];

SetAttributes[InlineResourceFunctions, HoldAllComplete];

InlineResourceFunctions[body_] := GU`MacroEvaluate[InlineResourceFunctions[body]];

SetAttributes[mInlineResourceFunctions, HoldAllComplete];
mInlineResourceFunctions[body_] := Module[{
	qbody = GU`Quoted[body]
},
	qbody //= ReplaceAll[{
		HoldPattern[ResourceFunction[s_String, o : OptionsPattern[]]] :> With[{
			fun = ResourceFunction[s, "Function", o]
		},
			fun /; MatchQ[fun, _Symbol]
		]
	}];
	qbody
];

End[] (* End Private Context *)

EndPackage[]
