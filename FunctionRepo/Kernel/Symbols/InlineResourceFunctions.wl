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
	qbody = GU`Quoted[body],
	newqbody,
	funNames,
	funSymbols
},
	newqbody = qbody;
	funNames = Cases[qbody, HoldPattern[ResourceFunction[s_String]] :> s, Infinity, Heads -> True];
	funSymbols = Select[
		AssociationMap[ResourceFunction[#, "Function"]&, funNames],
		MatchQ[_Symbol]
	];
	funNames = Keys @ funSymbols;

	If[ funNames =!= {},
		newqbody //= ReplaceAll[{
			HoldPattern[ResourceFunction[s_String]] :> With[{
				fun = funSymbols[s]
			},
				fun /; MatchQ[fun, _Symbol]
			]
		}];
		newqbody
		,
		qbody
	]
];

End[] (* End Private Context *)

EndPackage[]
