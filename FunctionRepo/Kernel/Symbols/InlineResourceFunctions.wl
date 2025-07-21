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
	funNames = DeleteDuplicates[
		Cases[qbody, HoldPattern[ResourceFunction[s_String, o : OptionsPattern[]]] :> {s, o}, Infinity, Heads -> True]
	];
	funSymbols = Select[
		AssociationMap[Apply[ResourceFunction[#1, "Function", ##2]&], funNames],
		MatchQ[_Symbol]
	];

	If[ funSymbols =!= <||>,
		newqbody //= ReplaceAll[{
			HoldPattern[ResourceFunction[s_String, o : OptionsPattern[]]] :> With[{
				fun = funSymbols[{s, o}]
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
