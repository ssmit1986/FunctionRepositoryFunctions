(* Wolfram Language Package *)

BeginPackage["FunctionRepo`GroupCases`", {"FunctionRepo`", "GeneralUtilities`"}]
(* Exported symbols added here with SymbolName::usage *)
GeneralUtilities`SetUsage[GroupCases, "GroupCases[{el$1, el$2, $$}, patt$] groups elements el$i by whether or not the match patt$ and creates an association <|patt$ -> $$, _ -> $$|>
GroupCases[list$, patt$ :> rhs$] applies a transformation to matched elements.
GroupCases[list$, {spec$1, spec$2, $$}] groups using multiple patterns.
GroupCases[list$, <|key$1 -> spec$1, $$}] labels the returned values with key$i.
GroupCases[spec$] represents an operator form of GroupCases."
];

Begin["`Private`"] (* Begin Private Context *) 

GroupCases[spec_][list_List] := GroupCases[list, spec];

GroupCases[{}, spec_] := With[{
	keys = Replace[patternList[spec], assoc_?AssociationQ :> Keys[assoc]]
},
	AssociationThread[keys, ConstantArray[{}, Length[keys]]]
];
GroupCases[list_List, Verbatim[_]] := <|_ -> list|>;
GroupCases[list_List, spec_] := Module[{
	pattSpec = patternList[spec],
	rules,
	results,
	sowTag
},
	rules = MapIndexed[
		makeRule[#1, sowTag @@ #2]&,
		Replace[pattSpec, assoc_?AssociationQ :> Values[assoc]]
	];
	results = Last @ Reap[
		Replace[list, rules, {1}],
		Array[sowTag, Length[rules]]
	];
	AssociationThread[
		Replace[pattSpec, assoc_?AssociationQ :> Keys[assoc]],
		Join @@@ results
	]
];

patternList[lst_List] /; !MemberQ[lst, Verbatim[_]] := Append[lst, _];
patternList[assoc_?AssociationQ] /; !MemberQ[assoc, Verbatim[_]] := Append[assoc, _ -> _];
patternList[spec : _List | _?AssociationQ] := spec;
patternList[Verbatim[_]] := {_};
patternList[patt_] := {patt, _};

makeRule[
	(rule : (Rule | RuleDelayed))[patt_, val_],
	tag_
] /; FreeQ[Unevaluated[val], Condition | RuleCondition] := match : patt :> Sow[val, tag];

makeRule[(rule : (Rule | RuleDelayed))[patt_, val_], tag_] := match_ :> With[{
	try = Replace[match, {rule[patt, val], _ :> tag}]
},
	Sow[try, tag] /; try =!= tag
];
makeRule[patt_, tag_] := match : patt :> Sow[match, tag];

End[] (* End Private Context *)

EndPackage[]