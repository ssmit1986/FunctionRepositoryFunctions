(* Wolfram Language Package *)

BeginPackage["FunctionRepo`AssociationTemplate`", {"FunctionRepo`", "GeneralUtilities`"}]
(* Exported symbols added here with SymbolName::usage *)
GeneralUtilities`SetUsage[AssociationTemplate, "AssociationTemplate[assoc$] creates an association in which \
TemplateObjects can be specified with TemplateSlots that query the Association itself."];

Begin["`Private`"] (* Begin Private Context *)

acyclicDependencyQ[refAssoc_?AssociationQ] := With[{
	graph = Graph[
		Flatten @ KeyValueMap[
			Thread[DirectedEdge[##]]&,
			refAssoc
		]
	]
},
	TrueQ @ And[
		LoopFreeGraphQ[graph],
		AcyclicGraphQ[graph]
	]
];
acyclicDependencyQ[_] := False;

packIfSmaller[list_] := With[{
	packedList = Developer`ToPackedArray[list]
},
	If[ ByteCount[packedList] < ByteCount[list],
		packedList,
		list
	]
];

findRefs[fun_Function] := DeleteDuplicates[
	Cases[
		(* Delete inner functions with slots *)
		ReplaceAll[Hold @@ fun, Function[_] | Function[Null, __] -> Null], 
		Verbatim[Slot][slot_String] | Verbatim[Slot][1][slot_String] :> slot, 
		Infinity,
		Heads -> True
	]
];

findRefs[template_] := DeleteDuplicates[
	Cases[template, 
		TemplateSlot[slot_String] :> slot, 
		Infinity,
		Heads -> True
	]
];

SetAttributes[processTemplate, HoldAll];
processTemplate[s_String] := StringTemplate[s];
processTemplate[expr : Except[_TemplateExpression| _TemplateObject | Function[_] | Function[Null, __]]] := TemplateExpression[expr];
processTemplate[other_] := other;

evaluateAssoc[assoc_] := AssociationThread[Keys[assoc], Values[assoc]];

AssociationTemplate[rules : {__Rule}] := AssociationTemplate[Association @ rules];

AssociationTemplate[] := AssociationTemplate[<||>];
AssociationTemplate[<||>] := AssociationTemplate[<||>, <||>, <||>, {}, ""];

AssociationTemplate[assoc_?AssociationQ] /; AllTrue[Keys[assoc], StringQ] := Module[{
	splitAssoc = Lookup[
		GroupBy[Normal @ assoc, Head],
		{Rule, RuleDelayed},
		{}
	],
	refs,
	posIndex = First /@ PositionIndex[Keys[assoc]]
},
	splitAssoc[[1]] = Association[splitAssoc[[1]]];
	splitAssoc[[2]] = AssociationThread[
		Keys[splitAssoc[[2]]] :> Evaluate @ Values[splitAssoc[[2]], processTemplate]
	];
	(* Find all dependent template slots that need to be extracted to calculate the templated one *)
	refs = evaluateAssoc[findRefs /@ splitAssoc[[2]]];
	If[ acyclicDependencyQ[refs]
		,
		AssociationTemplate[
			Sequence @@ splitAssoc,
			evaluateAssoc @ Map[packIfSmaller @ Lookup[posIndex, #]&, refs],
			Keys[assoc] (* Store the original keys to be able to re-assemble the Association in the correct order *),
			CreateUUID[]
		]
		,
		Failure["ConstructionFailure",
			<|
				"MessageTemplate" -> "Cyclic dependency of keys detected."
			|>
		]
	]
];

AssociationTemplate[assoc_?AssociationQ] := Failure["ConstructionFailure",
	<|
		"MessageTemplate" -> "Key(s) `Keys` are not Strings.",
		"MessageParameters" -> <|
			"Keys" -> Select[Keys[assoc], !StringQ[#]&]
		|>
	|>
];

AssociationTemplate[_] := Failure["ConstructionFailure",
	<|"MessageTemplate" -> "Can only construct AssociationTemplate from an Association with String keys."|>
];

$queryCache = <||>;

SetAttributes[cacheBlock, HoldAll];
cacheBlock[expr_] := Block[{$queryCache = <||>}, expr];

(* 
	This caching method is has less overhead than ordinary memoization when the cache needs to be cleared again
*)
cachedQuery[AssociationTemplate[_, _, _, _, id_], key_String, ___] /; KeyExistsQ[$queryCache, {id, key}] := $queryCache[{id, key}];

cachedQuery[AssociationTemplate[_, _, _, _, id_], key_String, extraVals_] /; KeyExistsQ[extraVals, key] := (
	$queryCache[{id, key}] = extraVals[key]
);

cachedQuery[
	sAssoc : AssociationTemplate[data_, _, _, _, id_],
	key_String,
	rest___
] /; KeyExistsQ[data, key] := ($queryCache[{id, key}] = data[key]);

cachedQuery[
	sAssoc : AssociationTemplate[data_, exprs_, refs_, keyList_, id_],
	key_String,
	rest___
] /; KeyExistsQ[exprs, key] := (
	$queryCache[{id, key}] = With[{
		templateVals = With[{
			keys = keyList[[refs[key]]]
		},
			Join[
				data,
				AssociationThread[keys, Map[cachedQuery[sAssoc, #, rest]&, keys]],
				rest
			]
		],
		template = exprs[key]
	},
		If[ Head[template] === Function,
			template @ templateVals,
			TemplateApply[template, templateVals]
		]
	]
);
cachedQuery[AssociationTemplate[_, _, _, _, id_], key_, ___] := (
	$queryCache[{id, key}] = Missing["KeyAbsent", key]
);

AssociationTemplate[_, _, _, _, _][key_, extraVals_?AssociationQ] /; KeyExistsQ[extraVals, key] := extraVals[key];

AssociationTemplate[data_, _, _, _, _][key_, ___] /; KeyExistsQ[data, key] := data[key];

(sAssoc : AssociationTemplate[_, _, _, _, _])[key_, rest : Repeated[_?AssociationQ, {0, 1}]] := cacheBlock[
	cachedQuery[sAssoc, key, rest]
];

AssociationTemplate /: Normal[sAssoc : AssociationTemplate[data_, expr_, _, keys_List, _]] := KeyTake[
	Join[data, expr],
	keys
];

AssociationTemplate /: Keys[AssociationTemplate[_, _, _, keys_List, _]] := keys;

AssociationTemplate /: Values[
	sAssoc : AssociationTemplate[data_, expr_, _, keys_List, _]
] := cacheBlock[
	cachedQuery[sAssoc, #]& /@ keys
];

AssociationTemplate /: Length[AssociationTemplate[_, _, _, keys_List, _]] := Length[keys];

AssociationTemplate /: Part[sAssoc : AssociationTemplate[_, _, _, _, _], s_String] := sAssoc[s];
AssociationTemplate /: Part[
	sAssoc : AssociationTemplate[_, _, _, _, _],
	strings : {__String}
] := cacheBlock[
	AssociationThread[
		strings,
		cachedQuery[sAssoc, #]& /@ strings
	]
];

AssociationTemplate /: Part[sAssoc : AssociationTemplate[_, _, _, keys_List, _], i : _Integer | {__Integer}] := Part[
	sAssoc,
	keys[[i]]
];

AssociationTemplate /: Part[
	sAssoc : AssociationTemplate[_, _, _, keys_List, _],
	All
] := AssociationThread[keys, Values[sAssoc]];

AssociationTemplate /: Part[AssociationTemplate[_, _, _, _, _], {}] := <||>;

AssociationTemplate /: Join[
	sAssocs : Longest[__AssociationTemplate]
] /; Length[{sAssocs}] > 1 := With[{
	list = Normal /@ {sAssocs}
},
	AssociationTemplate @ Apply[Join, list]
];

AssociationTemplate /: Join[sAssoc_AssociationTemplate, assoc : Longest[__?AssociationQ]] := 
	AssociationTemplate[Join[Normal @ sAssoc, assoc]];
AssociationTemplate /: Join[assoc : Longest[__?AssociationQ], sAssoc_AssociationTemplate] :=
	AssociationTemplate[Join[assoc, Normal @ sAssoc]];

AssociationTemplate /: Append[sAssoc_AssociationTemplate, new_] :=
	AssociationTemplate[Append[Normal @ sAssoc, new]];
AssociationTemplate /: Prepend[sAssoc_AssociationTemplate, new_] :=
	AssociationTemplate[Prepend[Normal @ sAssoc, new]];

AssociationTemplate /: MakeBoxes[
	AssociationTemplate[data_?AssociationQ, expr_?AssociationQ, refs_, keys_, _], 
	form_
] := BoxForm`ArrangeSummaryBox["AssociationTemplate",
	AssociationTemplate[data, expr],
	"\[LeftAssociation]\[Ellipsis]\[RightAssociation]",
	{
		BoxForm`SummaryItem[{"Number of data keys: ", Length[Keys[data]]}],
		BoxForm`SummaryItem[{"Number of templated keys: ", Length[Keys[expr]]}],
		BoxForm`SummaryItem[{"Total keys: ", Length[keys]}]
	},
	{
		BoxForm`SummaryItem[{"Data keys: ", Short[Keys[data], 3]}],
		BoxForm`SummaryItem[{"Templated keys: ", Short[Keys[expr], 3]}]
	},
	form
];

End[] (* End Private Context *)

EndPackage[]