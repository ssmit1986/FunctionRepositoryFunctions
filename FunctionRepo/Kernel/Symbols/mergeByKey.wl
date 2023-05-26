(* Wolfram Language Package *)

BeginPackage["FunctionRepo`MergeByKey`", {"FunctionRepo`", "GeneralUtilities`"}]
(* Exported symbols added here with SymbolName::usage *)
GeneralUtilities`SetUsage[MergeByKey, "MergeByKey[{assoc$1, assoc$2, $$}, {key$1 -> fun$1, key$2 -> fun$2, $$}, default$] merges the assocations using different merge functions for different keys."];

Begin["`Private`"] (* Begin Private Context *) 

MergeByKey[rules : {___Rule}, default : _ : Identity][data : {___?AssociationQ}] := MergeByKey[data, rules, default];

MergeByKey[{<||>...}, {___Rule}, Repeated[_, {0, 1}]] := <||>;

MergeByKey[data : {__?AssociationQ}, rules : {___Rule}, default : _ : Identity] := Module[{
	(* Unique symbol that is used for identifying where the undefined keys were after transposing the association *)
	missingToken,
	assoc,
	keys,
	queryRules,
	mergeRules = Replace[
		Flatten @ Replace[
			rules,
			Verbatim[Rule][lst_List, fun_] :> Thread[lst -> fun],
			{1}
		],
		Verbatim[Rule][Key[k_], fun_] :> k -> fun,
		{1}
	],
	keysSameQ = SameQ @@ Keys[data]
},
	If[ keysSameQ, (* Avoid KeyUnion if it's not necessary *)
		assoc = data,
		assoc = KeyUnion[DeleteCases[data, <||>], missingToken&]
	];
	keys = Keys[First @ assoc];
	(* This is essentially how GeneralUtilities`AssociationTranspose works *)
	assoc = AssociationThread[keys,
		If[ keysSameQ,
			Transpose @ Values[assoc],
			DeleteCases[Transpose @ Values[assoc], missingToken, {2}]
		]
	];
	keys = Key /@ keys;
	queryRules = DeleteCases[
		Thread[
			keys -> Lookup[mergeRules, keys, default]
		],
		_ -> Identity
	];
	If[ MatchQ[queryRules, {__Rule}]
		,
		Query[queryRules] @ assoc
		,
		assoc
	]
];

End[] (* End Private Context *)

EndPackage[]

