(* Wolfram Language Package *)

BeginPackage["FunctionRepo`GroupMerge`", {"FunctionRepo`", "GeneralUtilities`"}]
(* Exported symbols added here with SymbolName::usage *)
GeneralUtilities`SetUsage[GroupMerge, "GroupMerge[{assoc$1, assoc$2, $$}, {key$1, key$2, $$}, {keyOut$1 -> f$1, keyOut$2 -> f$2, $$}] returns a list of associations obtained by grouping the input data by key$1, key$2, $$ and then merging that grouped data using f$1, f$2, $$."
];

Begin["`Private`"] (* Begin Private Context *) 

GroupMerge[{}, _, _] := {};
GroupMerge[groupSpec_, mergeSpec_][data_] := GroupMerge[data, groupSpec, mergeSpec];

$partPatt = _Key | _String | Integer;

GroupMerge[dat_, groupSpec_, mergeSpec_] := Module[{
	datasetQ,
	data = dat,
	groupKeys = groupSpec,
	groupFun,
	mergeAssoc = mergeSpec,
	mergeFun
},
	Enclose[
		datasetQ = Head[data] === Dataset;
		If[ datasetQ, data = Normal[data]];
		ConfirmMatch[
			data,
			 {__Association?AssociationQ}
		];
		groupKeys = DeleteDuplicates @ Flatten[{groupSpec}];
		ConfirmMatch[
			groupKeys,
			{($partPatt | Rule[$partPatt, _])..}
		];
		mergeAssoc = ConfirmBy[Association[mergeAssoc], AssociationQ];
		
		groupFun = Replace[groupKeys,
			{
				r_Rule :> r[[2]],
				p_ :> Function[Part[#, p]]
			},
			{1}
		];
		groupKeys = Replace[
			Replace[groupKeys, r_Rule :> r[[1]], {1}],
			Key[k_] :> k,
			{1}
		];
		mergeFun = Function[list, Function[# @ list] /@ mergeAssoc];
		
		data = GroupBy[data, groupFun, mergeFun];
		data = ConfirmMatch[
			Flatten @ Last @ Reap[
				MapIndexed[
					Function[{assoc, pos},
						Sow[
							Join[
								AssociationThread[groupKeys, pos[[All, 1]]],
								assoc
							],
							sowGroupMerge
						]
					],
					data,
					{Length[groupFun]}
				],
				sowGroupMerge
			],
			{__Association?AssociationQ}
		];
		If[ datasetQ, data = Dataset[data]];

		data
	]
]

End[] (* End Private Context *)

EndPackage[]