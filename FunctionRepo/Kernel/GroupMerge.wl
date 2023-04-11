(* Wolfram Language Package *)

BeginPackage["FunctionRepo`GroupMerge`", {"FunctionRepo`", "GeneralUtilities`"}]
(* Exported symbols added here with SymbolName::usage *)
GeneralUtilities`SetUsage[GroupMerge, "GroupMerge[{assoc$1, assoc$2, $$}, {key$1, key$2, $$}, {keyOut$1 -> f$1, keyOut$2 -> f$2, $$}] returns a list of associations obtained by grouping the input data by key$1, key$2, $$ and then merging that grouped data using f$1, f$2, $$."
];

Begin["`Private`"] (* Begin Private Context *) 

$partPatt = _Key | _String | Integer;
$assocPatt = _Association?AssociationQ;
$assocListPatt = {__Association?AssociationQ};

GroupMerge[{}, _, _] := {};
GroupMerge[groupSpec_, mergeSpec_][data_] := GroupMerge[data, groupSpec, mergeSpec];

GroupMerge[dat_, groupSpec_, mergeSpec_] := Module[{
	datasetQ,
	data = dat,
	groupKeys = groupSpec,
	groupFun,
	mergeFun = mergeSpec
},
	Enclose[
		datasetQ = Head[data] === Dataset;
		If[ datasetQ, data = Normal[data]];
		ConfirmMatch[
			data,
			$assocListPatt
		];
		groupKeys = DeleteDuplicates @ Flatten[{groupSpec}];
		ConfirmMatch[
			groupKeys,
			{($partPatt | Rule[$partPatt, _])..}
		];
		
		groupFun = With[{
			list = Replace[groupKeys,
				{
					r_Rule :> r[[2]],
					p_ :> Function[Part[#, p]]
				},
				{1}
			]
		},
			Function[Through[list[#]]]
		];
		groupKeys = Replace[
			Replace[groupKeys, r_Rule :> r[[1]], {1}],
			Key[k_] :> k,
			{1}
		];
		mergeFun = mergeData[
			Replace[
				Flatten @ {mergeFun},
				Automatic :> Merge[automaticMergeFun],
				{1}
			]
		];
		
		data = GroupBy[
			data,
			groupFun -> KeyDrop[groupKeys],
			ConfirmMatch[mergeFun[#], $assocPatt]&
		];
		data = ConfirmMatch[
			KeyValueMap[
				Join[AssociationThread[groupKeys, #1], #2]&,
				data
			],
			$assocListPatt
		];
		If[ datasetQ, data = Dataset[data]];

		data
	]
];

automaticMergeFun = Replace[
	{
		{el_} :> el,
		list : {__} /; SameQ @@ list :> First[list],
		_ :> Missing["NotAvailable"]
	}
];

mergeData[mergeSpec_][data_] := Association @ Map[
	Replace[
		{
			Verbatim[Rule][key_, fun_] :> Rule[key, fun @ data],
			fun_ :> fun @ data
		}
	],
	mergeSpec
];

End[] (* End Private Context *)

EndPackage[]