(* Wolfram Language Package *)

BeginPackage["FunctionRepo`GroupTotalBy`", {"FunctionRepo`"}]

GeneralUtilities`SetUsage[GroupTotalBy,
	"GroupTotalBy[data$, groupKey$, selectionKey$ -> selectionValue$, aggKey$ -> aggFun$] computes the aggregation of aggKey$ in data$ using aggFun$,\
grouped by groupKey$ and after having applied selection criteria. The returned data is a list of Associations.
GroupTotalBy[$$, totalKey$] uses Total as the default aggregation function.
GroupTotalBy[$$, selectionKey$ -> {val$1, val$2, $$}, $$] performs the operation over several different selection criteria.
GroupTotalBy[$$][data$] uses GroupTotalBy in operator form."
];

Begin["`Private`"] (* Begin Private Context *) 

GroupTotalBy[dataset_Dataset, rest : Repeated[_, {3}]] := Replace[
	GroupTotalBy[Normal[dataset], rest],
	list_List :> Dataset[list]
];

(* Make Total the default aggregator *)
GroupTotalBy[dat_, splitSpec_, selectionSpec_, totalKey_String] := 
	GroupTotalBy[dat, splitSpec, selectionSpec, totalKey -> Total];

(* Operator form *)
GroupTotalBy[splitSpec_, selectionSpec_, aggSpec_][data_] :=
	GroupTotalBy[data, splitSpec, selectionSpec, aggSpec];

GroupTotalBy[
	dat : {__?AssociationQ},
	splitKey_String,
	selectionKey_String -> selectionPatt_,
	aggKey_String -> agg_
] := Module[{
	data = dat,
	selectionFun,
	aggFun
},
	Enclose[
		selectionFun = groupTotalBySelector[selectionKey -> selectionPatt];
		data = ConfirmBy[
			GroupBy[
				data,
				Key[splitKey],
				Association[selectionFun[#]]&
			],
			AssociationQ,
			"Grouping failed"
		];
		data = Flatten @ KeyValueMap[
			Function[{splitKeyVal, selectionAssoc},
				With[{
					aggData = ConfirmBy[
						groupTotalByAggregator[aggKey, agg][selectionAssoc],
						Function[AssociationQ[#] && FreeQ[#, _?FailureQ]],
						"Aggregation failed"
					]
				},
					KeyValueMap[
						Function[{selectionVal, aggVal},
							<|
								splitKey -> splitKeyVal,
								selectionKey -> selectionVal,
								aggKey -> aggVal
							|>
						],
						aggData
					]
				]
			],
			data
		];
		ConfirmMatch[data, {__?AssociationQ}]
	]
];

GroupTotalBy[_, _, _, _] := Enclose[Confirm[$Failed, "Arguments do not match function definition"]];
GroupTotalBy[Repeated[_, {0, 2}]] := Enclose[Confirm[$Failed, "Not enough arguments specified"]];

categorizeSelectionValue[s_String] := s;
categorizeSelectionValue[other_] := Missing["MultipleValues", other];

groupTotalBySelector[s_String -> lst_List] := With[{
	operatorList = groupTotalBySelector[s -> #]& /@ DeleteDuplicates[lst]
},
	Function[data,
		#[data]& /@ operatorList
	]
];

groupTotalBySelector[s_String -> patt_] := Function[data,
	categorizeSelectionValue[patt] -> Cases[
		data,
		KeyValuePattern[{s -> patt}]
	]
];

groupTotalByAggregator[aggKey_, agg_][dat_?AssociationQ] := groupTotalByAggregator[aggKey, agg] /@ dat;

groupTotalByAggregator[aggKey_, agg_][dat_List] := Replace[
	Cases[dat, KeyValuePattern[{aggKey -> n_?NumericQ}] :> n],
	{
		{} -> 0,
		other_ :> agg[other]
	}
];
groupTotalByAggregator[__][_] := $Failed;

End[] (* End Private Context *)

EndPackage[]