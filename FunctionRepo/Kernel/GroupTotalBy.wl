(* Wolfram Language Package *)

BeginPackage["FunctionRepo`GroupTotalBy`", {"FunctionRepo`"}]

GeneralUtilities`SetUsage[GroupTotalBy,
	"GroupTotalBy[data$, groupKey$, selectionKey$ -> selectionValue$, aggKey$ -> aggFun$] computes the aggregation of aggKey$ in data$ using aggFun$,\
grouped by groupKey$ and after having applied selection criteria. The returned data is a list of Associations.
GroupTotalBy[data$, {key$1, key$2, $$}, $$] groups by tuples of multiple keys.
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

GroupTotalBy[{<||>...}, Repeated[_, {3}]] := {};

GroupTotalBy[
	dat : {__?AssociationQ},
	splitKeys : (_String | {__String}),
	selectionKey_String -> selectionPatt_,
	aggKey_String -> agg_
] := Module[{
	data = KeyUnion[DeleteCases[dat, <||>]],
	splitSpec = Replace[splitKeys, s : Except[_List] :> {s}],
	keys,
	selectionFun
},
	Enclose[
		keys = Keys @ data[[1]];
		selectionFun = groupTotalBySelector[selectionKey -> selectionPatt];
		data = ConfirmBy[
			GroupBy[
				data,
				#[[splitSpec]]&,
				selectionFun
			],
			AssociationQ,
			"Grouping failed"
		];
		data = Flatten @ KeyValueMap[
			Function[
				Map[
					Function[assoc,
						Join[#1, assoc]
					],
					Replace[
						groupTotalByAggregator[aggKey, agg][#2],
						e : Except[_List] :> {e}
					]
				]
			],
			data
		];
		Rest @ KeyUnion[
			Prepend[AssociationThread[keys, 1]][
				ConfirmMatch[data, {__?AssociationQ}]
			]
		]
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
	Cases[data, KeyValuePattern[{s -> patt}]]
];

groupTotalByAggregator[aggKey_, agg_][dat : {__List}] := groupTotalByAggregator[aggKey, agg] /@ dat;

groupTotalByAggregator[aggKey_, agg_][dat : {___Association}] := With[{
	aggVal = Query[agg] @ If[ dat === {}, {}, Lookup[dat, aggKey]],
	rest = Merge[
		KeyDrop[dat, aggKey],
		If[ SameQ @@ #,
			First[#, Missing["Undefined"]],
			Missing["Undefined"]
		]&
	]
},
	Append[rest, aggKey -> aggVal]
];

groupTotalByAggregator[__][_] := $Failed;

End[] (* End Private Context *)

EndPackage[]