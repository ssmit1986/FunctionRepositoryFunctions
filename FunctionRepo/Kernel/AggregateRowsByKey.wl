(* Wolfram Language Package *)

BeginPackage["FunctionRepo`AggregateRowsByKey`", {"FunctionRepo`"}]

GeneralUtilities`SetUsage[AggregateRowsByKey,
	"AggregateRowsByKey[data$, aggKey$ -> aggFun$] merges data, using aggFun$ for aggKey$ and keeping values for other keys if they are the same."
];

Begin["`Private`"] (* Begin Private Context *) 

AggregateRowsByKey[spec_][data_] := AggregateRowsByKey[data, spec];

AggregateRowsByKey[data_Dataset, spec_] := Replace[
	AggregateRowsByKey[Normal[data], spec],
	result : Except[_?FailureQ] :> Dataset[result]
];

AggregateRowsByKey[
	dat : {__?AssociationQ},
	aggSpec : (_Rule | {__Rule})
] := Module[{
	data = KeyUnion[dat, <||>]
},
	Enclose[
		ConfirmBy[
			MergeByKey[
				data,
				Replace[aggSpec, e : Except[_List] :> {e}],
				Replace[
					{
						l : {__} /; SameQ @@ l :> First[l],
						_ :> Missing["Undefined"]
					}
				]
			],
			AssociationQ
		]
	]
];

End[] (* End Private Context *)

EndPackage[]