(* Wolfram Language Package *)

BeginPackage["FunctionRepo`MissingDataLogLikelihood`", {"FunctionRepo`"}]
(* Exported symbols added here with SymbolName::usage *)
GeneralUtilities`SetUsage[MissingDataLogLikelihood,
	"MissingDataLogLikelihood[dist$, data$] computes a log-likelihood for the observed data$ from the distribution dist$ assuming that missing values are ignorable."
];


Begin["`Private`"] (* Begin Private Context *)

MissingDataLogLikelihood[dist_, vec_?VectorQ] := LogLikelihood[dist, DeleteMissing[vec]];

MissingDataLogLikelihood[dist_, mat_?MatrixQ] := Module[{
	groupedData = GroupBy[
		DeleteCases[mat, {__Missing}],
		Map[MissingQ] -> DeleteMissing
	],
	marginals, rng
},
	rng = Range[Length @ First[Keys[groupedData], {}]];
	marginals = Map[
		MarginalDistribution[
			dist,
			Pick[rng, #, False]
		]&,
		Keys[groupedData]
	];
	Total @ MapThread[
		LogLikelihood,
		{
			marginals,
			Values[groupedData]
		}
	]
];


End[] (* End Private Context *)

EndPackage[]
