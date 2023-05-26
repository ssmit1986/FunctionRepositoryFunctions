(* Wolfram Language Package *)

BeginPackage["FunctionRepo`LogSumExpLayer`", {"FunctionRepo`"}]
(* Exported symbols added here with SymbolName::usage *)
GeneralUtilities`SetUsage[LogSumExpLayer,
	"LogSumExpLayer[] creates a NetGraph that computes the LogSumExp of an array on level 1.
LogSumExpLayer[{n$1, n$2, $$}] creates a NetGraph that computes the LogSumExp of an array on levels n$1, n$2, $$.
LogSumExpLayer[All] aggregates across all levels, returning a scalar value."
];

Begin["`Private`"] (* Begin Private Context *)

Options[LogSumExpLayer] = {
	"Input" -> Automatic,
	"Output" -> Automatic,
	"Aggregator" -> Total,
	"LevelSorting" -> True
};

LogSumExpLayer[levels : (_Integer | {__Integer}) : 1, opts : OptionsPattern[]] := With[{
	sortedLevels = If[ TrueQ[OptionValue["LevelSorting"]],
		SortBy[{Sign, Abs}], (*Sorts the positive and negative levels for the ReplicateLayer chain *)
		Identity
	][Flatten@{levels}],
	aggregator = OptionValue["Aggregator"]
},
	NetGraph[
		<|
			"max" -> AggregationLayer[Max, sortedLevels],
			"replicate" -> NetChain[Map[ReplicateLayer[Automatic, #] &, sortedLevels]],
			"subtract" -> ThreadingLayer[Subtract],
			"exp" -> ElementwiseLayer[Exp],
			"sum" -> AggregationLayer[aggregator, sortedLevels],
			"logplusmax" -> ThreadingLayer[Function[{avg, max}, Log[avg] + max]]
		|>,
		{
			NetPort["Input"] -> "max" -> "replicate",
			{NetPort["Input"], "replicate"} -> "subtract" -> "exp" -> "sum",
			{"sum", "max"} -> "logplusmax"
		},
		"Input" -> OptionValue["Input"],
		"Output" -> OptionValue["Output"]
	]
];

LogSumExpLayer[All, opts : OptionsPattern[]] := NetChain[
	{
		FlattenLayer[],
		LogSumExpLayer[
			1,
			Sequence @@ FilterRules[ {opts}, Except["Input" | "Output"]]
		]
	},
	"Input" -> OptionValue["Input"],
	"Output" -> OptionValue["Output"] 
];

End[] (* End Private Context *)

EndPackage[]


