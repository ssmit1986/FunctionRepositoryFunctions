(* Wolfram Language Package *)

BeginPackage["FunctionRepo`maximumSpacingEstimation`", {"FunctionRepo`"}]
(* Exported symbols added here with SymbolName::usage *)
maximumSpacingEstimation::usage = "maximumSpacingEstimation[data, dist] fits dist to data using the maximum spacing estimation method.";

Begin["`Private`"] (* Begin Private Context *)

Options[maximumSpacingEstimation] = Join[
	Options[NMaximize],
	{Assumptions :> $Assumptions}
];
maximumSpacingEstimation[
	data_?(VectorQ[#, NumericQ]&),
	dist_?Statistics`Library`UnivariateDistributionQ,
	opts : OptionsPattern[]
] := Module[{
	expr = Inactivate[
		Mean @ Log @ Differences @ Flatten[{
			0,
			CDF[dist, Sort @ data],
			1
		}],
		Except[Sort]
	],
	cons = DistributionParameterAssumptions[dist],
	result
},
	result = Block[{
		Indeterminate = -Statistics`Library`MachineInfinity
	},
		NMaximize[
			{
				expr,
				Simplify[cons && OptionValue[Assumptions]]
			},
			Statistics`Library`GetDistributionParameters[dist],
			Sequence @@ FilterRules[{opts}, Options[NMaximize]]
		]
	];
	If[ MatchQ[result, {_, {__Rule}}],
		<|
			"Distribution" -> dist /. Last[result],
			"MaxSpacingEstimate" -> First[result]
		|>,
		$Failed
	]
];

End[] (* End Private Context *)

EndPackage[]