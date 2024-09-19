(* Wolfram Language Package *)

BeginPackage["FunctionRepo`HighestDensityInterval`", {"FunctionRepo`"}]
(* Exported symbols added here with SymbolName::usage *)
GeneralUtilities`SetUsage[HighestDensityInterval,
	"HighestDensityInterval[dist$, p$] gives the shortest interval that contains the fraction p$ of the probability mass of distribution dist$."
];

Begin["`Private`"] (* Begin Private Context *)

HighestDensityInterval::invalidp = "Numeric fraction `1` must be between 0 and 1."

HighestDensityInterval[_, p_?(Function[TrueQ @ Or[# <= 0, 1 < #]])] := (
	Message[HighestDensityInterval::invalidp, p];
	$Failed
);
HighestDensityInterval[dist_?DistributionParameterQ, p_] := With[{
	try = iHDF[dist, p]
},
	try /; MatchQ[try, _Interval | _?FailureQ]
];
HighestDensityInterval[dist_?VectorQ, p_] := With[{
	try = iNHDF[dist, p]
},
	try /; MatchQ[try, _Interval | _?FailureQ]
];

iHDF[_, p_?(Function[TrueQ @ Or[# < 0, 1 < #]])] := (
	Message[HighestDensityInterval::invalidp, p];
	$Failed
);
iHDF[dist_, _?(EqualTo[1])] := DistributionDomain[dist];

iHDF[dist_, p_] := Assuming[
	And[0 < p < 1, 0 <= \[FormalX] <= 1 - p],
	With[{
		invCDF = InverseCDF[dist] 
	},
		Replace[
			Minimize[
				{Simplify[invCDF[p + \[FormalX]] - invCDF[\[FormalX]]], $Assumptions},
				\[FormalX]
			],
			{
				{_, sol : {__Rule}} :> Interval @ Map[
					invCDF,
					With[{x = Simplify @ Replace[\[FormalX], sol]},
						{x, p + x}
					]
				],
				_ :> Missing[]
			}
		]
	]
];

End[] (* End Private Context *)

EndPackage[]