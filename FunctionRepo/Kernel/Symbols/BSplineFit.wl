(* Wolfram Language Package *)

BeginPackage["FunctionRepo`BSplineFit`", {"FunctionRepo`"}]
(* Exported symbols added here with SymbolName::usage *)
GeneralUtilities`SetUsage[BSplineFit,
	"BSplineFit[data$, n$] finds a BSpline function that approximates the data."
];

Begin["`Private`"] (* Begin Private Context *)

BSplineFit[dat_List, n_Integer] := Module[{
	data = dat
},
	Enclose[
		ConfirmAssert[MatchQ[Dimensions[data], {_, 2}] && MatrixQ[data, NumericQ]];
		ConfirmAssert[n > 3];
		iBSplineFit[data, n, 3]
	]
]

subDiv[{min_, max_}, n_] := Block[{
	sub = Subdivide[min, max, n - 2],
	d
},
	d = Subtract @@ sub[[{2, 1}]];
	Flatten[{First[sub] - d, sub, Last[sub] + d}]
];


iBSplineFit[dat_, n_, d_] := Module[{
	data = dat,
	minMax = MinMax[dat[[All, 1]]],
	knots,
	bSplineBasis,
	x
},
	knots = subDiv[minMax, n - d + 2];
	knots = Join[
		ConstantArray[First @ knots, d],
		knots,
		ConstantArray[Last @ knots, d]
	];
	bSplineBasis = Prepend[1] @ Map[
		BSplineBasis[{3, knots}, #, x]&,
		Range[1, n]
	];
	Fit[data, bSplineBasis, x, "Function"]
]

End[] (* End Private Context *)

EndPackage[]
