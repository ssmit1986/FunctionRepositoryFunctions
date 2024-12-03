(* Wolfram Language Package *)

BeginPackage["FunctionRepo`BSplineFit`", {"FunctionRepo`"}]
(* Exported symbols added here with SymbolName::usage *)
GeneralUtilities`SetUsage[BSplineFit,
	"BSplineFit[data$, n$] finds a BSpline function that approximates the data."
];

FunctionRepo`LocalSupportFit;

Begin["`Private`"] (* Begin Private Context *)

LocalSupportFit[dat_List, fun_, spec_] := Module[{
	data = dat,
	n, d, nodes,
	basis,
	funs = Flatten[{fun}]
},
	Enclose[
		ConfirmAssert[MatchQ[Dimensions[data], {_, 2}] && MatrixQ[data, NumericQ]];
		{nodes, n, d} = ConfirmBy[resolveSpec[MinMax[data[[All, 1]]], spec], ListQ];
		basis = Prepend[1] @ Flatten @ Map[
			Function[
				Comap[funs] @ Divide[Subtract[\[FormalX], #], d]
			],
			nodes
		];
		Fit[data, basis, \[FormalX], "Function"]
	]
];
LocalSupportFit[___] := $Failed;

dataRange[{min_, max_}] := Subtract[max, min];

resolveSpec[minMax_, Into[n_Integer?Positive]] := resolveSpec[minMax, {n, 2 * Divide[dataRange @ minMax, n - 2]}];
resolveSpec[minMax_, d_?Positive] := resolveSpec[minMax, 
	{
		2 * Ceiling @ Divide[dataRange[minMax], d] + 4,
		d
	}
];
resolveSpec[{min_, max_}, {n_Integer, d_?Positive}] := {
	Subdivide[min, max, n - 1],
	n,
	d
};
resolveSpec[___] := $Failed;

BSplineFit[dat_List, spec_] := LocalSupportFit[dat, BSplineBasis[3, (#/4 + 1/2)]&, spec];

End[] (* End Private Context *)

EndPackage[]
