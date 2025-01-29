(* Wolfram Language Package *)

BeginPackage["FunctionRepo`LocalSupportFit`", {"FunctionRepo`"}]


GeneralUtilities`SetUsage[LocalSupportFit,
	"LocalSupportFit[data$, f$, Into[n$]] finds an approximating curve for data$ using a linear combination of n$ equally spaced copies of kernel function f$."
];

Begin["`Private`"] (* Begin Private Context *)

Options[LocalSupportFit] = {
	IncludeConstantBasis -> True
};


LocalSupportFit[dat_List, fun_, spec_, opts : OptionsPattern[]] := Module[{
	data = dat,
	n, d, nodes,
	basis,
	funs = Flatten[{fun}],
	includeConstantBasisQ = TrueQ @ OptionValue[IncludeConstantBasis]
},
	Enclose[
		ConfirmAssert[MatchQ[Dimensions[data], {_, 2}] && MatrixQ[data, NumericQ]];
		{nodes, n, d} = ConfirmBy[resolveSpec[MinMax[data[[All, 1]]], spec], ListQ];
		basis = ConfirmBy[Confirm @ computeBasis[funs, nodes, d, \[FormalX]], ListQ];
		If[ includeConstantBasisQ,
			basis = Prepend[1] @ basis
		];
		Fit[data, basis, \[FormalX], "Function"]
	]
];
LocalSupportFit[___] := $Failed;

dataRange[{min_, max_}] := Subtract[max, min];

resolveSpec[minMax_, Into[n_Integer?Positive]] := resolveSpec[minMax, {n, 2 * Divide[dataRange @ minMax, n - 2]}];
resolveSpec[minMax_, d_?Positive] := resolveSpec[minMax,
	{
		2 * Ceiling @ Divide[dataRange[minMax], d] + 2,
		d
	}
];
resolveSpec[{min_?NumericQ, max_?NumericQ}, {n_Integer, d_?Positive}] := {
	Flatten @ CoordinateBoundsArray[{{min, max}}, Into[n - 2], Scaled[1/2], 1],
	n,
	d
};
resolveSpec[___] := $Failed;

computeBasis[funs_List, nodes_, invD_?SquareMatrixQ, x_List] /; Length[x] === Length[d] := Flatten @ Map[
	Function[x0,
		Comap[funs] @ Dot[Subtract[x, x0], invD, Subtract[x, x0]]
	],
	nodes
];

computeBasis[funs_List, nodes_, d_, x_] := Flatten @ Map[
	Function[x0,
		Comap[funs] @ Divide[Subtract[x, x0], d]
	],
	nodes
];

computeBasis[___] := $Failed;


End[] (* End Private Context *)

EndPackage[]
