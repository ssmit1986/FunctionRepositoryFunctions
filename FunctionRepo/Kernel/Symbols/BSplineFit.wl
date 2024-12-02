(* Wolfram Language Package *)

BeginPackage["FunctionRepo`BSplineFit`", {"FunctionRepo`"}]
(* Exported symbols added here with SymbolName::usage *)
GeneralUtilities`SetUsage[BSplineFit,
	"BSplineFit[data$, n$] finds a BSpline function that approximates the data."
];

FunctionRepo`LocalSupportFit;

Begin["`Private`"] (* Begin Private Context *)

LocalSupportFit[dat_List, fun_, d_?Positive] := Module[{
	data = dat,
	nodes,
	basis,
	funs = Flatten[{fun}]
},
	Enclose[
		ConfirmAssert[MatchQ[Dimensions[data], {_, 2}] && MatrixQ[data, NumericQ]];
		nodes = ConfirmBy[findNodes[MinMax[data[[All, 1]]], d], ListQ];
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

boxed[f_, x_] := Quiet[UnitBox[Divide[x, 8]] * f[x], General::munfl];

findNodes[{min_, max_}, d_] := Block[{
	halfd = Divide[d, 2],
	nodes,
	offset
},
	nodes = Range[Subtract[min, halfd], max + d, halfd];
	offset = Subtract[Subtract[max, Last[nodes]], Subtract[First[nodes], min]];
	nodes = Subtract[nodes, Divide[offset, 2]];
	nodes
];

BSplineFit[dat_List, d_] := LocalSupportFit[dat, BSplineBasis[3, (#/2 + 1/2)]&, d];

End[] (* End Private Context *)

EndPackage[]
