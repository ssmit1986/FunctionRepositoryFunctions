BeginPackage["FunctionRepo`LatinHypercubeSample`", {"FunctionRepo`", "GeneralUtilities`"}]
(* Exported symbols added here with SymbolName::usage *)
GeneralUtilities`SetUsage[FunctionRepo`LatinHypercubeSampleUniform,
	"LatinHypercubeSampleUniform[dim$, n$] samples n$ points from the hypercube of dimension dim$ using Latin Hypercube Sampling.
LatinHypercubeSampleUniform[dim$, n$, samples$] repeats the sampling samples$ times.
LatinHypercubeSampleUniform[{dist$1, dist$2, $$}, n$, $$] samples a product distribution using LHC sampling."
];
GeneralUtilities`SetUsage[FunctionRepo`LatinHypercubeSampleDiscrete,
	"LatinHypercubeSampleDiscrete[dim$, n$] samples n$ points from the hyper grid Range[n$]^dim$ Latin Hypercube Sampling."
];

Begin["`Private`"] (* Begin Private Context *) 

LatinHypercubeSampleUniform[dim_Integer, n_Integer] := With[{
	leftbounds = Divide[Range[0, n - 1, 1.], n],
	step = Divide[1., n]
},
	Transpose[
		Table[
			RandomSample[leftbounds],
			dim
		]
	] + RandomReal[{0, step}, {n, dim}]
];
LatinHypercubeSampleUniform[dists : {__?DistributionParameterQ}, n_Integer, rest___] := Transpose @ MapThread[
	Construct,
	{
		CurryApplied[InverseCDF, 2] /@ dists,
		Transpose @ LatinHypercubeSampleUniform[Length[dists], n, rest]
	}
];
LatinHypercubeSampleUniform[dim_, n_, nSamples_Integer] := Join @@ Table[
	LatinHypercubeSampleUniform[dim, n],
	nSamples
];

LatinHypercubeSampleDiscrete[dim_Integer, n_Integer, rest___] := LatinHypercubeSampleDiscrete[dim, Range[n], rest];
LatinHypercubeSampleDiscrete[vals_List?MatrixQ] := Transpose[RandomSample /@ vals]
LatinHypercubeSampleDiscrete[vals_List?MatrixQ, nSamples_Integer] := Join @@ Table[
	LatinHypercubeSampleDiscrete[vals],
	nSamples
];
LatinHypercubeSampleDiscrete[dim_Integer, vals_List] := Transpose @ Table[RandomSample[vals], dim];
LatinHypercubeSampleDiscrete[dim_Integer, vals_List, nSamples_Integer] := Join @@ Table[
	LatinHypercubeSampleDiscrete[dim, vals],
	nSamples
];


End[] (* End Private Context *)

EndPackage[]

