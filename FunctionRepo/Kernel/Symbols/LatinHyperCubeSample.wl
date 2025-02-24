BeginPackage["FunctionRepo`LatinHyperCubeSample`", {"FunctionRepo`", "GeneralUtilities`"}]
(* Exported symbols added here with SymbolName::usage *)
GeneralUtilities`SetUsage[FunctionRepo`LatinHyperCubeSampleUniform,
	"LatinHyperCubeSampleUniform[dim$, n$] samples n$ points from the hypercube of dimension dim$ using Latin Hypercube Sampling.
LatinHyperCubeSampleUniform[dim$, n$, samples$] repeats the sampling samples$ times.
LatinHyperCubeSampleUniform[{dist$1, dist$2, $$}, n$, $$] samples a product distribution using LHC sampling."
];
GeneralUtilities`SetUsage[FunctionRepo`LatinHyperCubeSampleDiscrete,
	"LatinHyperCubeSampleDiscrete[dim$, n$] samples n$ points from the hyper grid Range[n$]^dim$ Latin Hypercube Sampling."
];

Begin["`Private`"] (* Begin Private Context *) 

LatinHyperCubeSampleUniform[dim_Integer, n_Integer] := With[{
	leftbounds = N[Range[0, n - 1]/n],
	step = 1./n
},
	Transpose[
		Table[
			RandomSample[leftbounds],
			dim
		]
	] + RandomReal[{0, step}, {n, dim}]
];
LatinHyperCubeSampleUniform[dists : {__?DistributionParameterQ}, n_Integer, rest___] := Inner[
	Construct,
	InverseCDF /@ dists,
	Transpose @ LatinHyperCubeSampleUniform[Length[dists], n, rest],
	List
];
LatinHyperCubeSampleUniform[dim_, n_, nSamples_Integer] := Join @@ Table[
	LatinHyperCubeSampleUniform[dim, n],
	nSamples
];

LatinHyperCubeSampleDiscrete[dim_Integer, n_Integer, rest___] := LatinHyperCubeSampleDiscrete[dim, Range[n], rest];
LatinHyperCubeSampleDiscrete[vals_List?MatrixQ] := Transpose[RandomSample /@ vals]
LatinHyperCubeSampleDiscrete[vals_List?MatrixQ, nSamples_Integer] := Join @@ Table[
	LatinHyperCubeSampleDiscrete[vals],
	nSamples
];
LatinHyperCubeSampleDiscrete[dim_Integer, vals_List] := Transpose @ Table[RandomSample[vals], dim];
LatinHyperCubeSampleDiscrete[dim_Integer, vals_List, nSamples_Integer] := Join @@ Table[
	LatinHyperCubeSampleDiscrete[dim, vals],
	nSamples
];


End[] (* End Private Context *)

EndPackage[]

