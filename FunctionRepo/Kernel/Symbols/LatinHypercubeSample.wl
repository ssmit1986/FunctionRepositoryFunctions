BeginPackage["FunctionRepo`LatinHypercubeSample`", {"FunctionRepo`", "GeneralUtilities`"}]
(* Exported symbols added here with SymbolName::usage *)
GeneralUtilities`SetUsage[FunctionRepo`LatinHypercubeSample,
	"LatinHypercubeSample[dim$, n$] samples n$ points from the hypercube of dimension dim$ using Latin Hypercube Sampling.
LatinHypercubeSample[dim$, n$, samples$] repeats the sampling samples$ times.
LatinHypercubeSample[{dist$1, dist$2, $$}, n$, $$] samples a product distribution using LHC sampling."
];
GeneralUtilities`SetUsage[FunctionRepo`LatinHypercubeSampleDiscrete,
	"LatinHypercubeSampleDiscrete[dim$, n$] samples n$ points from the hyper grid Range[n$]^dim$ Latin Hypercube Sampling."
];

Begin["`Private`"] (* Begin Private Context *)

LatinHypercubeSample[dim_Integer, n_Integer] := With[{
	samples = hyperCubeSampleUniform[dim, n]
},
	If[ dim === 1,
		Flatten[samples],
		Transpose[samples]
	]
];
LatinHypercubeSample[dist_?DistributionParameterQ, rest__] := InverseCDF[dist, LatinHypercubeSample[1, rest]];
LatinHypercubeSample[dists : {__?DistributionParameterQ}, rest__] := Transpose @ MapThread[
	Construct,
	{
		CurryApplied[InverseCDF, 2] /@ dists,
		hyperCubeSampleUniform[Length[dists], rest]
	}
];
LatinHypercubeSample[spec_, n_, nSamples_Integer] := Join @@ Table[
	LatinHypercubeSample[spec, n],
	nSamples
];

hyperCubeSampleUniform[dim_, n_] := With[{
	leftbounds = Divide[Range[0, n - 1, 1.], n],
	step = Divide[1., n]
},
	Table[
		RandomSample[leftbounds],
		dim
	] + RandomReal[{0, step}, {dim, n}]
];
hyperCubeSampleUniform[dim_, n_, nSamples_Integer] := Join[##, 2]& @@ Table[
	hyperCubeSampleUniform[dim, n],
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

