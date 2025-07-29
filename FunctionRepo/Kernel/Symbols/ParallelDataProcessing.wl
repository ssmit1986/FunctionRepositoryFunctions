(* Wolfram Language Package *)

BeginPackage["FunctionRepo`ParallelDataProcessing`", {"FunctionRepo`"}]

(* Exported symbols added here with SymbolName::usage *)
GU`SetUsage[ParallelDataProcessing,
	"ParallelDataProcessing[f$, data$] divides data$ into $ProcessorCount partitions part$i and then evaluates f[part$i] in parallel.
ParallelDataProcessing[f$, data$, test$] uses test$ to check if the result is valid."
];

Begin["`Private`"] (* Begin Private Context *)


nep := nep = ResourceFunction["NearEqualPartition", "Function"];

nearEqualPartition[list_List, n_] := nep[list, n];
nearEqualPartition[assoc_Association, n_] := GU`AssociationTranspose[
	nep[#, n]& /@ assoc,
	{}
];


launchKernels[] := launchKernels[$ProcessorCount];

launchKernels[n_Integer] /; n > $ProcessorCount := launchKernels[$ProcessorCount];

launchKernels[n_Integer] /; n === $ProcessorCount === $KernelCount := Kernels[];

launchKernels[n_Integer] /; $KernelCount < n := LaunchKernels[n];


dataAssocQ[assoc_] := And[
	AssociationQ[assoc],
	AllTrue[assoc, ListQ],
	SameQ @@ Map[Length, assoc]
];

dataQ[expr_] := MatchQ[expr, _List?ListQ | _Association?dataAssocQ]


ParallelDataProcessing[f_][data_] := ParallelDataProcessing[f, data];

ParallelDataProcessing[f_, data_] := ParallelDataProcessing[f, data, Function[True]];

ParallelDataProcessing[f_, data_?dataQ, test_] := Module[{
	kernels,
	partitions,
	files,
	mxFile,
	result
},
	nep;
	kernels = launchKernels[];
	DistributeDefinitions @@ Flatten[
		Hold @@ Cases[
			Hold[{f, test}],
			s_Symbol /; Context[s] =!= "System`" :> Hold[s],
			{0, Infinity},
			Heads -> True
		]
	];
	partitions = nearEqualPartition[
		data,
		Length[kernels]
	];
	files = WithCleanup[
		SetDirectory[$TemporaryDirectory]
		,
		Map[
			With[{filename = CreateUUID["data-"] <> ".mx"},
				AbsoluteFileName @ Export[filename, #]
			]&,
			partitions
		]
		,
		ResetDirectory[]
	];
	ParallelDo[
		Export[
			mxFile,
			Replace[
				f @ Import[mxFile, "MX"],
				Except[_?test] :> Failure["EvaluationFailed", <||>]
			]
		],
		{mxFile, files}
	];
	result = Import[#, "MX"]& /@ files;
	DeleteFile[files];
	result
];
ParallelDataProcessing[_, __] := Failure["ParallelDataProcessing", <||>];


End[] (* End Private Context *)

EndPackage[]
