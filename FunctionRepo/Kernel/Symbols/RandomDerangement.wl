(* Wolfram Language Package *)

BeginPackage["FunctionRepo`RandomDerangement`", {"FunctionRepo`"}]
(* Exported symbols added here with SymbolName::usage *)
GeneralUtilities`SetUsage[RandomDerangement,
	"RandomDerangement[n$] gives a random permutation of Range[n$] such that non of the elements stay in the same place.
RandomDerangement[list$] gives a random derangement of the elements in list$. Each element is assumed to be unique."
];

Begin["`Private`"] (* Begin Private Context *)

partitions[n_] := partitions[n] = IntegerPartitions[n, All, Range[2, n]];

RandomPartition[1] := {1};
RandomPartition[n_Integer?Positive] := With[{
	rand = Accumulate[Sort @ RandomReal[1, n]]
},
	DeleteCases[
		Differences[Prepend[0] @ ReplacePart[Floor[5 * rand / Max[rand]], n -> n]],
		0
	]
];

RandomDerangement[n_Integer /; n > 1] := With[{
	ints = Range[n],
	part = RandomPartition[n]
},
	PermutationReplace[
		ints,
		Cycles @ TakeList[RandomSample[ints], part]
	]
];

RandomDerangement[list_List] := With[{
	perm = RandomDerangement[Length[list]]
},
	list[[perm]] /; ListQ[perm]
];

RandomDerangement[_] := $Failed;

End[] (* End Private Context *)

EndPackage[]
