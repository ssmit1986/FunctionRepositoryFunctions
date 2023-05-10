(* Wolfram Language Package *)

BeginPackage["FunctionRepo`DiscreteHausdorffDistance`", {"FunctionRepo`", "GeneralUtilities`"}]
(* Exported symbols added here with SymbolName::usage *)
GeneralUtilities`SetUsage[DiscreteHausdorffDistance,
	"DiscreteHausdorffDistance[set$1, set$2] returns the Hausdorff distance between two sets."
];

Begin["`Private`"] (* Begin Private Context *) 

Options[DiscreteHausdorffDistance] = Options[Nearest];

DiscreteHausdorffDistance[s1_, s2_, opts : OptionsPattern[]] := Max[
	Nearest[s1 -> "Distance", s2, 1, opts],
	Nearest[s2 -> "Distance", s1, 1, opts]
];

End[] (* End Private Context *)

EndPackage[]