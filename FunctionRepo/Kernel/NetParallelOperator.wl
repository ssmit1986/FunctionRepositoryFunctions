(* Wolfram Language Package *)

BeginPackage["FunctionRepo`NetParallelOperator`", {"FunctionRepo`"}]
(* Exported symbols added here with SymbolName::usage *)
GeneralUtilities`SetUsage[NetParallelOperator,
	"NetParallelOperator[{net$1, net$2, $$}] creates a net with a single input and multiple outputs corresponding to applying the differents nets to the input.
NetParallelOperator[<|key$1 -> net$1, $$|>] labels the outputs with the given keys.
NetParallelOperator[nets$, cat$] creates a single output by catenating the results together using network cat$."
];


Begin["`Private`"] (* Begin Private Context *)

NetParallelOperator[{} | <||>, ___] := $Failed;

NetParallelOperator[nets_?AssociationQ] := With[{
	outputs = ToString /@ Keys[nets],
	keys = ToString /@ Range[Length[nets]]
},
	NetGraph[
		Thread[keys -> Values[nets]],
		MapThread[
			Function[NetPort["Input"] -> #1 -> NetPort[#2]],
			{keys, outputs}
		]
	]
];

(* Outputs are given as "Output1", "Output2", etc. *)
NetParallelOperator[nets : {__}] := NetParallelOperator[
	AssociationThread[
		Function["Output" <> ToString[#]] /@ Range[Length[nets]],
		nets
	]
];

(* 
	Evaluates nets in parallel, combines the outputs into a single array and aggregates the results into one output using layer cat.
	A typical value for cat would be CatenateLayer[0] (which is what you get when you specify Automatic)
*)
NetParallelOperator[nets : _List | _?AssociationQ, cat_] := Enclose @ With[{
	parallelNet = Confirm @ NetParallelOperator[nets]
},
	NetGraph[
		{
			"parallel" -> parallelNet,
			"cat" -> Replace[cat, Automatic -> CatenateLayer[0]]
		},
		Join[
			{NetPort["Input"] -> "parallel"},
			Map[
				Function[NetPort["parallel", #] -> "cat"],
				Keys @ Information[parallelNet, "OutputPorts"]
			]
		]
	]
];

End[] (* End Private Context *)

EndPackage[]
