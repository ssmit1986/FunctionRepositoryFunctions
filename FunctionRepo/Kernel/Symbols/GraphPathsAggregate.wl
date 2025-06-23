(* Wolfram Language Package *)

BeginPackage["FunctionRepo`GraphPathsAggregate`", {"FunctionRepo`"}]
(* Exported symbols added here with SymbolName::usage *)
GeneralUtilities`SetUsage[GraphPathsAggregate,
	"GraphPathsAggregate[gr$, start$ -> end$, f$, g$] "
];

Begin["`Private`"] (* Begin Private Context *)


Options[GraphPathsAggregate] = {
	"EdgeProperty" -> EdgeWeight
};


expr : GraphPathsAggregate[___] := Module[{
	args = ArgumentsOptions[expr, 4],
	opts, propName, pathSpecType
},
	Catch[
		Enclose[
			ConfirmMatch[Confirm @ args, {_List, _List}];
			{args, opts} = args;
			ConfirmBy[First @ args, GraphQ];
			propName = OptionValue[GraphPathsAggregate, opts, "EdgeProperty"];
			pathSpecType = Head[args[[2]]];
			Switch[pathSpecType,
				Rule,
					findPathAndCompute @@ Append[args, propName],
				List,
					multiPathCompute @@ Append[args, propName],
				_,
					Failure["GraphPathsAggregate",
						<|
							"MessageTemplate" -> "The second argument should be a Rule or List"
						|>
					]
			]
		],
		GraphPathsAggregate
	]
];

error[msg_] := Throw[
	Failure["Error", <|"Message" -> msg|>],
	GraphPathsAggregate
];
error[msg_String, rest__] := error[StringForm[msg, rest]];

return[expr_] := Throw[expr, GraphPathsAggregate];

findPathAndCompute[gr_, s_ -> t_, f_, g_, propName_] := Module[{
	paths = FunctionRepo`FindPathEdges[gr, s, t, Infinity, All]
},
	If[ ! ListQ[paths],
		error["No valid path `1` found", s -> t]
	];
	multiPathCompute[gr, paths, f, g, propName]
];

multiPathCompute[gr_, paths : {__List}, f_, g_, propName_] := g @@ Map[
	pathValues[gr, #, f, propName]&,
	paths
];
multiPathCompute[gr_, path_List, f_, g_, propName_] := g @ pathValues[gr, path, f, propName]

pathValues[gr_, edges_, f_, propName_] := With[{
	invalidEdges = Discard[edges, EdgeQ[gr, #]&]
},
	If[ invalidEdges === {},
		f @@ Replace[
			AnnotationValue[{gr, edges}, propName], $Failed -> Missing["NotDefined", propName],
			{1}
		],
		error["Edges `1` do not exist", invalidEdges]
	]
];


End[] (* End Private Context *)

EndPackage[]
