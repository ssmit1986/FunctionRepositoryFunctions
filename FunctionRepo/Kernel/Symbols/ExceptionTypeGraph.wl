(* Wolfram Language Package *)

BeginPackage["FunctionRepo`ExceptionTypeGraph`", {"FunctionRepo`"}]
(* Exported symbols added here with SymbolName::usage *)
GeneralUtilities`SetUsage[ExceptionTypeGraph,
	"DefinitionString[sym$] returns the definition of symbol sym$ as a string with context aliases for readability. Removes ReadProtected attribute and shows internal implementation details."
];

Begin["`Private`"] (* Begin Private Context *)

parentExceptions[type_] := DeleteCases[Exception[type]["ExceptionTagList"], type];

Options[ExceptionTypeGraph] = {"RegisteredTypesOnly" -> False};

ExceptionTypeGraph[opts : OptionsPattern[]] := Module[{
	types, edges, vertices,
	regOnlyQ = TrueQ @ OptionValue["RegisteredTypesOnly"]
},
	types = ExceptionTypes[];
	edges = Flatten @ KeyValueMap[
		Thread @ DirectedEdge[#2, #1] &,
		AssociationMap[parentExceptions, types]
	];
	If[ regOnlyQ,
		edges //= Select[AllTrue @ MatchQ[Alternatives @@ types]]
	];
	vertices = DeleteDuplicates @ Flatten[List @@@ edges];
	TransitiveReductionGraph @ Graph[
		vertices,
		edges,
		VertexLabels -> Map[# -> Style[#, ShowStringCharacters -> True]&, vertices],
		GraphLayout -> {"LayeredDigraphEmbedding", "Orientation" -> Top}
	]
];

End[] (* End Private Context *)

EndPackage[]
