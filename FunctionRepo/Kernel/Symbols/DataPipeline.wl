(* Wolfram Language Package *)

BeginPackage["FunctionRepo`DataPipeline`", {"FunctionRepo`"}]
(* Exported symbols added here with SymbolName::usage *)
GeneralUtilities`SetUsage[DataPipeline,
	"DataPipeline[{op1$, op2$, ...}][data$] applies a sequence of operators op$i to data$. If an operator fails or does not return valid intermediary data, the pipeline stops and returns a failure.
DataPipeline[{key$1 -> operator$1, ...}, {key$i1 -> key$j1, $$}][data$] applies a computational network to the data. The keys are used to extract and store intermediary data, and the edges define the flow of data between operators. A rule {key$i1, $$} -> key$j can be used in the second argument to send multiple pieces of data in List form to a single operator.
Information[DataPipeline[...], \"Graph\"] returns a graph representation of the pipeline or network."
];

Begin["`Private`"] (* Begin Private Context *)

Options[DataPipeline] = {
	"FailureDetection" -> Automatic,
	"CatchMessages" -> True
};

failPattern = _Missing | Indeterminate | Undefined | ComplexInfinity | $Canceled | $Aborted | _DirectedInfinity;
failQ[data_] := MatchQ[data, failPattern];

parseFailureDetection[Automatic] := failQ;
parseFailureDetection[None] := Function[False];
parseFailureDetection[fun_] := fun;

createFailure[fail_?FailureQ] := fail;
createFailure[other_] := With[{head = Head[other]},
	Failure["InvalidData",
		<|
			"MessageTemplate" -> "DataPipeline encountered invalid data with Head `1`",
			"MessageParameters" -> {head},
			"Head" -> head,
			"Expression" -> other
		|>
	]
];

checkedOperator[False, op_][data___] := op[data];
checkedOperator[True, op_][data___] := With[{
	tag = CreateUUID[]
},
	Enclose[
		ConfirmQuiet[op[data], All, Null, tag],
		Identity,
		tag
	]
];

postFailsafe[failTest_][op_] := postFailsafe[op, failTest];

postFailsafe[op_, failTest_][data_] := Replace[
	op[data],
	{
		fail_?FailureQ :> fail,
		res_?failTest :> createFailure[res]
	}
];

(* Data pipeline *)
parseOpts[list_List] := {
	parseFailureDetection @ OptionValue[DataPipeline, list, "FailureDetection"],
	TrueQ @ OptionValue[DataPipeline, list, "CatchMessages"]
};

(pipe_DataPipeline)[args___] /; Length[HoldComplete[args]] <= 1 := With[{
	parse = System`Private`ArgumentsWithRules[
		pipe,
		{1, 2},
		List,
		Options[DataPipeline]
	]
},
	Switch[parse,
		{{_List}, _},
			dataChain[
				parse[[1, 1]],
				parseOpts[Last[parse]]
			][args],
		{{vertexKeyValPattern, edgeKeyValPattern}, _},
			dataGraph[
				parse[[1, 1]],
				parse[[1, 2]],
				parseOpts[Last[parse]]
			][args],
		_,
			Failure["InvalidPipeline", <|"Pipeline" -> pipe|>]
	]
];

(* Linear pipeline *)
dataChain[rules : {__Rule}, test_] := dataChain[Values @ rules, test];
dataChain[_, _][fail_?FailureQ] := fail;
dataChain[_, {failTest_, _}][data_] /; failTest[data] := createFailure[data];
dataChain[{}, _][anything_] := anything;
dataChain[{op_}, {failTest_, boole_}][data_] := postFailsafe[checkedOperator[boole, op], failTest][data]; (* final validation before returning the result *)
dataChain[{op_, rest__}, lst : {failTest_, boole_}][data_] := dataChain[{rest}, lst][checkedOperator[boole, op][data]];

(* Special case for start of the chain *)
dataChain[{op_, rest___}, lst : {_, boole_}][] := dataChain[{rest}, lst][checkedOperator[boole, op][]];

(* Shouldn't ever be reached *)
dataChain[pipe___][___] := Failure["InvalidPipeline", <|"Pipeline" -> HoldComplete[pipe]|>];


(* Network pipeline *)
vertexKeyValPattern = {(_String -> _)...};
edgeKeyValPattern = {((_String -> _String) | ({__String} -> _String))...};

standardizeEdges[edgeRules_] := Reverse[
	Normal @ GroupBy[
		edgeRules,
		Last -> First,
		Replace[
			{
				{el_} :> el,
				other_ :> Flatten[other]
			}
		]
	],
	{2}
];

trimAssoc[edges_][dataOut_] := Replace[
	KeyDrop[Flatten @ Keys[edges]] @ dataOut,
	a_Association /; Length[a] === 1 :> First[a]
];

allKeys[vertList_, edges_] := Flatten[{Keys[vertList], Keys[edges], Values[edges]}];

inputKeys[vertList_, edges_] := Complement[
	Flatten[{Keys[vertList], Keys[edges]}],
	Values[edges]
];

outputKeys[vertList_, edges_] := Complement[
	Values[edges],
	Flatten @ Keys[edges]
];

(* Short circuit evaluations *)
dataGraph[_, {},  {failTest_, boole_}][anything_] := postFailsafe[Identity, failTest] @ anything;
dataGraph[_, _, _][fail_?FailureQ] := fail;

(* Automatic generation of missing inputs *)
dataGraph[vertList_, edges_, test_][] := dataGraph[vertList, edges, test][<||>];
dataGraph[vertList_, edges_, test : {failTest_, boole_}][arg : Except[_Association]] := With[{
	keysIn = inputKeys[vertList, edges]
},
	Condition[
		(* Generate other inputs automatically *)
		dataGraph[vertList, edges, test][<|"Input" -> arg|>]
		,
		And[
			Length[keysIn] > 1,
			MemberQ[keysIn, "Input"]
		]
	]
];
dataGraph[vertList_, edges_, test : {failTest_, boole_}][assoc_Association] := Module[{
	keysIn = Complement[inputKeys[vertList, edges], Keys[assoc]],
	generatedInput
},
	Condition[
		(* Evaluate the generator functions *)
		generatedInput = Map[
			Function[checkedOperator[boole, #][]],
			KeyTake[vertList, keysIn]
		];
		dataGraph[
			Normal @ KeyDrop[vertList, keysIn],
			edges,
			test
		][Join[assoc, generatedInput]]
		,
		keysIn =!= {} && ContainsAll[Keys[vertList], keysIn]
	]
];

(* Main evaluation *)
dataGraph[vertList_, edges_, test : {failTest_, boole_}][data_] := With[{
	edgeRules = standardizeEdges[edges]
},
	If[
		AssociationQ[data] && Or[!MemberQ[Flatten @ Keys[edges], "Input"], KeyExistsQ[data, "Input"]]
		,
		With[{
			failArgs = Select[data, Comap[FailureQ || failTest]]
		},
			If[ Length[failArgs] > 0,
				createFailure[failArgs],
				Replace[
					iDataGraph[vertList, edgeRules, test][data],
					a_Association :> trimAssoc[edges] @ KeyDrop[a, Keys[data]] (* Only keep the computed vertices that do not feed other computations *)
				]
			]
		]
		,
		If[ failTest[data],
			createFailure[data],
			Replace[
				iDataGraph[vertList, edgeRules, test][<|"Input" -> data|>],
				a_Association :> trimAssoc[edges] @ KeyDrop[a, "Input"] (* Only keep the computed vertices that do not feed other computations *)
			]
		]
	]
];

iDataGraph[___][expr : Except[_Association]] := createFailure[expr];
iDataGraph[_, {}, _][data_] := data;
iDataGraph[vertices_, {(keyIn : _String | {__String}) -> keyOut_, rest___}, lst : {failTest_, boole_}][data_] := With[{
	input = If[ ListQ[keyIn],
		Replace[
			Lookup[data, keyIn],
			l_List /; AnyTrue[l, MissingQ] :> Missing["MissingInput"]
		],
		data[keyIn]
	],
	op = Lookup[vertices, keyOut]
},{
	newData = Which[
		MissingQ[input],
			Failure["MissingInput", <|"Key" -> keyIn|>],
		MissingQ[op],
			Failure["MissingOperator", <|"Key" -> keyOut|>],
		True,
			postFailsafe[checkedOperator[boole, op], failTest] @ input
	]
},
	If[ FailureQ[newData],
		createFailure[newData],
		iDataGraph[vertices, {rest}, lst] @ Append[
			data,
			keyOut -> newData
		]
	]
];

(* Shouldn't ever be reached *)
Scan[
	Function[sym,
		sym[pipe___][___] := Failure["InvalidPipeline", <|"Pipeline" -> HoldComplete[pipe]|>],
		HoldFirst
	]
	,
	Hold[dataChain, dataGraph, iDataGraph]
];

DataPipeline /: Information[
	HoldPattern @ DataPipeline[vertices : {__Rule}, edges : {__Rule}, OptionsPattern[]],
	"Graph"
] := With[{
	vlist = Labeled[#1, Row[{#1, ": ", Short[#2]}]]& @@@ vertices,
	elist = DirectedEdge @@@ Flatten @ Replace[
		standardizeEdges @ edges,
		r : Verbatim[Rule][_List, _String] :> Thread[r],
		{1}
	]
},
	Graph[vlist, elist,
		GraphLayout -> {"LayeredDigraphEmbedding", "Orientation" -> Left},
		VertexLabels -> Automatic
	]
];

DataPipeline /: Information[
	HoldPattern @ DataPipeline[chain_List, OptionsPattern[]],
	"Graph"
] := PathGraph[
	If[ MatchQ[chain, {__Rule}],
		Labeled[#1, Row[{#1, ": ", Short[#2]}]]& @@@ chain,
		MapIndexed[
			Labeled[First @ #2, Short[#1]]&,
			chain
		]
	],
	GraphLayout -> {"LayeredDigraphEmbedding", "Orientation" -> Left},
	DirectedEdges -> True,
	VertexLabels -> Automatic
];


End[] (* End Private Context *)

EndPackage[]
