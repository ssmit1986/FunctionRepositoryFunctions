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
vertexInputPattern = With[{patt = _String | {__String}},
	patt | KeyTake[patt] | Lookup[patt]
];
edgeKeyValPattern = {(vertexInputPattern -> _String)...};

stripEdgeOperators[list_] := ReplaceAll[list, KeyTake | Lookup -> Identity];

standardizeEdges[edgeRules_] := Reverse[
	Normal @ GroupBy[
		edgeRules,
		Last -> First,
		Replace[
			{
				{el_} :> el,
				other_ :> With[{flat = Flatten[{other}]}, flat /; MatchQ[flat, {__String}]],
				_ :> $Failed
			}
		]
	],
	{2}
];

trimAssoc[edges_][dataOut_] := Replace[
	KeyDrop[allInputs[edges]] @ dataOut,
	a_Association /; Length[a] === 1 :> First[a]
];

allKeys[vertList_, edges_] := Flatten[{Keys[vertList], Keys[edges], Values[edges]}];

allInputs[edges_] := Flatten @ stripEdgeOperators @ Keys[edges];

inputKeys[vertList_, edges_] := Complement[
	Flatten[{Keys[vertList], allInputs[edges]}],
	Values[edges]
];

outputKeys[vertList_, edges_] := Complement[
	Values[edges],
	allInputs[edges]
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
	If[ FailureQ[edgeRules],
		Failure["InvalidEdges", <|"Edges" -> HoldComplete[edges]|>],
		If[
			AssociationQ[data] && Or[!MemberQ[allInputs[edges], "Input"], KeyExistsQ[data, "Input"]]
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
	]
];

propagateMissing[data_] := With[{
	miss = Select[data, MissingQ]
},
	If[ Length[miss] === 0,
		data,
		Missing["MissingInput",  Keys[miss]]
	]
];

(* Intermediate data graph evaluation *)

iDataGraph[___][expr : Except[_Association]] := createFailure[expr];
iDataGraph[_, {}, _][data_] := data;
iDataGraph[vertices_, {inputSpec_ -> keyOut_, rest___}, lst : {failTest_, boole_}][data_] := With[{
	input = Switch[inputSpec,
		{__String},
			propagateMissing @ Lookup[data, inputSpec],
		_String,
			data[inputSpec],
		_Lookup | _KeyTake,
			propagateMissing @ inputSpec @ data
	],
	op = Lookup[vertices, keyOut, Identity]
},{
	newData = Which[
		MissingQ[input],
			Failure["MissingInput", <|"Missing" -> input|>],
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
		stripEdgeOperators @ standardizeEdges @ edges,
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

DataPipeline /: MakeBoxes[
	expr : DataPipeline[vertices : {__Rule}, edges : {__Rule}, OptionsPattern[]], 
	form_
] := With[{
	gr = Information[expr, "Graph"],
	stEdges = standardizeEdges[edges]
},
	BoxForm`ArrangeSummaryBox["DataPipeline",
		expr,
		"Network",
		{
			BoxForm`SummaryItem[{"Inputs: ", inputKeys[vertices, stEdges]}],
			BoxForm`SummaryItem[{"Outputs: ", outputKeys[vertices, stEdges]}],
			BoxForm`SummaryItem[{"Number of operations: ", VertexCount[gr]}]
		},
		{
			BoxForm`SummaryItem[{"Graph: ", Show[gr, ImageSize -> 300]}]
		},
		form
	] /; GraphQ[gr]
];

DataPipeline /: MakeBoxes[
	expr : DataPipeline[chain_List, OptionsPattern[]], 
	form_
] := With[{
	gr = Information[expr, "Graph"]
},
	BoxForm`ArrangeSummaryBox["DataPipeline",
		expr,
		"Chain",
		{
			BoxForm`SummaryItem[{"Number of operations: ", VertexCount[gr]}]
		},
		{
			BoxForm`SummaryItem[{"Graph: ", Show[gr, ImageSize -> 300]}]
		},
		form
	] /; GraphQ[gr]
];

FunctionRepo`DataPipeline`Private`dataPipelineTests = {
	TestCreate[
		FailureQ[DataPipeline[{Lookup["a"]}][Association[]]]
		,
		True
		,
		TestID->"Test-7c84cae5-863d-4e1b-a0c7-d2bc4c854efd"
	],

	TestCreate[
		DataPipeline[{"a" -> Lookup["x"]},  {"Input" -> "a"}][Association["x" -> 1]]
		,
		1
		,
		TestID->"Test-6d1093ea-1613-4dc4-9c5e-eb6a882af0dd"
	],

	TestCreate[
		FailureQ[DataPipeline[{"a" -> Lookup["x"]},  {"Input" -> "a"}][Association["y" -> 1]]]
		,
		True
		,
		TestID->"Test-2e6ea717-1b6a-48f8-8d51-2c0afdff2497"
	],

	TestCreate[
		FailureQ[DataPipeline[{"a" -> Lookup["a"]},  {"x" -> "a"}][Association["x" -> 1]]]
		,
		True
		,
		TestID->"Test-d9be00d4-b31b-4ddd-997a-1e79be8efc1b"
	],

	TestCreate[
		FailureQ[DataPipeline[{"a" -> Lookup["a"]},  {"x" -> "a"}][Association["y" -> 1]]]
		,
		True
		,
		TestID->"Test-f3b91dcb-f148-4b7f-b839-857bf88326d7"
	],

	TestCreate[
		DataPipeline[{1 & }][]
		,
		1
		,
		TestID->"Test-4dfa7bcb-c0fc-4e7a-ba52-6da201a3cbc2"
	],

	TestCreate[
		FailureQ[DataPipeline[{Missing[] & }][]]
		,
		True
		,
		TestID->"Test-0638ac79-6a85-43ff-bb53-1ac183537a75"
	],

	TestCreate[
		DataPipeline[
	{"rand1"->(1&), "rand2"->("y"&), "c"->Identity},
	{{"rand1", "rand2", "Input"}->"c"}
	][5]
		,
		{1,  "y", 5}
		,
		TestID->"Test-121a3d22-fe25-4781-bfd9-57ece3c0e4a0"
	],

	TestCreate[
		DataPipeline[
	{"rand1"->(Missing[]&), "rand2"->("y"&), "c"->Identity},
	{{"rand1", "rand2", "Input"}->"c"}
	][5]//FailureQ
		,
		True
		,
		TestID->"Test-990633f1-aa3b-410c-bcf2-350be9e93910"
	],

	TestCreate[
		DataPipeline[
	{"rand1"->(Missing[]&), "rand2"->("y"&), "c"->Identity},
	{{"rand1", "rand2"}->"c"}
	][]//FailureQ
		,
		True
		,
		TestID->"Test-7ef9dd9c-f5cf-48ae-9219-630e980ae1f3"
	],

	TestCreate[
		DataPipeline[{}]["something"]
		,
		"something"
		,
		TestID->"Test-1c4b5f4f-4eba-401d-acc8-1b983b8dc77f"
	],

	TestCreate[
		FailureQ[DataPipeline[{}][Missing[]]]
		,
		True
		,
		TestID->"Test-ddece7e4-2390-45a2-b37a-6cba0d9d204e"
	],

	TestCreate[
		DataPipeline[{"a" -> Select[EvenQ]},  {}]["something"]
		,
		"something"
		,
		TestID->"Test-01c882bb-0a4c-4b42-b993-d18ab88f9a30"
	],

	TestCreate[
		FailureQ[DataPipeline[{"a" -> Select[EvenQ]},  {}][Missing[]]]
		,
		True
		,
		TestID->"Test-c45f75bb-9fce-4fe6-b194-d4675f3ffec0"
	],

	TestCreate[
		pipeline = DataPipeline[{Select[PrimeQ],  Map[#1^2 & ], Total}]
		,
		DataPipeline[{Select[PrimeQ],  Map[#1^2 & ], Total}]
		,
		TestID->"Test-d3ce65fc-d25e-45c4-b00e-6c81b155e1ae"
	],

	TestCreate[
		pipeline[Range[10]]
		,
		87
		,
		TestID->"Test-3e3c8e1b-e809-4079-974c-b79380eff1f5"
	],

	TestCreate[
		GraphQ[Information[pipeline,  "Graph"]]
		,
		True
		,
		TestID->"Test-0c63d1bf-2ec2-4243-acc5-0185b73d2ba3"
	],

	TestCreate[
		DataPipeline[{Flatten,  pipeline, Framed}][{{1},  {2,  3}, {4,  5, 6}}]
		,
		Framed[38]
		,
		TestID->"Test-908659a4-7277-432d-8cd2-fc003db5db0d"
	],

	TestCreate[
		FailureQ[DataPipeline[{Select[EvenQ],  Total, 1/#1 & , Sqrt}][Range[-6,  6]]]
		,
		True
		,
		TestID->"Test-3160ae9f-9f92-417c-b26c-6bed9fda47a0"
	],

	TestCreate[
		GraphQ[Information[DataPipeline[{"a" -> Select[PrimeQ],  "b" -> Map[#1^2 & ], "c" -> Total}],  "Graph"]]
		,
		True
		,
		TestID->"Test-1270b689-8da6-46f7-9939-c0990476daa7"
	],

	TestCreate[
		pipeline1 = DataPipeline[
	{"a"->Select[EvenQ], "b"->Select[PrimeQ], "c"->Catenate, "d"->Total}, {"Input"->"a", "Input"->"b", {"a", "b"}->"c", "c"->"d"}
	];
	GraphQ[Information[pipeline1, "Graph"]]
		,
		True
		,
		TestID->"Test-c80b582a-da62-4568-a9d7-574697df607c"
	],

	TestCreate[
		pipeline1[Range[10]]
		,
		47
		,
		TestID->"Test-8fb3cb21-eac4-4da0-9000-6135d3710d77"
	],

	TestCreate[
		Information[
	DataPipeline[{"a"->Select[EvenQ], "b"->Select[PrimeQ], "c"->Catenate, "d"->Total}, {"Input"->"a", "Input"->"b", "a"->"c", "b"->"c",  "c"->"d"}
	], "Graph"]//GraphQ
		,
		True
		,
		TestID->"Test-e9e11e2c-4277-406b-964c-2126b9371cd1"
	],

	TestCreate[
		pipeline2 = DataPipeline[
	{"a"->Select[EvenQ], "b"->Select[PrimeQ], "c"->Catenate, "d"->Total},
	{"x"->"a", "y"->"b", {"a", "b"}->"c",  "c"->"d"}
	];
	pipeline2@<|
	"x"->{1, 2, 3, 4, 5, 6, 7, 8, 9, 10},
	"y"->{37, 60, 80, 18, 61}
	|>
		,
		128
		,
		TestID->"Test-a98d9194-6fe7-41c4-b65d-777552067cd9"
	],

	TestCreate[
		GraphQ[Information[pipeline2,  "Graph"]]
		,
		True
		,
		TestID->"Test-8b66f873-da8a-4b04-97cc-42896d9b7002"
	],

	TestCreate[
		pipeline3 =DataPipeline[
	{"a"->Select[EvenQ], "b"->Select[PrimeQ], "c"->Catenate, "d"->Total,  "e"->Mean},
	{"x"->"a", "y"->"b", {"a", "b"}->"c",  "c"->"d","c"->"e"}
	];
	pipeline3@<|
	"x"->{1, 2, 3, 4, 5, 6, 7, 8, 9, 10},
	"y"->{37, 60, 80, 18, 61}
	|>
		,
		Association["d" -> 128,  "e" -> 128/7]
		,
		TestID->"Test-95cf2382-7642-44ec-9449-6b60898188a4"
	],

	TestCreate[
		GraphQ[Information[pipeline3,  "Graph"]]
		,
		True
		,
		TestID->"Test-bd403b0b-8044-40f4-a0a5-f551dd4529c5"
	],

	TestCreate[
		pipeline1 = DataPipeline[
	{"a"->Select[EvenQ], "b"->Select[PrimeQ], "c"->Catenate, "d"->Total},
	{"x"->"a", "y"->"b", {"a", "b"}->"c",  "c"->"d"}
	]
		,
		DataPipeline[{"a" -> Select[EvenQ],  "b" -> Select[PrimeQ], "c" -> Catenate, "d" -> Total},
	{"x" -> "a",  "y" -> "b", {"a",  "b"} -> "c", "c" -> "d"}]
		,
		TestID->"Test-cbbd799d-477a-4be0-a8a1-128686e0675f"
	],

	TestCreate[
		pipeLine2=DataPipeline[
	{
	"x" -> Function[Range[#]],
	"y"->Function[Range[#^3]],
	"pipe" -> pipeline1,
	"out"->Framed
	},
	{"Input"->"x", "Input"->"y", KeyTake[{"x", "y"}]->"pipe", "pipe"->"out"}
	]
		,
		DataPipeline[{"x" -> (Range[#1] & ),  "y" -> (Range[#1^3] & ),
	"pipe" -> DataPipeline[{"a" -> Select[EvenQ],  "b" -> Select[PrimeQ], "c" -> Catenate, "d" -> Total},
	{"x" -> "a",  "y" -> "b", {"a",  "b"} -> "c", "c" -> "d"}], "out" -> Framed},
	{"Input" -> "x",  "Input" -> "y", KeyTake[{"x",  "y"}] -> "pipe", "pipe" -> "out"}]
		,
		TestID->"Test-8a29e4c9-4f07-46a8-bd4c-c0454e9b4e67"
	],

	TestCreate[
		pipeLine2[5]
		,
		Framed[1599]
		,
		TestID->"Test-5cd1ad0b-88d8-42a4-9713-79ea0e6f0792"
	],

	TestCreate[
		randomPipeline = DataPipeline[{RandomReal[] & , Exp}]
		,
		DataPipeline[{RandomReal[] & , Exp}]
		,
		TestID->"Test-506a415b-e236-49e6-b9af-e32048771c19"
	],

	TestCreate[
		MatchQ[{__?NumericQ}][Table[randomPipeline[],  5]]
		,
		True
		,
		TestID->"Test-9149e448-df56-41ec-a44e-346d9ca1f74c"
	],

	TestCreate[
		randomNetwork=DataPipeline[
	{"rand1"->(RandomReal[]&), "rand2"->(RandomChoice[{"x", "y"}]&),
	"a"->Exp,
	"b"->Function[# <> #],
	"c"->Identity
	},
	{"rand1"->"a", "rand2"->"b", {"a", "b"}->"c"}
	]
		,
		DataPipeline[{"rand1" -> (RandomReal[] & ),  "rand2" -> (RandomChoice[{"x",  "y"}] & ), "a" -> Exp,
	"b" -> (StringJoin[#1,  #1] & ), "c" -> Identity},  {"rand1" -> "a",  "rand2" -> "b", {"a",  "b"} -> "c"}]
		,
		TestID->"Test-63c88370-cd26-4889-bd30-c0c377e60849"
	],

	TestCreate[
		MatchQ[{_?NumericQ,  _String}][randomNetwork[]]
		,
		True
		,
		TestID->"Test-9415afc6-b413-47b1-8e5a-7b00815d5dfb"
	],

	TestCreate[
		randomNetwork[Association["rand1" -> -1,  "rand2" -> "z"]]
		,
		{1/E,  "zz"}
		,
		TestID->"Test-29f02505-0894-43df-bf64-c23e00ae2271"
	],

	TestCreate[
		MatchQ[{_?NumericQ,  "zz"}][randomNetwork[Association["rand2" -> "z"]]]
		,
		True
		,
		TestID->"Test-a9684fb4-9b6c-4ad6-8cbc-4337a9e05ede"
	],

	TestCreate[
		FailureQ[DataPipeline[{Lookup["a"],  Total}][Association["b" -> 1]]]
		,
		True
		,
		TestID->"Test-4d59ff87-b4ef-421a-ba82-c4ca4679e8f3"
	],

	TestCreate[
		MissingQ[DataPipeline[{Lookup["a"],  Query[Total]},  "FailureDetection" -> None][Association["b" -> 1]]]
		,
		True
		,
		TestID->"Test-cb0c7481-e328-4d3b-9d9b-696a17dd20cd"
	],

	TestCreate[
		FailureQ[DataPipeline[{Lookup["a"],  Failsafe[Total,  ListQ]},  "FailureDetection" -> None][Association["b" -> 1]]]
		,
		True
		,
		TestID->"Test-5a2830a8-4f86-49d5-a4a0-aa0aeb2da8c3"
	],

	TestCreate[
		FailureQ[DataPipeline[{Lookup["a"],  Log}][Association["a" -> 0]]]
		,
		True
		,
		TestID->"Test-4e0238cc-9a37-49ec-be3b-19c8d45b0525"
	],

	TestCreate[
		DataPipeline[{Lookup["a"],  Log},  "FailureDetection" -> MissingQ][Association["a" -> 0]]
		,
		-Infinity
		,
		TestID->"Test-543ee22e-889f-46e6-9233-2ab1403fa36a"
	],

	TestCreate[
		FailureQ[DataPipeline[{Extract[3],  Sqrt}][{1,  2}]]
		,
		True
		,
		TestID->"Test-33433996-f342-4e21-8729-3ad672c1dd14"
	],

	TestCreate[
		DataPipeline[{Extract[3],  Sqrt},  "CatchMessages" -> False][{1,  2}]
		,
		Quiet @ Sqrt[Extract[3][{1,  2}]]
		,
		{Extract::partw}
		,
		TestID->"Test-e8483e72-33c8-4fd7-b09c-b67cdf5cc9f8"
	]
}

End[] (* End Private Context *)

EndPackage[]
