(* Wolfram Language Package *)

BeginPackage["FunctionRepo`DataPipeline`", {"FunctionRepo`"}]
(* Exported symbols added here with SymbolName::usage *)
GeneralUtilities`SetUsage[DataPipeline,
	"DataPipeline[{operator1$, operator2$, ...}][data$] applies a sequence of operators to data$. If an operator fails or does not return valid intermediary data, the pipeline stops and returns a failure."
];

Begin["`Private`"] (* Begin Private Context *)

dataPattern = _List | _Dataset | _Association?AssociationQ | _Tabular?TabularQ;
dataQ[data_] := MatchQ[data, dataPattern];

(* Data pipeline *)

(* Linear pipeline *)
DataPipeline[{}][anything_] := anything;
DataPipeline[{el_}][data : dataPattern] := el[data];
DataPipeline[{el_, rest__}][data : dataPattern] := DataPipeline[{rest}][el[data]];
DataPipeline[op : Except[_List]][_] := Failure["InvalidOperator", <|"Head" -> Head[op]|>];


(* Network pipeline *)

vertexKeyValPattern = {(_String -> _)...} | _Association?(AssociationQ[#] && AllTrue[Keys[#], StringQ]&);
edgeKeyValPattern = {((_String -> _String) | ({__String} -> _String))...};

DataPipeline[_, {}][anything_] := anything;
DataPipeline[vertList : vertexKeyValPattern, edges : edgeKeyValPattern][data : dataPattern] := If[
	AssociationQ[data],
	Replace[
		iDataPipeline[vertList, edges][data],
		a_Association :> KeyTake[a, Keys[vertList]] (* Only keep the computed keys *)
	],
	Replace[
		iDataPipeline[vertList, edges][<|"Input" -> data|>],
		a_Association :> a["Output"]
	]
];

iDataPipeline[_, {}][data_] := data;
iDataPipeline[vertices_, {(keyIn : _String | {__String}) -> keyOut_, rest___}][data_] := With[{
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
			op[input]
	]
},
	If[ !FailureQ[newData],
		iDataPipeline[vertices, {rest}] @ Append[
			data,
			keyOut -> newData
		],
		newData
	]
];

(* Failure handling *)
DataPipeline[__][fail_?FailureQ] := fail;
DataPipeline[__][other : Except[dataPattern]] := Failure["InvalidData", <|"Head" -> Head[other]|>];

End[] (* End Private Context *)

EndPackage[]
