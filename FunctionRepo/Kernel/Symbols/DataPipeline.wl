(* Wolfram Language Package *)

BeginPackage["FunctionRepo`DataPipeline`", {"FunctionRepo`"}]
(* Exported symbols added here with SymbolName::usage *)
GeneralUtilities`SetUsage[DataPipeline,
	"DataPipeline[{operator1$, operator2$, ...}][data$] applies a sequence of operators to data$. If an operator fails or does not return valid intermediary data, the pipeline stops and returns a failure."
];

Begin["`Private`"] (* Begin Private Context *)

dataPattern = _List | _Dataset | _Association?AssociationQ | _Tabular?TabularQ;

(* Linear pipeline *)
DataPipeline[{}][anything_] := anything;
DataPipeline[{el_}][data : dataPattern] := el[data];
DataPipeline[{el_, rest__}][data : dataPattern] := DataPipeline[{rest}][el[data]];
DataPipeline[op : Except[_List]][_] := Failure["InvalidOperator", <|"Head" -> Head[op]|>];
DataPipeline[_][fail_?FailureQ] := fail;
DataPipeline[_][other : Except[dataPattern]] := Failure["InvalidData", <|"Head" -> Head[other]|>];

vertexKeyValPattern = {(_String -> _)...} | _Association?(AssociationQ[#] && AllTrue[Keys[#], StringQ]&);
edgeKeyValPattern = {((_String -> _) | ({__String} -> _))...};


(* Network pipeline *)
DataPipeLine[{} | <||>, _List][anything_] := anything;
DataPipeLine[vertList : vertexKeyValPattern, edges : edgeKeyValPattern][data : dataPattern] := iDataPipeLine[vertList, edges][data];

iDataPipeLine[verts_, {keyIn_String -> keyOut_, rest___}][data_Association] := iDataPipeLine[verts, {rest}] @ Append[
	data,
	keyOut -> Lookup[verts, keyIn][data[keyIn]]
];
iDataPipeLine[verts_, {keysIn_List -> keyOut_, rest___}][data_Association] := iDataPipeLine[verts, {rest}] @ Append[
	data,
	keyOut -> KeyTake[data, keysIn]
];

End[] (* End Private Context *)

EndPackage[]
