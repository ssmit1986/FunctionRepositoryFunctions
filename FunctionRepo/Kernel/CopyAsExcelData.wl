(* Wolfram Language Package *)

BeginPackage["FunctionRepo`CopyAsExcelData`", {"FunctionRepo`"}]
(* Exported symbols added here with SymbolName::usage *)
GeneralUtilities`SetUsage[CopyAsExcelData,
	"CopyAsExcelData[list$] copies data in a way such that it can be pasted into Excel."
];

Begin["`Private`"] (* Begin Private Context *) 

CopyAsExcelData[list_?VectorQ] := CopyAsExcelData[{list}];

CopyAsExcelData[{}] := $Failed;

CopyAsExcelData[data2D : {__List?VectorQ}] := Module[{
	dataToCopy = ExportString[
		Map[cleanup, data2D, {2}],
		"Table"
	]
},
	CopyToClipboard[dataToCopy];
	dataToCopy
];

cleanup[normal : _String | _?NumericQ] := normal;
cleanup[date_DateObject?DateObjectQ] := DateString[date, "ISODateTime"];
cleanup[other_] := TextString[other];

End[] (* End Private Context *)

EndPackage[]