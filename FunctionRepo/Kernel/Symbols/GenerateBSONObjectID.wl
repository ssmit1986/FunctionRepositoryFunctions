(* Wolfram Language Package *)

BeginPackage["FunctionRepo`GenerateBSONObjectID`", {"FunctionRepo`"}]
(* Exported symbols added here with SymbolName::usage *)
GeneralUtilities`SetUsage[GenerateBSONObjectID,
	"GenerateBSONObjectID[] generates a unique BSONObjectID[...] object for MongoDB.
GenerateBSONObjectID[n$] generates n$ unique IDs."
];

Begin["`Private`"] (* Begin Private Context *)

Needs["MongoLink`" -> None];

randomHexValue[n_] := RandomInteger[{0, 16^n - 1}];

hexStringFromExpression[None | Null | "", n_] := randomHexValue[n];
hexStringFromExpression[expr_, n_] := With[{
	int = Replace[
		expr,
		{
			i_Integer :> i,
			s_String :> With[{
				digits = StringDelete[s, Except[DigitCharacter]]
			},
				FromDigits[digits] /; digits =!= ""
			],
			_ :> $Failed
		}
	]
},
	If[ FailureQ[int],
		StringPadLeft[Hash[expr, Automatic, "HexString"], n, "0"],
		IntegerString[int, 16, n]
	]
];

$counter := $counter = CreateDataStructure["Counter", randomHexValue[6]];

$machineID := $machineID = hexStringFromExpression[$MachineID, 6];

$processID := $processID = hexStringFromExpression[$ProcessID, 4];

incrementHexString[counter_, inc_Integer, ndigits_Integer] := With[{
	i = counter["AddTo", inc]
},
	IntegerString[i, 16, ndigits]
];

GenerateBSONObjectID[] := MongoLink`BSONObjectID @ StringJoin[
	IntegerString[UnixTime[], 16, 8], (* Timestamp *)
	$machineID, (* Machine identifier *)
	$processID, (* Process identifier *)
	incrementHexString[$counter, 1, 6] (* auto-incrementing 6-digit hex counter *)
];

GenerateBSONObjectID[n_Integer] := Table[GenerateBSONObjectID[], n];


End[] (* End Private Context *)

EndPackage[]
