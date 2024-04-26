(* Wolfram Language Package *)

BeginPackage["FunctionRepo`SpreadsheetIndexToPosition`", {"FunctionRepo`"}]
(* Exported symbols added here with SymbolName::usage *)
GeneralUtilities`SetUsage[SpreadsheetIndexToPosition,
	"SpreadsheetIndexToPosition[str$] converts a spreadsheet cell designation to a Wolfram position specification."
];

Begin["`Private`"] (* Begin Private Context *)

SpreadsheetIndexToPosition[s_] := Module[{str = s, tag},
	Enclose[
		ConfirmAssert[
			And[
				StringQ[str],
				StringMatchQ[str, LetterCharacter.. ~~ DigitCharacter...]
			],
			Null,
			tag
		];
		If[ StringContainsQ[str, DigitCharacter]
			,
			ConfirmMatch[
				Replace[
					StringSplit @ StringReplace[
						str,
						a : LetterCharacter ~~ b : DigitCharacter :> a <> " " <> b
					],
					{col_, row_} :> {
						FromDigits[row],
						columnToNumber[col]
					}
				],
				{_Integer, _Integer},
				Null,
				tag
			]
			,
			ConfirmBy[columnToNumber[str], IntegerQ, Null, tag]
		],
		Identity,
		tag
	]
];

alphabet = ToUpperCase[Alphabet[]];
nA = Length @ alphabet;
letterToNumber = AssociationThread[alphabet, Range @ nA];

columnToNumber[s_String] := columnToNumber[s] = With[{
	list = Lookup[letterToNumber, Characters @ ToUpperCase[s]]
},
	list . Power[26, Range[Length[list] - 1, 0, -1]]
];

End[] (* End Private Context *)

EndPackage[]
