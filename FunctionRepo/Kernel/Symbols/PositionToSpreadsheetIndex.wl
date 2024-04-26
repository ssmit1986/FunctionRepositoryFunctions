(* Wolfram Language Package *)

BeginPackage["FunctionRepo`SpreadsheetIndexToPosition`", {"FunctionRepo`"}]
(* Exported symbols added here with SymbolName::usage *)
GeneralUtilities`SetUsage[PositionToSpreadsheetIndex,
	"PositionToSpreadsheetIndex[{row$, col$}] converts a Wolfram position to a spreadsheet cell designation."
];

Begin["`Private`"] (* Begin Private Context *)

PositionToSpreadsheetIndex[p_] := Module[{
	pos = p,
	row, col,
	tag
},
	Enclose[
		If[ IntegerQ[pos]
			,
			ConfirmAssert[Positive @ pos, Null, tag];
			ConfirmBy[numberToColumn[pos], StringQ, Null, tag]
			,
			{row, col} = ConfirmMatch[pos, {_Integer?Positive, _Integer?Positive}, Null, tag];
			ConfirmBy[
				StringJoin[
					numberToColumn[col],
					IntegerString[row]
				],
				StringQ,
				Null,
				tag
			]
		],
		Identity,
		tag
	]
];

alphabet = ToUpperCase[Alphabet[]];
nA = Length @ alphabet;
qm[m_, n_, d_] := {
	Quotient[m, n, d],
	Mod[m, n, d]
};

numberToColumn[n_] := numberToColumn[n] = StringJoin @ Part[
	alphabet,
	Replace[
		FixedPoint[
			Replace[
				#,
				i_Integer?(GreaterThan[nA]) :> Splice[qm[i, nA, 1]],
				{1}
			]&,
			{n}
		],
		{Repeated[0], rest___} :> rest
	]
];

End[] (* End Private Context *)

EndPackage[]
