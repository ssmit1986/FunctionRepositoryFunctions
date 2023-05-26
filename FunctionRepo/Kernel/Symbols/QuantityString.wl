(* Wolfram Language Package *)

BeginPackage["FunctionRepo`QuantityString`", {"FunctionRepo`"}]
(* Exported symbols added here with SymbolName::usage *)
GeneralUtilities`SetUsage[QuantityString,
	"QuantityString[q$] attempts to create an abbreviated form of the canonical representation of the unit of q$.
QuantityString[q$, \"BoxForm\"] creates a linear string that resembles the way that q$ is typeset in the FrontEnd.
QuantityString[q$, template$] uses a template to typeset the output."
];

Begin["`Private`"] (* Begin Private Context *)

QuantityString[q_] := QuantityString[q, "Canonical"];

QuantityString[q_?QuantityQ, "Canonical"] := With[{
	mag = QuantityMagnitude[q],
	unit = PowerExpand @ QuantityUnit[q]
},
	If[ And[
			Head[mag] === MixedMagnitude,
			Head[unit] === MixedUnit,
			Length[First[unit]] === Length[First[mag]]
		],
		StringRiffle @ MapThread[
			QuantityString @ Quantity[#1, #2]&,
			{
				First[mag],
				First[unit]
			}
		],
		StringTemplate["`1` `2`"][
			mag,
			canonicalUnitShort[unit]
		]
	]
];

QuantityString[q_?QuantityQ, "BoxForm"] := With[{
	boxes = ToBoxes[q, StandardForm]
},
	Catch[
		boxesToString @ ReplaceAll[
			boxes,
			{
				box : TemplateBox[list : {__}, tag_String /; StringContainsQ[tag, "Quantity"], ___] :> Replace[
					displayFunction[tag],
					{
						(* Add space between the magnitude and the units *)
						f_Function :> f @@ MapAt[RowBox[{# , " "}]&, list, {1}],
						_ :> Throw[$Failed, $quantityTemplateBoxTag]
					}
				]
			}
		],
		$quantityTemplateBoxTag
	]
];

QuantityString[q_?QuantityQ, template_] := With[{
	quantityElements = quantityElementStrings[q]
},
	Replace[
		quantityElements,
		{
			list : {__String} :> TemplateApply[
				template,
				list
			],
			_ -> $Failed
		}
	]
];

QuantityString[___] := $Failed;

displayFunction[tag_String] := With[{
	try = UsingFrontEnd[CurrentValue[{StyleDefinitions, tag, TemplateBoxOptions, DisplayFunction}]]
},
	If[ Head[try] === Function,
		displayFunction[tag] = try,
		$Failed
	]
];

quantityElementStrings[q_] := With[{
	strings = Replace[
		ToBoxes[q, StandardForm],
		{
			TemplateBox[
				{_, args__, canonical_},
				_String?(StringStartsQ["Quantity"]),
				___
			] :> With[{
				unit = PowerExpand @ QuantityUnit[q]
			},
				{
					toInputString[QuantityMagnitude[q]],
					Sequence @@ Map[boxesToString, {args}],
					canonicalUnitToString[unit],
					canonicalUnitShort[unit]
				}
			],
			_ -> $Failed
		}
	]
},
	If[ MatchQ[strings, {__String}],
		strings,
		$Failed
	]
];

deleteQuotes[expr_] := expr /. s_String :> StringDelete[s, "\""];

toInputString[expr_] := deleteQuotes @ ToString[
	expr,
	InputForm
];

cleanupInputStrings[expr_] := expr //. s_String :> StringReplace[s,
	{
		"*" -> " ",
		"Sqrt[" ~~ root : Shortest[__] ~~ "]" :> root <> "^(1/2)"
	}
];

canonicalUnitToString[unit_] := cleanupInputStrings @ toInputString[
	Replace[unit, MixedUnit[l_List] :> l]
];

canonicalUnitShort[MixedUnit[l_List]] := toInputString[canonicalUnitShort /@ l];

canonicalUnitShort[DatedUnit[unit_, date_]] := StringTemplate["`1` (`2` `3`)"][
	canonicalUnitShort[unit],
	canonicalUnitToString[unit],
	DateString[DateObject[date], "ISODate"]
];

canonicalUnitShort[unit_] := cleanupInputStrings @ toInputString[
	ReplaceAll[
		unit,
		s_String :> StringTrim[
			StringReplace[ToString @ QuantityForm[s, "Abbreviation"], WhitespaceCharacter.. -> " "]
		]
	]
];

cleanupBoxes[boxes_] := boxes //. {
	(StyleBox | TagBox | FormBox | InterpretationBox | TooltipBox | PanelBox)[b_, ___] :> b,
	SqrtBox[b_] :> RadicalBox[b, "2"],
	RadicalBox[b_, n_] :> SuperscriptBox[b, FractionBox["1", n]],
	SuperscriptBox["\[Null]", s_String /; StringMatchQ[s, "\[Prime]"..]] :> StringRepeat["'", StringLength[s]]
};

$ignorableSpaceCharacter = Alternatives[
	"\[NegativeVeryThinSpace]", "\[NegativeThinSpace]",
	"\[NegativeMediumSpace]", "\[NegativeThickSpace]",
	"\[VeryThinSpace]"
];

replaceWhitespace[expr_] := expr /. s_String :> StringReplace[
	s,
	{
		$ignorableSpaceCharacter -> "\[InvisibleSpace]",
		Except["\[InvisibleSpace]", WhitespaceCharacter] :> " "
	}
];

exportBoxes[boxes_] := Replace[
	UsingFrontEnd[
		FrontEndExecute[
			FrontEnd`ExportPacket[BoxData[boxes], "PlainText"]
		]
	],
	{
		{s_String, __} :> s,
		_ -> $Failed
	}
];

boxesToString[boxes_] := StringReplace[
	StringTrim @ exportBoxes[
		replaceWhitespace @ deleteQuotes @ cleanupBoxes[boxes]
	],
	{
		WhitespaceCharacter.. ~~ "'" :> "'",
		WhitespaceCharacter.. :> " "
	}
];

End[] (* End Private Context *)

EndPackage[]
