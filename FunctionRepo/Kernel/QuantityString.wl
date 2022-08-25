(* Wolfram Language Package *)

BeginPackage["FunctionRepo`QuantityString`", {"FunctionRepo`"}]
(* Exported symbols added here with SymbolName::usage *)
GeneralUtilities`SetUsage[QuantityString,
	"QuantityString[q$] creates a linear string that resembles the way that q$ is typeset in the FrontEnd.
QuantityString[q$, template$] uses a template to typeset the output."
];

Begin["`Private`"] (* Begin Private Context *)

QuantityString[q_?QuantityQ] := With[{
	boxes = ToBoxes[q, StandardForm]
},
	Replace[
		boxes,
		{
			box : TemplateBox[list : {__}, tag_String, ___] :> Replace[
				UsingFrontEnd[CurrentValue[{StyleDefinitions, tag, TemplateBoxOptions, DisplayFunction}]],
				{
					f_Function :> boxesToString[
						(* Add space between the magnitude and the units *)
						f @@ MapAt[RowBox[{# , " "}]&, list, {1}]
					],
					_ -> $Failed
				}
			],
			_ -> $Failed
		}
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

quantityElementStrings[q_] := With[{
	strings = Replace[
		ToBoxes[q, StandardForm],
		{
			TemplateBox[
				{_, args__, canonical_},
				_String?(StringStartsQ["Quantity"]),
				___
			] :> {
				toInputString[QuantityMagnitude[q]],
				Sequence @@ Map[boxesToString, {args}],
				canonicalToString[canonical]
			},
			_ -> $Failed
		}
	]
},
	If[ MatchQ[strings, {__String}],
		strings,
		$Failed
	]
];

toInputString[expr_] := ToString[Unevaluated @ expr, InputForm];

canonicalToString[boxes_] := ToExpression[
	boxes,
	StandardForm,
	Function[expr, StringDelete[toInputString[Unevaluated[expr]], "\""], HoldAllComplete]
];

stringReduce[str_String] := ReplaceRepeated[str,
	s_String :> StringReplace[s,
		{
			WhitespaceCharacter... ~~ "\[InvisibleSpace]" ~~ WhitespaceCharacter... -> "",
			WhitespaceCharacter.. ~~ ")" :> ")",
			"(" ~~ WhitespaceCharacter.. :> "(",
			WhitespaceCharacter.. -> " ",
			"\"" -> "",
			n : NumberString ~~ "`" :> n,
			"^" ~~ primes : "\[Prime]".. :> StringRepeat["'", StringLength[primes]]
		}
	]
];

$parenthesesExceptions = Alternatives[
	"",
	LetterCharacter..,
	NumberString,
	"(" ~~ ___ ~~ ")"
];

addParentheses[s_String] := With[{
	trimmed = StringTrim[StringDelete[s, "\""]]
},
	If[ StringMatchQ[trimmed, $parenthesesExceptions],
		trimmed,
		"(" <> trimmed <> ")"
	]
];

cleanupBoxes[boxes_] := boxes //. {
	(StyleBox | TagBox | FormBox | InterpretationBox | TooltipBox | PanelBox)[b_, ___] :> b,
	SqrtBox[b_] :> RadicalBox[b, "2"],
	RadicalBox[b_, n_] :> SuperscriptBox[b, FractionBox["1", n]],
	SuperscriptBox["\[Null]", s_String /; StringMatchQ[s, "\[Prime]"..]] :> StringRepeat["'", StringLength[s]]
};

deleteQuotes[expr_] := expr /. s_String :> StringDelete[s, "\""];

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

(*
boxesToString[str_String] := StringDelete["\""] @ str;

boxesToString[boxes_] := Replace[
	ReplaceRepeated[
		stripMetaBoxes[boxes],
		{
			SqrtBox[n_String] :> SuperscriptBox[n, "1/2"],
			RadicalBox[a_String, b_String] :> SuperscriptBox[a, addParentheses["1/" <> addParentheses[b]]],
			SuperscriptBox[s_String, n_String] :> addParentheses[s] <> "^" <> addParentheses[n],
			FractionBox[p_String, q_String] :> addParentheses[addParentheses[p] <> "/" <> addParentheses[q]],
			RowBox[strings : {___String}] :> StringDelete[StringJoin[strings], "\""]
		}
	],
	{
		s_String :> StringTrim @ stringReduce[s],
		_ -> $Failed
	}
];
*)

End[] (* End Private Context *)

EndPackage[]
