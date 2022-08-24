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
			box : TemplateBox[l_List, tag_String, ___] :> Replace[
				UsingFrontEnd[CurrentValue[{StyleDefinitions, tag, TemplateBoxOptions, DisplayFunction}]],
				{
					f_Function :> boxesToString[f @@ l],
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

boxesToString[str_String] := StringDelete["\""] @ str;

boxesToString[boxes_] := Replace[
	ReplaceRepeated[
		boxes,
		{
			(StyleBox | TagBox | FormBox | InterpretationBox | TooltipBox | PanelBox)[b_, ___] :> b,
			SqrtBox[n_String] :> SuperscriptBox[n, "(1/2)"],
			RadicalBox[a_String, b_String] :> SuperscriptBox[a, "(1/" <> b <> ")"],
			SuperscriptBox[s_String, n_String] :> s <> "^" <> n <> " ",
			FractionBox[p_String, q_String] :> "(" <> p <> "/" <> q <> ")", (* Probably doesn't happen, but just in case *)
			RowBox[strings : {___String}] :> StringDelete[StringRiffle[strings], "\""]
		}
	],
	{
		s_String :> StringTrim @ StringReplace[s,
			{
				" \[InvisibleSpace] " -> "",
				"\[InvisibleSpace]" -> "",
				WhitespaceCharacter.. ~~ ")" :> ")",
				"(" ~~ WhitespaceCharacter.. :> "(",
				WhitespaceCharacter.. -> " ",
				"\"" -> "",
				n : NumberString ~~ "`" -> n
			}
		],
		_ -> $Failed
	}
];

End[] (* End Private Context *)

EndPackage[]
