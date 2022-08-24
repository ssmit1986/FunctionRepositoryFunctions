(* Wolfram Language Package *)

BeginPackage["FunctionRepo`QuantityString`", {"FunctionRepo`"}]
(* Exported symbols added here with SymbolName::usage *)
GeneralUtilities`SetUsage[QuantityString,
	"QuantityString[q$] creates a linear string that resembles the way that q$ is typeset in the FrontEnd.
QuantityString[q$, template$] uses a StringTemplate to typeset the output string."
];

Begin["`Private`"] (* Begin Private Context *)

StringTemplate["`Magnitude` `Abbreviation`"]

QuantityString[q_] := QuantityString[q, Automatic];

QuantityString[q_?QuantityQ, template_] := With[{
	quantityElements = quantityElementStrings[q]
},
	Replace[
		quantityElements,
		{
			assoc_?AssociationQ :> TemplateApply[
				autoTemplate[template],
				{assoc["FormStrings"], assoc["Tag"]}
			],
			_ -> $Failed
		}
	]
];

QuantityString[___] := $Failed;

autoTemplate[Automatic] :=
	TemplateIf[ StringContainsQ[TemplateSlot[2], "Prefix"],
		StringTemplate["`Abbreviation``Magnitude`"],
		StringTemplate["`Magnitude` `Abbreviation`"]
	];

autoTemplate[other_] := other;

removeNonstandardCharacters[expr_] := ToString[expr, CharacterEncoding -> "ASCII"];

quantityElementStrings[q_] := With[{
	mag = QuantityMagnitude[q],
	boxes = Replace[
		ToBoxes[q, StandardForm],
		{
			TemplateBox[
				{_, short_, long_, canonical_},
				templateTag_String?(StringStartsQ["Quantity"]),
				___
			] :> {
				templateTag,
				{
					shortBoxesToString[short],
					ToString[long], (* Should be a string anyway *)
					canonicalToString[canonical]
				}
			},
			_ -> $Failed
		}
	]
},
	If[ MatchQ[boxes, {_String, {Repeated[_String, {3}]}}],
		<|
			"Tag" -> boxes[[1]],
			"FormStrings" -> Append[
				AssociationThread[
					{"Abbreviation", "LongForm", "CanonicalForm"},
					boxes[[2]]
				],
				"Magnitude" -> mag
			]
		|>,
		$Failed
	]
];

toInputString[expr_] := ToString[Unevaluated @ expr, InputForm];

canonicalToString[boxes_] := ToExpression[
	boxes,
	StandardForm,
	Function[expr, StringDelete[toInputString[Unevaluated[expr]], "\""], HoldAllComplete]
];

shortBoxesToString[boxes_] := Replace[
	ReplaceRepeated[
		boxes,
		{
			(StyleBox | TagBox | FormBox | InterpretationBox)[b_, ___] :> b,
			SuperscriptBox[s_, n_] :> ToString[s] <> "^" <> ToString[n] <> " ",
			RowBox[strings : {___String}] :> StringDelete[StringJoin[strings], "\""]
		}
	],
	{
		s_String :> StringTrim @ StringReplace[s,
			{
				"\[InvisibleSpace]" -> "",
				WhitespaceCharacter.. ~~ ")" :> ")",
				"(" ~~ WhitespaceCharacter.. :> "(",
				WhitespaceCharacter -> " ",
				"\"" -> ""
			}
		],
		_ -> $Failed
	}
];

End[] (* End Private Context *)

EndPackage[]
