(* Wolfram Language Package *)

BeginPackage["FunctionRepo`ToExpressionExpect`", {"FunctionRepo`", "GeneralUtilities`"}]
(* Exported symbols added here with SymbolName::usage *)
GeneralUtilities`SetUsage[ToExpressionExpect, 
    "ToExpressionExpect[input$, patt$] interprets input as an expression and then, before evaluating it, checks that it matches patt$ before evaluating and returning it.
ToExpressionExpect[input$, patt$, form$] uses interpretation rules corresponding to the specified form.
ToExpressionExpect[input$, patt$, form$, h$] wraps the head h$ around the expression produced before evaluating it."
];

Begin["`Private`"] (* Begin Private Context *) 

ToExpressionExpect[input_, patt_, form_, HoldComplete] := iToExpressionExpect[input, patt, form];

ToExpressionExpect[input_, patt_, form_, h_] := handleOutput[
	iToExpressionExpect[input, patt, form],
	h
];

ToExpressionExpect[input_, patt_, form_] := handleOutput @ iToExpressionExpect[input, patt, form];

ToExpressionExpect[input_, patt_] := handleOutput @ iToExpressionExpect[input, patt, Automatic];



handleOutput[HoldComplete[expr_]] := expr;
handleOutput[HoldComplete[expr_], h_] := h[expr];
handleOutput[failure_, ___] := failure;

iToExpressionExpect[input_String?StringQ, patt_, Automatic] := iToExpressionExpect[input, patt, InputForm];
iToExpressionExpect[input_, patt_, Automatic] := iToExpressionExpect[input, patt, StandardForm];
iToExpressionExpect[input_, patt_, form_] := With[{
	try = ToExpression[input, form, HoldComplete]
},
	Replace[try,
		{
			_?FailureQ :> Failure["InterpretationFailure",
				<|
					"MessageTemplate" -> "Input could not be interpreted as a valid expression.",
					"MessageParameters" -> <||>
				|>
			],
			Except[HoldComplete[patt]] :> Failure["MatchFailure",
				<|
					"MessageTemplate" -> "Interpreted expression did not match `Pattern`",
					"MessageParameters" -> <|"Pattern" -> patt|>
				|>
			]
		}
	]
];

End[] (* End Private Context *)

EndPackage[]