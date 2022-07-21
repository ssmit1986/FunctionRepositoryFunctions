(* Wolfram Language Package *)

BeginPackage["FunctionRepo`ToExpressionMatched`", {"FunctionRepo`", "GeneralUtilities`"}]
(* Exported symbols added here with SymbolName::usage *)
GeneralUtilities`SetUsage[ToExpressionMatched, 
	"ToExpressionMatched[input$, patt$] interprets input as an expression and then, before further evaluating it, checks that it matches patt$ before evaluating and returning it.
ToExpressionMatched[input$, patt$, form$] uses interpretation rules corresponding to the specified form.
ToExpressionMatched[input$, patt$, form$, h$] wraps the head h$ around the expression produced before evaluating it."
];

Begin["`Private`"] (* Begin Private Context *) 

ToExpressionMatched[input_, patt_, form_, HoldComplete] := iToExpressionMatched[input, patt, form];

ToExpressionMatched[input_, patt_, form_, h_] := handleOutput[
	iToExpressionMatched[input, patt, form],
	h
];

ToExpressionMatched[input_, patt_, form_] := handleOutput @ iToExpressionMatched[input, patt, form];

ToExpressionMatched[input_, patt_] := handleOutput @ iToExpressionMatched[input, patt, Automatic];



handleOutput[HoldComplete[expr_]] := expr;
handleOutput[HoldComplete[expr_], h_] := h[expr];
handleOutput[failure_, ___] := failure;

iToExpressionMatched[input_String?StringQ, patt_, Automatic] := iToExpressionMatched[input, patt, InputForm];
iToExpressionMatched[input_, patt_, Automatic] := iToExpressionMatched[input, patt, StandardForm];
iToExpressionMatched[input_, patt_, form_] := With[{
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