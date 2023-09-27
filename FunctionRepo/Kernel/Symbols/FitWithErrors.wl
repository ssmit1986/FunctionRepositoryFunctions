(* Wolfram Language Package *)

BeginPackage["FunctionRepo`FitWithErrors`", {"FunctionRepo`"}]
(* Exported symbols added here with SymbolName::usage *)
GeneralUtilities`SetUsage[FitWithErrors,
	"FitWithErrors[data$, fitFun$]..."
];

Begin["`Private`"] (* Begin Private Context *)

values[data_] := Uncertainty`AroundValue[data, "Value"];
errors[data_] := Uncertainty`AroundValue[data, "Uncertainty"];

SetAttributes[FitWithErrors, HoldFirst];

FitWithErrors[
	(model : LinearModelFit | NonlinearModelFit)[dat_, args__, opts___?OptionQ]
] /; MemberQ[dat, _Around, 2] := Module[{
	data = dat,
	rest = Splice[{args}, model],
	options = Flatten @ {opts},
	xdat, weights, dataVals, wp,
	n, dimIn, dummyVars, currentFit,
	yerrors
},
	Enclose[
		n = Length[data];
		If[ VectorQ[data], 
			data = Transpose[{Range[n], data}]
		];
		ConfirmAssert[MatrixQ[data] && !MemberQ[data, Around[_, _List], {2}]];
		dimIn = ConfirmBy[Dimensions[data][[2]] - 1, Positive];
		dummyVars = Array[\[FormalX], dimIn];
		wp = Lookup[options, WorkingPrecision, Automatic];
		If[ !NumericQ[wp],
			wp = Precision[data]
		];

		dataVals = SetPrecision[values[data], wp];
		xdat = Take[data, All, dimIn];
		currentFit = ConfirmMatch[model[dataVals, rest, options], _FittedModel];
		options = FilterRules[options, Except[Weights]];
		yerrors = SetPrecision[errors[data[[All, -1]]]^2, wp];
		FixedPoint[
			Function[
				weights = Plus[
					yerrors,
					SetPrecision[errors[propagateXErrors[currentFit, xdat, dummyVars]]^2, wp]
				];
				weights = ConfirmQuiet[Divide[1, weights]];
				currentFit = ConfirmMatch[model[dataVals, rest, Weights -> weights, options], _FittedModel];
				SetPrecision[weights, wp - 3]
			],
			Lookup[options, Weights],
			20
		];
		currentFit
	]
];

FitWithErrors[expr_] := expr;

propagateXErrors[fit_, xdat_, vars_] := With[{
	rules = Map[
		Thread[vars -> #]&,
		xdat
	],
	modelExpr = fit @@ vars
},
	Map[
		AroundReplace[modelExpr, #]&,
		rules
	]
];

End[] (* End Private Context *)

EndPackage[]
