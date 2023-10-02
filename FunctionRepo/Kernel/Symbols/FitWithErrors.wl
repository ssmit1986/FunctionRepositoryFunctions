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
	(model : LinearModelFit | NonlinearModelFit)[dat_, args__, opts___?OptionQ],
	niter : _ : 20
] /; MemberQ[dat, _Around, 2] := Module[{
	data = dat,
	rest = {args},
	options = Flatten @ {opts},
	xdat, dataVals, wp, testWP, weights,
	n, dimIn, dummyVars, currentFit,
	yerrors, fitFun
},
	Enclose[
		n = Length[data];
		If[ VectorQ[data] && !MemberQ[data, _Rule],
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
		currentFit = ConfirmMatch[model[dataVals, Splice[rest, model], options], _FittedModel];
		options = FilterRules[options, Except[Weights]];
		yerrors = SetPrecision[errors[data[[All, -1]]]^2, wp];
		weights = ConfirmQuiet[
			Divide[1, totalVariance[yerrors, xdat, currentFit, dummyVars, wp]]
		];
		testWP = Max[5, wp - 3];
		fitFun = If[ model === NonlinearModelFit,
			Function[{w, fm},
				model[
					dataVals,
					rest[[1]],
					List @@@ fm["BestFitParameters"], (* Continue from the last best fit *)
					Splice[rest[[3 ;;]], model],
					Weights -> w,
					options,
					Method -> "Newton"
				]
			],
			Function[{w, fm},
				model[dataVals, Splice[rest, model], Weights -> w, options]
			]
		];
		FixedPoint[
			Function[
				currentFit = ConfirmMatch[fitFun[weights, currentFit], _FittedModel];
				weights = ConfirmQuiet[
					Divide[1, totalVariance[yerrors, xdat, currentFit, dummyVars, wp]]
				];
				SetPrecision[weights, testWP] (* Return lower precision weights for testsing convergence *)
			],
			SetPrecision[weights, testWP],
			niter
		];
		currentFit
	]
];

FitWithErrors[expr_, ___] := expr;

totalVariance[yVar_, xdat_, currentFit_, dummyVars_, wp_] := Plus[
	yVar,
	SetPrecision[errors[propagateXErrors[xdat, currentFit, dummyVars]]^2, wp]
];

propagateXErrors[xdat_, fit_, vars_] := With[{
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
