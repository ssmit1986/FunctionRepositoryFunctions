(* Wolfram Language Package *)

BeginPackage["FunctionRepo`AsynchronousDynamicModule`", {"FunctionRepo`", "GeneralUtilities`"}]
(* Exported symbols added here with SymbolName::usage *)
GeneralUtilities`SetUsage[AsynchronousDynamicModule,
	"AsynchronousDynamicModule[DynamicModule[vars$, expr$, $$, Initialization :> init$, $$]] modifies a DynamicModule to accomodate slow initialization code."
];

Begin["`Private`"] (* Begin Private Context *) 

Attributes[handleUnsavedVariables] = {HoldAll};
handleUnsavedVariables[
	initVar_,
	opts1___,
	UnsavedVariables :> {vars___},
	opts2___
] := Hold[
	opts1,
	UnsavedVariables :> {initVar, vars},
	opts2
];
handleUnsavedVariables[initVar_, opts___] := Hold[
	UnsavedVariables :> {initVar},
	opts
];

Attributes[AsynchronousDynamicModule] = {HoldAll};

AsynchronousDynamicModule[
	DynamicModule[{vars___}, content_, opts1___, Initialization :> init_, opts2___], 
	placeHolder : _ : ProgressIndicator[Appearance -> "Necklace"]
] := iAsynchronousDynamicModule[{vars}, content,
	init,
	placeHolder,
	Evaluate @ Unique["Global`initQ"],
	opts1, opts2
];

AsynchronousDynamicModule[
	DynamicModule[{vars___}, content_, opts1___, Initialization :> init_, opts2___], 
	placeHolder : _ : ProgressIndicator[Appearance -> "Necklace"],
	initVar_Symbol
] := iAsynchronousDynamicModule[{vars}, content,
	init,
	placeHolder,
	initVar,
	opts1, opts2
];

Attributes[iAsynchronousDynamicModule] = {HoldAll};

iAsynchronousDynamicModule[
	{vars___},
	content_,
	init_,
	placeHolder_,
	initVar_Symbol,
	opts : OptionsPattern[DynamicModule]
] := Replace[
	handleUnsavedVariables[initVar, opts],
	Hold[newOpts__] :> DynamicModule[{
		vars,
		initVar = False
	},
		Dynamic[
			If[ TrueQ @ initVar,
				content,
				placeHolder
			],
			TrackedSymbols :> {initVar}
		],
		SynchronousInitialization -> False,
		Initialization :> (
			initVar = False;
			init;
			initVar = True
		),
		newOpts
	]
];

AsynchronousDynamicModule[dm_DynamicModule, ___] := dm;

End[] (* End Private Context *)

EndPackage[]