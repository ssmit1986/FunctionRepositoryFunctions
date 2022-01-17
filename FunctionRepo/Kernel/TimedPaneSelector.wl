(* Wolfram Language Package *)

BeginPackage["FunctionRepo`TimedPaneSelector`", {"FunctionRepo`", "GeneralUtilities`"}]
(* Exported symbols added here with SymbolName::usage *)
GeneralUtilities`SetUsage[TimedPaneSelector,
	"TimedPaneSelector[content$, Dynamic[state$], rules$] creates a pane that changes states automatically as specified by rules$.
TimedPaneSelector[content$, {Dynamic[state$], Dynamic[t$]}, rules$] updates t$ to track the time remaining till the pane changes."
];

Begin["`Private`"] (* Begin Private Context *)

SetAttributes[customDynamicWrapper, HoldRest];
customDynamicWrapper[display_, refreshAction_, interval_] /; TrueQ[$CloudEvaluation] := With[{
	refreshRate = N @ Replace[
		1/interval,
		Except[_?NumericQ] :> 1
	]
},
	Row[{
		display,
		DynamicModule[{dummy},
			Animator[ (* Animator has controllable refresh rate in the cloud *)
				Dynamic[dummy,
					Function[
						refreshAction;
						dummy = #;
					]
				],
				{0, 1},
				RefreshRate -> refreshRate,
				AppearanceElements -> None,
				ImageSize -> {0, 0}
			]
		]
	}]
];

customDynamicWrapper[disp_, action_, interval_] :=
	DynamicWrapper[disp, action,
		TrackedSymbols :> {},
		UpdateInterval -> interval
	];

findResetTime[num_?NumericQ, ___] := num;
findResetTime[Dynamic[val_, ___], rest___] := findResetTime[val, rest];
findResetTime[fun_, key_, default_] := Replace[
	Replace[
		fun[key],
		Except[_?NumericQ] :> Setting[default]
	],
	Except[_?NumericQ] :> 5
];

Options[TimedPaneSelector] = Join[
	{
		UpdateInterval -> 1.,
		"ResetTime" -> <||>,
		"DefaultResetTime" -> 5.
	},
	Options[PaneSelector]
];

TimedPaneSelector[
	content : {__Rule}, (* You can use _ -> defaultContent to put things into the 3rd argument of PaneSelector *)
	{
		Dynamic[
			triggerVar_,
			trigFun : _ : Function[{val1, expr1}, expr1 = val1, HoldRest] (* The default function resets the pane to the defaultKey when the trigger expires *)
		],
		Dynamic[
			tRemain_,
			tFun : _ : Function[{val2, expr2}, expr2 = val2, HoldRest] (* allows the user to extract the remaining time out of the pane *)
		]
	},
	resetRules : {___Rule},
	opts : OptionsPattern[]
] := DynamicModule[{
	triggeredQ,
	currentTriggeredKey,
	tExpire,
	triggerKeys = DeleteDuplicates[Keys[resetRules]],
	interval = OptionValue[UpdateInterval],
	resetTimeSpec = OptionValue["ResetTime"],
	defaultResetTime = OptionValue["DefaultResetTime"]
},
	customDynamicWrapper[
		DynamicWrapper[
			PaneSelector[
				Normal @ KeyDrop[content, Blank[]],
				Dynamic[triggerVar],
				Lookup[content, Blank[], ""],
				Sequence @@ FilterRules[{opts}, Options[PaneSelector]]
			]
			,
			If[ MemberQ[triggerKeys, triggerVar] (* triggerVar changed and the countdown needs to be triggered *)
				,
				If[ triggerVar =!= currentTriggeredKey, (* Double check that the DynamicWrapper didn't fire spuriously *) 
					triggeredQ = True; (* Start the timer when the triggerVar goes up or changes to another trigger variable *)
					currentTriggeredKey = triggerVar;
					tFun[
						findResetTime[resetTimeSpec, currentTriggeredKey, defaultResetTime],
						tRemain
					];
					tExpire = SessionTime[] + tRemain
				]
				,
				(* Reset the pane if the trigger is reset externally *)
				triggeredQ = False;
				Clear[currentTriggeredKey];
				tFun[0., tRemain];
				tExpire = SessionTime[]
			],
			TrackedSymbols :> {triggerVar}
		]
		,
		If[ triggeredQ,
			(* Time is running *)
			With[{
				now = SessionTime[]
			},
				tFun[Subtract[tExpire, now], tRemain];
				tExpire = now + tRemain;
				If[ TrueQ[triggerVar === currentTriggeredKey && tExpire < now]
					,
					trigFun[Lookup[resetRules, currentTriggeredKey, Null], triggerVar];
					triggeredQ = False;
					Clear[currentTriggeredKey];
					tFun[0., tRemain];
					tExpire = now
				]
			]
		]
		,
		interval
	],
	Initialization :> (
		triggeredQ = MemberQ[triggerKeys, triggerVar];
		If[ triggeredQ,
			currentTriggeredKey = triggerVar
		];
		If[ !NumericQ[tRemain],
			tFun[
				Boole[triggeredQ] * findResetTime[resetTimeSpec, currentTriggeredKey, defaultResetTime],
				tRemain
			]
		];
		tExpire = SessionTime[] + tRemain
	),
	UnsavedVariables :> {triggeredQ, currentTriggeredKey, tExpire}
];
TimedPaneSelector[fst_, trigDynamic_Dynamic, rest___] := DynamicModule[{
	tExpire
},
	TimedPaneSelector[fst,
		{trigDynamic, Dynamic[tExpire]},
		rest
	]
];

End[] (* End Private Context *)

EndPackage[]