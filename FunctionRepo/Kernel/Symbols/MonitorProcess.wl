(* Wolfram Language Package *)

BeginPackage["FunctionRepo`MonitorFile`", {"FunctionRepo`", "GeneralUtilities`"}]
(* Exported symbols added here with SymbolName::usage *)
GeneralUtilities`SetUsage[MonitorProcess, 
	"MonitorProcess[proc$] creates a dynamic that shows the output of a process proc$."
];

Begin["`Private`"] (* Begin Private Context *) 

pane[expr_] := Pane[expr, {800, 200} , ImageSize -> {Full, 250}, ImageSizeAction -> "Scrollable", Scrollbars -> Automatic];

Options[MonitorProcess] = {
	UpdateInterval -> 1.,
	"KillProcessQ" -> True
};

MonitorProcess[proc_, opts : OptionsPattern[]] := MonitorProcess[proc, Function[Null], opts];
MonitorProcess[proc_ProcessObject, post_, opts : OptionsPattern[]] := DynamicModule[{
	killProcQ = OptionValue["KillProcessQ"],
	updateInterval = OptionValue[UpdateInterval],
	progress = "", done = False,
	read
},
	PaneSelector[
		{
			False -> DynamicWrapper[
				Framed[pane @ Dynamic[progress], FrameStyle -> Red]
				,
				If[ TrueQ[! done],
					read = ReadString[proc, EndOfBuffer];
					Which[
						read === EndOfFile, 
							done = True;
							post[proc];
							If[TrueQ @ killProcQ, KillProcess[proc]]
						,
						StringQ[read],
							progress = progress <> read,
						True,
							Null
					]
				],
				TrackedSymbols :> {},
				UpdateInterval -> updateInterval,
				SynchronousUpdating -> False
			]
		},
		Dynamic[done],
		Framed[pane @ Dynamic[progress, TrackedSymbols :> {progress}], FrameStyle -> Darker[Green, 0.1]],
		ImageSize -> Automatic
	]
];



(*

obj = With[{proc = proc},
	SessionSubmit[
		ScheduledTask[
			Replace[
				ReadString[proc, EndOfBuffer], 
				{
					"" :> Null,
					EndOfFile :> TaskRemove[$CurrentTask],
					s_String :> Print[s]
				}
			],
			Quantity[5, "Seconds"]
		]
	]
]

*)


End[] (* End Private Context *)

EndPackage[]
