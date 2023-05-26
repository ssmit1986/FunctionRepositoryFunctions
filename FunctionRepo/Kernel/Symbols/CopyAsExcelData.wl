(* Wolfram Language Package *)

BeginPackage["FunctionRepo`CopyAsExcelData`", {"FunctionRepo`"}]
(* Exported symbols added here with SymbolName::usage *)
GeneralUtilities`SetUsage[CopyAsExcelData,
	"CopyAsExcelData[list$] copies data in a way such that it can be pasted into Excel."
];

Begin["`Private`"] (* Begin Private Context *) 

CopyAsExcelData[{}] := $Failed;

CopyAsExcelData[list_List?VectorQ] := CopyAsExcelData[{list}];

CopyAsExcelData[data2D_List] := Module[{
	dataToCopy = If[ MatchQ[data2D, {__List?VectorQ}],
		ExportString[
			Map[cleanup, data2D, {2}],
			"TSV"
		],
		$Failed
	]
},
	If[ StringQ[dataToCopy],
		CopyToClipboard[dataToCopy];
		CopyAsExcelData[dataToCopy]
		,
		$Failed
	]
];
CopyAsExcelData[str_String, "PlainText"] := (
	CopyToClipboard[str];
	CopyAsExcelData[str]
);
CopyAsExcelData[Except[_String], ___] := $Failed;
CopyAsExcelData[] := $Failed;

CopyAsExcelData /: MakeBoxes[CopyAsExcelData[dataToCopy_String], StandardForm] := clickToCopyAsPlainText[dataToCopy];

cleanup[normal : _String | _?NumericQ] := normal;
cleanup[date_DateObject?DateObjectQ] := DateString[date,
	Switch[date["Granularity"],
		"Hour" | "Minute" | "Second" | "Instant",
			"ISODateTime",
		_,
			"ISODate"
	]
];
cleanup[time_TimeObject?TimeObjectQ] := DateString[time, "Time"];
cleanup[other_] := TextString[other];

(*
	TemplateBox has been adapted from
	CurrentValue[{StyleDefinitions, "ClickToCopy"}]
	and
	CurrentValue[{StyleDefinitions, "ClickToCopy2"}]
*)
clickToCopyAsPlainText[str_String] := TemplateBox[
	{str},
	"ClickToCopy",
	DisplayFunction -> Function[
		TagBox[
			DynamicModuleBox[
				{Typeset`boxobj$$, Typeset`cellobj$$}
				,
				TagBox[
					TagBox[
						ButtonBox[
							TagBox[
								DynamicBox[ToBoxes[#1, StandardForm], SingleEvaluation -> True],
								BoxForm`Undeploy,
								DefaultBaseStyle -> {Deployed -> False}
							],
							ButtonFunction :> FrontEndExecute[
								{
									CopyToClipboard[#1],
									NotebookDelete[Typeset`cellobj$$],
									FrontEnd`AttachCell[
										Typeset`boxobj$$,
										Cell[BoxData[TemplateBox[{"Copied"}, "ClickToCopyTooltip"]]],
										{1, {Center, Bottom}},
										{Center, Top},
										"ClosingActions" -> {"ParentChanged", "MouseExit"}
									]
								}
							],
							Evaluator -> None,
							Appearance -> {
								"Default" -> None,
								"Hover" -> FrontEnd`FileName[{"Typeset", "ClickToCopy"}, "Hover.9.png"],
								"Pressed" -> FrontEnd`FileName[{"Typeset", "ClickToCopy"}, "Pressed.9.png"]
							},
							BaseStyle -> {},
							DefaultBaseStyle -> {},
							BaselinePosition -> Baseline,
							FrameMargins -> 2,
							Method -> "Preemptive"
						],
						EventHandlerTag[
							{
								"MouseEntered" :> (
									Typeset`cellobj$$ = MathLink`CallFrontEnd[
										FrontEnd`AttachCell[
											Typeset`boxobj$$,
											Cell[BoxData[TemplateBox[{"Copy"}, "ClickToCopyTooltip"]]],
											{1, {Center, Bottom}},
											{Center, Top},
											"ClosingActions" -> {"ParentChanged"}
										]
									]
								),
								"MouseExited" :> NotebookDelete[Typeset`cellobj$$],
								PassEventsDown -> True,
								Method -> "Preemptive",
								PassEventsUp -> True
							}
						]
					],
					MouseAppearanceTag["LinkHand"]
				],
				Initialization :> (
					Typeset`boxobj$$ = EvaluationBox[]
				),
				DynamicModuleValues :> {},
				UnsavedVariables :> {Typeset`boxobj$$, Typeset`cellobj$$},
				BaseStyle -> {
					Editable -> False
				}
			],
			Deploy,
			DefaultBaseStyle -> "Deploy"
		]
	],
	InterpretationFunction -> Function[RowBox[{"CopyAsExcelData", "[", InterpretationBox["", #], "]"}]]
];

End[] (* End Private Context *)

EndPackage[]