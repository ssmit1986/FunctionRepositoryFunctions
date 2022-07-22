(* Wolfram Language Package *)

BeginPackage["FunctionRepo`CopyAsExcelData`", {"FunctionRepo`"}]
(* Exported symbols added here with SymbolName::usage *)
GeneralUtilities`SetUsage[CopyAsExcelData,
	"CopyAsExcelData[list$] copies data in a way such that it can be pasted into Excel."
];

Begin["`Private`"] (* Begin Private Context *) 

CopyAsExcelData[{}] := $Failed;

CopyAsExcelData[list_List?VectorQ] := CopyAsExcelData[{list}];

CopyAsExcelData[data2D : {__List?VectorQ}] := Module[{
	dataToCopy = ExportString[
		Map[cleanup, data2D, {2}],
		"TSV"
	]
},
	If[ StringQ[dataToCopy],
		CopyToClipboard[dataToCopy];
		clickToCopyAsPlainText[dataToCopy]
		,
		$Failed
	]
];

CopyAsExcelData[___] := $Failed;

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

clickToCopyAsPlainText[str_String] := RawBoxes[
	TemplateBox[{str}, "ClickToCopy",
		DisplayFunction -> Function[
			TagBox[
				DynamicModuleBox[
					{Typeset`boxobj$$, Typeset`cellobj$$}
					,
					TagBox[
						TagBox[
							ButtonBox[
								TagBox[#1, BoxForm`Undeploy, DefaultBaseStyle -> {Deployed -> False}],
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
									"MouseExited" :>
									NotebookDelete[Typeset`cellobj$$],
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
		InterpretationFunction -> Function[ToBoxes[#1, StandardForm]]
	]
];

End[] (* End Private Context *)

EndPackage[]