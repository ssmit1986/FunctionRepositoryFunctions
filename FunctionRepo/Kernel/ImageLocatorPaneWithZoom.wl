(* Wolfram Language Package *)

BeginPackage["FunctionRepo`ImageLocatorPaneWithZoom`", {"FunctionRepo`", "GeneralUtilities`"}]
(* Exported symbols added here with SymbolName::usage *)
GeneralUtilities`SetUsage[ImageLocatorPaneWithZoom, "ImageLocatorPaneWithZoom[$$] works like LocatorPane, but provides a zoom area around your cursor."];

Begin["`Private`"] (* Begin Private Context *) 

marker[col_] := Graphics[
	{col, Thickness[0.15], Circle[{0, 0}, 0.8],  Disk[{0, 0}, 0.3]},
	PlotRange -> 1
];

plotMarkers[pts : {__List}, rest__] := Map[plotMarkers[#, rest] &, pts];
plotMarkers[pt : {_, _}, markerFun_, {size_, col_}] := Inset[markerFun[col], pt, {0, 0}, size];
plotMarkers[___] := {};

showDetail[img_,
	{loc : {x_, y_}, pts_List},
	range_Integer,
	insetSize_,
	{markerFun_, col_, markerSize_},
	axesQ_
] := Inset[
	Show[
		img,
		Graphics[
			{
				plotMarkers[pts, markerFun, {markerSize, col}]
			}
		],
		PlotRange -> {x + range * {-1, 1}, y + range * {-1, 1}},
		ImageSize -> Full,
		Axes -> axesQ, AxesOrigin -> loc,
		PlotRangePadding -> None, ImagePadding -> None
	],
	loc,
	loc,
	insetSize
];
showDetail[___] := {};

toGraphics[expr_] := Show[expr];

getSize[img_?ImageQ] := getSize @ toGraphics[img];
getSize[gr_Graphics] := Replace[
	Lookup[
		AbsoluteOptions[gr, PlotRange],
		PlotRange
	],
	mat_?MatrixQ :> Min[Subtract[#2, #1]& @@@ mat]
];
getSize[_] := $Failed;

validBackgroundQ[expr_] := MatchQ[expr, _?ImageQ | _Graphics];

Options[ImageLocatorPaneWithZoom] = Join[
	Options[LocatorPane],
	{
		"ShowZoomControls" -> True,
		"ZoomLevel" -> 2,
		"ZoomSize" -> 0.15,
		"MarkerFunction" -> Automatic,
		"MarkerColor" -> Black,
		"MarkerSize" ->  0.02,
		"ShowCoordinates" -> True
	}
];

ImageLocatorPaneWithZoom[
	Dynamic[pts_, rest___],
	image_?validBackgroundQ,
	rest : Except[_?OptionQ]...,
	opts : OptionsPattern[]
] := With[{
	imgSize = getSize[image]
},
	If[ NumericQ[imgSize]
		,
		DynamicModule[{
			img = toGraphics[image],
			size, zoom, markerFun, color, axesQ,
			pixels, controlsQ,
			calcPixels,
			locatorSize
		},
			Column[
				{
					DynamicWrapper[
						OpenerView[
							{
								"Controls",
								Grid[
									MapAt[Item[#, Alignment -> Right]&, {All, 1}] @ {
										{
											"Zoom factor:",
											Manipulator[Dynamic[zoom], {1, 5}]
										},
										{
											"Zoom area size:",
											Manipulator[Dynamic[size], {0.05, 0.5}]
										},
										{
											"Marker size:",
											Manipulator[Dynamic[locatorSize], {0.01, 0.1}]
										},
										{
											"Marker color:",
											ColorSlider[Dynamic[color]]
										},
										{
											"Show coordinates?",
											Checkbox[Dynamic[axesQ]]
										}
									},
									Alignment -> Left,
									BaseStyle -> "Text"
								]
							},
							Dynamic[controlsQ]
						],
						pixels = calcPixels[imgSize, size, zoom],
						TrackedSymbols :> {size, zoom}
					],
					LocatorPane[
						Dynamic[pts, rest],
						Show[
							img,
							Graphics[
								Dynamic @ {
									plotMarkers[pts, markerFun, {Scaled[locatorSize], color}],
									If[ TrueQ[zoom > 1] || axesQ,
										showDetail[img,
											{MousePosition["Graphics"], pts},
											pixels,
											Scaled[size],
											{markerFun, color, Scaled[locatorSize/size]},
											axesQ
										],
										{}
									]
								}
							]
						], 
						rest,
						Sequence @@ FilterRules[{opts}, Options[LocatorPane]],
						Appearance -> None
					]
				},
				Alignment -> Left
			]
			,
			Initialization :> (
				calcPixels[d_, s_, z_] := Floor @ Divide[s * d, 2 * z];
				size = OptionValue["ZoomSize"];
				zoom = OptionValue["ZoomLevel"];
				markerFun = Replace[OptionValue["MarkerFunction"], Automatic -> marker];
				color = OptionValue["MarkerColor"];
				locatorSize = OptionValue["MarkerSize"];
				axesQ = OptionValue["ShowCoordinates"];
				controlsQ = TrueQ @ OptionValue["ShowZoomControls"];
				pixels = calcPixels[imgSize, size, zoom];
			),
			BaseStyle -> "Text"
		]
		,
		Enclose[ConfirmAssert[NumericQ[imgSize], "Unable to infer image dimensions"]]
	]
];

End[] (* End Private Context *)

EndPackage[]

