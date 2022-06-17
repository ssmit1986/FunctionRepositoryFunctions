(* Wolfram Language Package *)

BeginPackage["FunctionRepo`LocatorPaneWithZoom`", {"FunctionRepo`", "GeneralUtilities`"}]
(* Exported symbols added here with SymbolName::usage *)
GeneralUtilities`SetUsage[LocatorPaneWithZoom, "LocatorPaneWithZoom[$$] works like LocatorPane, but provides a zoom area around your cursor."];

Begin["`Private`"] (* Begin Private Context *) 

marker[col_] := Graphics[
	{col, Thickness[0.15], Circle[{0, 0}, 0.8],  Disk[{0, 0}, 0.3]},
	PlotRange -> 1
];

plotMarkers[pts : {__List}, rest__] := Map[plotMarkers[#, rest] &, pts];
plotMarkers[pt : {_, _}, size_, {markerFun_, col_}] := Inset[markerFun[col], pt, {0, 0}, size];
plotMarkers[___] := {};

showDetail[img_,
	{loc : {x_, y_}, pts_List},
	range_Integer,
	insetSize_,
	{markerFun_, col_},
	axesQ_
] := Inset[
	Show[
		img,
		Graphics[
			{
				plotMarkers[pts, Scaled[0.15], {markerFun, col}]
			}
		],
		PlotRange -> {x + range * {-1, 1}, y + range * {-1, 1}},
		ImageSize -> Full,
		Axes -> axesQ, AxesOrigin -> loc,
		PlotRangePadding -> None, ImagePadding -> None
	],
	loc,
	Automatic,
	insetSize
];
showDetail[___] := {};

Options[LocatorPaneWithZoom] = Join[
	Options[LocatorPane],
	{
		"ShowZoomControls" -> True,
		"ZoomLevel" -> 2,
		"ZoomSize" -> 0.15,
		"MarkerFunction" -> Automatic,
		"MarkerColor" -> Black,
		"ShowCoordinates" -> True
	}
]

LocatorPaneWithZoom[Dynamic[pts_], image_?ImageQ, rest : Except[_?OptionQ]..., opts : OptionsPattern[]] := With[{
	minDim = Min @ ImageDimensions[image],
	controlsQ = TrueQ @ OptionValue["ShowZoomControls"]
},
	DynamicModule[{
		img = image,
		size, zoom, markerFun, color, axesQ,
		pixels,
		calcPixels
	},
		Column[
			{
				If[ controlsQ
					,
					DynamicWrapper[
						OpenerView[{
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
						}, True],
						pixels = calcPixels[minDim, size, zoom],
						TrackedSymbols :> {size, zoom}
					]
					,
					Nothing
				],
				LocatorPane[
					Dynamic[pts],
					Show[
						img,
						Graphics[
							Dynamic @ {
								plotMarkers[pts, Scaled[0.02], {markerFun, color}],
								showDetail[img,
									{MousePosition["Graphics"], pts},
									pixels,
									Scaled[size],
									{markerFun, color},
									axesQ
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
			axesQ = OptionValue["ShowCoordinates"];
			pixels = calcPixels[minDim, size, zoom];
		),
		BaseStyle -> "Text"
	]
];

End[] (* End Private Context *)

EndPackage[]

