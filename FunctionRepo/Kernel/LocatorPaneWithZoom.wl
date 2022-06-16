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
plotMarkers[pt : {_, _}, size_, col_] := Inset[marker[col], pt, {0, 0}, size];
plotMarkers[___] := {};

showDetail[img_, pt : {x_, y_}, pts_List, range_Integer, insetSize_] := Inset[
	Framed[
		Show[
			img,
			Graphics[
				{
					plotMarkers[pt, Scaled[0.15], Blue],
					plotMarkers[pts, Scaled[0.15], Red]
				}
			],
			PlotRange -> {x + range * {-1, 1}, y + range * {-1, 1}}
		],
		Background -> White,
		FrameStyle -> Thick
	],
	pt,
	Automatic,
	insetSize
];
showDetail[___] := {};

Options[LocatorPaneWithZoom] = Join[
	Options[LocatorPane],
	{
		"ZoomLevel" -> 2,
		"ZoomSize" -> 0.15
	}
]

LocatorPaneWithZoom[Dynamic[pts_], image_?ImageQ, rest : Except[_?OptionQ]..., opts : OptionsPattern[]] := With[{
	minDim = Min[ImageDimensions[image]]
},
	DynamicModule[{
		img = image,
		size,
		zoom,
		pixels,
		calcPixels
	},
		DynamicWrapper[
			Grid[
				{
					{
						Item["Zoom:", Alignment -> Right],
						Manipulator[Dynamic[zoom], {1, 5}]
					},
					{
						Item["Zoom size:", Alignment -> Right],
						Manipulator[Dynamic[size], {0.05, 0.5}]
					},
					{
						LocatorPane[
							Dynamic[pts],
							Show[
								Image[img, ImageSize -> Full],
								Graphics[
									Dynamic @ {
										plotMarkers[pts, Scaled[0.02], Red],
										showDetail[img, MousePosition["Graphics"], pts, pixels, Scaled[size]]
									}
								]
							], 
							rest,
							Sequence @@ FilterRules[{opts}, Options[LocatorPane]],
							Appearance -> None
						],
						SpanFromLeft
					}
				},
				Alignment -> Left,
				BaseStyle -> "Text"
			],
			pixels = calcPixels[minDim, size, zoom],
			TrackedSymbols :> {size, zoom}
		]
		,
		Initialization :> (
			calcPixels[d_, s_, z_] := Floor @ Divide[s * d, 2 * z];
			size = OptionValue["ZoomSize"];
			zoom = OptionValue["ZoomLevel"];
			pixels = calcPixels[minDim, size, zoom];
		)
	]
];

End[] (* End Private Context *)

EndPackage[]

