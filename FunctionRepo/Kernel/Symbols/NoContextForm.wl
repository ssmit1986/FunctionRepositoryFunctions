BeginPackage["FunctionRepo`NoContextForm`", {"FunctionRepo`", "GeneralUtilities`"}]
(* Exported symbols added here with SymbolName::usage *)
GeneralUtilities`SetUsage[NoContextForm,
	"NoContextForm[expr$] prints expression without any context information shown directly."
];

Begin["`Private`"] (* Begin Private Context *) 

mouseOverBoxes[box1_, box2_] := PaneSelectorBox[
	{False -> box1, True -> box2},
	Dynamic[CurrentValue["MouseOver"]],
	FrameMargins -> 0,
	ImageSize -> Automatic
]

$notInsideNoContextFormQ = True;

NoContextForm[expr_] /; $notInsideNoContextFormQ := Block[{
	$notInsideNoContextFormQ = False
},
	With[{
		boxes = Internal`InheritedBlock[{MakeBoxes},
			MakeBoxes[s_Symbol, _] := With[{
				str = SymbolName[Unevaluated @ s],
				strNormal = ToString[Unevaluated @ s]
			},
				If[ strNormal === str,
					strNormal,
					With[{
						box = mouseOverBoxes[
							StyleBox[str, FontColor -> RGBColor[0.5, 0.5, 0.5]],
							strNormal
						]
					}, 
						InterpretationBox[box, s]
					]
				]
			];
			ToBoxes[expr]
		]
	},
		RawBoxes[boxes]
	]
];

NoContextForm[expr_] := RawBoxes @ ToBoxes[expr];

End[] (* End Private Context *)

EndPackage[]

