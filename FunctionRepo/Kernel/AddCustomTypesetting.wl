(* Wolfram Language Package *)

BeginPackage["FunctionRepo`AggregateRowsByKey`", {"FunctionRepo`"}]

GeneralUtilities`SetUsage[AddCustomTypesetting,
	"AddCustomTypesetting[sym$, fun$] creates a MakeBoxes rule for head sym$ so that objects with that head are displayed as fun$[sym$] but still copy-pastable.
AddCustomTypesetting[sym$, fun$, size$] will use size$ as the ByteCount limit for directly inlining the contents."
];

Begin["`Private`"] (* Begin Private Context *) 

AddCustomTypesetting[sym_Symbol, fun_] := (
	sym /: MakeBoxes[obj_sym, fmt_] := boxesWithDelayedCache[fun, obj, fmt]
);
AddCustomTypesetting[sym_Symbol, fun_, limit_] := (
	sym /: MakeBoxes[obj_sym, fmt_] := Block[{
		$NotebookInlineStorageLimit = limit
	}, 
		boxesWithDelayedCache[fun, obj, fmt]
	]
);

boxesWithDelayedCache[displayFun_, expr_, fmt_] := Module[{
	interpretable, exprkey, 
	byteCount
},
	{interpretable, exprkey, byteCount} = BoxForm`CacheExprIfLarge[expr, 2];
	With[{
		boxes =  ToBoxes[
			DynamicModule[{Typeset`embedState = If[MissingQ[exprkey], "Failed", "Ready"]}, 
				ElisionsDump`makeOuterGrid[displayFun[expr], Head[expr], exprkey, byteCount, Dynamic[Typeset`embedState], fmt]
			], 
			fmt
		]
	},
		If[ TrueQ[interpretable], 
			Replace[ElisionsDump`$OverrideSummaryInterp,
				{
					HoldComplete[interp_] :> InterpretationBox[boxes, interp, Selectable -> False, Editable -> False, SelectWithContents -> True],
					_ :> InterpretationBox[boxes, expr, Selectable -> False, Editable -> False, SelectWithContents -> True]
				}
			], 
			With[{
				shortString = ToString[Unevaluated @ Short @ expr, OutputForm, PageWidth -> 400]
			}, 
				TagBox[
					TemplateBox[{boxes}, "CopyTag", DisplayFunction -> (#1 &), InterpretationFunction -> (shortString &)], 
					Selectable -> False, Editable -> False, SelectWithContents -> True,
					Replace[exprkey, {None | _Missing :> Sequence[], key_ :> (BoxID -> key)}]
				]
			]
		]
	]
];

End[] (* End Private Context *)

EndPackage[]