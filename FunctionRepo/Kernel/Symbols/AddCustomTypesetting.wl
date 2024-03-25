(* Wolfram Language Package *)

BeginPackage["FunctionRepo`AggregateRowsByKey`", {"FunctionRepo`"}]

GeneralUtilities`SetUsage[AddCustomTypesetting,
	"AddCustomTypesetting[sym$, fun$] creates a MakeBoxes rule for head sym$ so that objects with that head are displayed as fun$[sym$[$$]] but still copy-pastable.
AddCustomTypesetting[sym$, fun$, size$] will use size$ as the ByteCount limit for directly inlining the contents."
];

Begin["`Private`"] (* Begin Private Context *) 

$typesettingQ = False;

AddCustomTypesetting[sym_Symbol, rest___] := AddCustomTypesetting[_sym, rest];

AddCustomTypesetting[patt_, None, ___] := Unset[MakeBoxes[obj : patt, fmt_]];

AddCustomTypesetting[patt_, fun_] := (
	MakeBoxes[obj : patt, fmt_] := Block[{
		$typesettingQ = True
	},
		boxesWithDelayedCache[fun, obj, fmt]
	] /; !TrueQ[$typesettingQ]
);
AddCustomTypesetting[patt_, fun_, limit_] := (
	MakeBoxes[obj : patt, fmt_] := Block[{
		$NotebookInlineStorageLimit = limit,
		$typesettingQ = True
	}, 
		boxesWithDelayedCache[fun, obj, fmt]
	] /; !TrueQ[$typesettingQ]
);

boxesWithDelayedCache[displayFun_, expr_, fmt_] := Module[{
	interpretable, exprkey, 
	byteCount
},
	{interpretable, exprkey, byteCount} = BoxForm`CacheExprIfLarge[expr, 2];
	With[{
		boxes =  ToBoxes[
			DynamicModule[{Typeset`embedState = If[MissingQ[exprkey], "Failed", "Ready"]}, 
				ElisionsDump`makeOuterGrid[displayFun[expr, fmt], Head[expr], exprkey, byteCount, Dynamic[Typeset`embedState], fmt]
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