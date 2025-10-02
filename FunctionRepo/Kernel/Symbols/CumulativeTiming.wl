(* Wolfram Language Package *)

BeginPackage["FunctionRepo`CumulativeTiming`", {"FunctionRepo`"}]
(* Exported symbols added here with SymbolName::usage *)
GU`SetUsage[CumulativeTiming,
	"CumulativeTiming[expr$, tag$] evaluates expr$ and returns the result. It also keeps track of how much time was spent evaluating the \
expression in $CumulativeTimings[tag$], accumulating the time spent on tag$ expressions. $EvaluationCounts will keep track of how many times the \
function has been invoked.
CumulativeTiming[expr$] is effectively equivalent to CumulativeTiming[expr$, HoldForm @ Head[expr$]], but easier to use."
];

GU`SetUsage[FunctionRepo`TagForTiming,
	"TagForTiming[expr$, tag$] can be used internally in code to collect timing information for CumulativeTiming. It returns expr$."
];

Begin["`Private`"] (* Begin Private Context *)

GU`DefineMacro[MacroCumulativeTiming,
	MacroCumulativeTiming[body_] := mCumulativeTiming[body]
];

$CumulativeTimings = <||>;
$EvaluationCounts = <||>
$time = Null;

SetAttributes[CumulativeTiming, HoldAllComplete];
SetAttributes[TagForTiming, HoldAllComplete];

SetAttributes[heldHead, HoldAllComplete];
heldHead[s_Symbol] := HoldForm[s];
heldHead[h_[___]] := heldHead[h];

initTag[tag_] /; !KeyExistsQ[$CumulativeTimings, tag] := (
	$CumulativeTimings[tag] = 0.;
	$EvaluationCounts[tag] = 0
);
initTag[tag_] := $CumulativeTimings[tag];


Scan[
	Function[sym,
		sym[body_] := With[{
			tag = heldHead[body]
		},
			sym[body, tag]
		]
	],
	{CumulativeTiming, TagForTiming}
];


CumulativeTiming[body_, tag_] := Block[{
	$CumulativeTimingStartedQ = True,
	$currentTag,
	$time,
	$CumulativeTimings = <||>,
	$EvaluationCounts = <||>,
	output
},
	output = TagForTiming[body, tag];
	{
		<|
			"Timings" -> $CumulativeTimings,
			"EvaluationCounts" -> $EvaluationCounts
		|>,
		output
	}
];

TagForTiming[expr_, _] /; !TrueQ[$CumulativeTimingStartedQ] := expr;

TagForTiming[body_, tag_] /; !ValueQ[$currentTag, Method -> "OwnValuesPresent"] := Block[{
	$currentTag = tag,
	$time = SessionTime[],
	evalResult
},
	initTag[tag];
	evalResult = body;
	$CumulativeTimings[tag] += SessionTime[] - $time;
	$EvaluationCounts[tag]++;
	evalResult
]

TagForTiming[body_, tag_] /; $currentTag === tag := (
	$EvaluationCounts[tag]++;
	body
);

TagForTiming[body_, tag_] /; $currentTag =!= Null := Block[{
	prevTag = $currentTag,
	$currentTag = tag,
	$time = SessionTime[],
	evalResult, evalTime
},
	initTag[tag];
	evalResult = body;
	evalTime = SessionTime[] - $time;
	$CumulativeTimings[tag] += evalTime;
	$CumulativeTimings[prevTag] -= evalTime; (* Subtract from the tag that was being timed before so we don't count twice *)
	$EvaluationCounts[tag]++;
	evalResult
];


End[] (* End Private Context *)

EndPackage[]
