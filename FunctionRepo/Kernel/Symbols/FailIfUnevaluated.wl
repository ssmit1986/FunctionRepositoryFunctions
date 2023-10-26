(* Wolfram Language Package *)

BeginPackage["FunctionRepo`FailIfUnevaluated`", {"FunctionRepo`"}]
(* Exported symbols added here with SymbolName::usage *)
GeneralUtilities`SetUsage[FailIfUnevaluated,
	"FailIfUnevaluated[head$[args$$]] checks if a composite expression evaluates and returns a failure if not."
];

Begin["`Private`"] (* Begin Private Context *)

SetAttributes[FailIfUnevaluated, HoldAllComplete];

FailIfUnevaluated[expr_] := FailIfUnevaluated[expr, $Failed];

FailIfUnevaluated[head_[args___], failExp_] := With[{
	evalHead = head
},
	With[{
		evalArgs = With[{
			attr = attributes[evalHead]
		},
			Function[Null, HoldComplete[##], attr][args]
		]
	},
		Replace[
			evalArgs,
			{
				HoldComplete[newArgs___] :> With[{
					try = evalHead[newArgs]
				},
					try /; try =!= Unevaluated[evalHead[newArgs]]
				],
				_ :> failExp
			}
		]
	]
];

(* Fallback for symbols and other expressions not of the form head_[args___] *)
FailIfUnevaluated[expr_, failExp_] := With[{
	eval = expr
},
	If[	eval === Unevaluated[expr],
		failExp,
		eval
	]
];

attributes[head_Symbol] := DeleteCases[Attributes[head],
	Protected | ReadProtected | Locked | Constant | Stub | Temporary | OneIdentity | NumericFunction
];
attributes[HoldPattern @ Function[_, _, attr_]] := attr;
attributes[_] := {};

End[] (* End Private Context *)

EndPackage[]
