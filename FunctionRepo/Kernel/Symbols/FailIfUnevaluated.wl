(* Wolfram Language Package *)

BeginPackage["FunctionRepo`FailIfUnevaluated`", {"FunctionRepo`"}]
(* Exported symbols added here with SymbolName::usage *)
GeneralUtilities`SetUsage[FailIfUnevaluated,
	"FailIfUnevaluated[head$[args$$]] checks if a composite expression evaluates and returns a failure if not."
];

Begin["`Private`"] (* Begin Private Context *)

SetAttributes[FailIfUnevaluated, HoldAllComplete];

FailIfUnevaluated[expr_] := FailIfUnevaluated[expr, $Failed];

FailIfUnevaluated[sym_Symbol, failExp_] := With[{
	eval = sym
},
	If[	eval === Unevaluated[sym],
		failExp,
		eval
	]
];

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
FailIfUnevaluated[_, failExp_] := failExp;


attributes[head_Symbol] := DeleteCases[Attributes[head], Protected | ReadProtected | Locked];
attributes[HoldPattern @ Function[_, _, attr_]] := attr;
attributes[_] := {};

End[] (* End Private Context *)

EndPackage[]
