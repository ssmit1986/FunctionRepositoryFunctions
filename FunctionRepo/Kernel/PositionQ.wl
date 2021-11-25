(* Wolfram Language Package *)

BeginPackage["FunctionRepo`PositionQ`", {"FunctionRepo`"}]
(* Exported symbols added here with SymbolName::usage *)
GeneralUtilities`SetUsage[PositionQ,
	"PositionQ[expr$, pos$] returns True if pos$ is a valid position in expr$.
PositionQ[expr$, {pos$1, pos$2, $$}] returns True if all positions exists in expr$."];

Begin["`Private`"] (* Begin Private Context *)

PositionQ[pos_][expr_] := PositionQ[expr, pos];

PositionQ[_, {} | {{}}] := True;

SetAttributes[returnTrue, HoldAllComplete];
returnTrue[_] := True;

PositionQ[expr_, pos : {__Integer}] := TrueQ @ Quiet[
	Extract[Unevaluated[expr], pos, returnTrue],
	{Extract::partw, Extract::partd}
];

PositionQ[expr_, pos : {__List}] := ListQ @ Quiet[
	Extract[Unevaluated[expr], pos, HoldComplete],
	{Extract::partw, Extract::partd}
];

PositionQ[_, _] := False;

End[] (* End Private Context *)

EndPackage[]