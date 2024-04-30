(* Wolfram Language Package *)

BeginPackage["FunctionRepo`DampedFixedPoint`", {"FunctionRepo`"}]
(* Exported symbols added here with SymbolName::usage *)
GeneralUtilities`SetUsage[DampedFixedPoint,
	"DampedFixedPoint[f$, expr$, alpha$, n$] works like FixedPoint, but uses a partial update with damping factor alpha$ to stabilize the convergence."
];

Begin["`Private`"] (* Begin Private Context *)

Options[DampedFixedPoint] = Options[FixedPoint];

DampedFixedPoint[f_, expr_, alpha_?Positive, rest___] := FixedPoint[
	alphaUpdate[f, alpha],
	expr,
	rest
];

alphaUpdate[f_, _?(EqualTo[0])] := Identity;
alphaUpdate[f_, _?(EqualTo[1])] := f;
alphaUpdate[f_, alpha_] := Function[e, e + alpha * Subtract[f[e], e]];

End[] (* End Private Context *)

EndPackage[]
