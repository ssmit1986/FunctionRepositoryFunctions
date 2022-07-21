(* Wolfram Language Package *)

BeginPackage["FunctionRepo`VariationalInterpolation`", {"FunctionRepo`", "GeneralUtilities`"}]
(* Exported symbols added here with SymbolName::usage *)
GeneralUtilities`SetUsage[VariationalInterpolation, 
	"VariationalInterpolation[args$$] bla"
];

Begin["`Private`"] (* Begin Private Context *) 

VariationalInterpolation[lagrangian_, y_, bc : {{x1_, y1_}, {x2_, y2_}}, n_Integer] := Module[{
	lg, x, xvars, xmat, 
	xinit, min
},
	xvars = Array[x, {n, 2}];
	xmat = MapAt[
		x1 + Subtract[x2, x1] LogisticSigmoid[#] &,
		xvars,
		{All, 1}
	];
	xinit = RandomVariate[NormalDistribution[0, 2], n];
	xinit = Transpose[{
		xinit,
		Quiet[Interpolation[bc]][
		xmat[[All, 1]] /. Thread[xvars[[All, 1]] -> xinit]]
	}];
	lg[mat_?(MatrixQ[#, NumericQ] &)] := lg[mat] = Activate[lagrangian /. y -> Interpolation[Join[mat, bc]]];
	min = FindMinimum[
		lg[xmat],
		Evaluate @ Transpose[{
			Flatten[xvars],
			Flatten[xinit]
		}]
	];
	{min[[1]], Interpolation[Join[xmat, bc] /. min[[2]]]}
];

End[] (* End Private Context *)

EndPackage[]