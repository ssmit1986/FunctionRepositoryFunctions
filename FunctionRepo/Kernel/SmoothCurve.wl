(* Wolfram Language Package *)

BeginPackage["FunctionRepo`SmoothCurve`", {"FunctionRepo`", "GeneralUtilities`"}]
(* Exported symbols added here with SymbolName::usage *)
GeneralUtilities`SetUsage[SmoothCurve, 
    "SmoothCurve[{pt$1, pt2$2, $$}] creates a smooth curve that goes through all points pt$i."
];

Begin["`Private`"] (* Begin Private Context *) 

SmoothCurve[pts_?MatrixQ, opts : OptionsPattern[]] := Module[{
    controlPoints,
    tvec
},
    pts
];

chebyshevNodes[npts_Integer] := Rescale[
    NumericalSort @ Cos[Pi * Divide[Range[0, npts - 1], npts - 1]]
];
equalSpaceNodes[npts_Integer] := Divide[Range[npts - 2], (npts - 1)];

ClearAll[bezCurve];
bezCurve[{pt : {__}}] := pt&;
bezCurve[ctrlPts_?MatrixQ] := bezCurve[ctrlPts] = With[{
    b1 = bezCurve[Most[ctrlPts]],
    b2 = bezCurve[Rest[ctrlPts]]
}, (* Recursive definition of Bezier curve. Returns a Function *)
    (1 - #) * b1[#] + # * b2[#]&
];
bezCurve[ctrlPts_?MatrixQ, t_] := Simplify[bezCurve[ctrlPts][t]];

(* Recursive definition of BSpline basis functions. Returns a Function *)
nbase[{i_, 0}, uVec_] := Function[
    Evaluate[Piecewise[{{1, uVec[[i + 1]] <= # < uVec[[i + 2]]}, {0, True}}]]
];

nbase[{i_, p_}, uVec_] := nbase[{i, p}, uVec] = Function[
    Evaluate @ Quiet[
        Check[(# - uVec[[i + 1]])/(uVec[[i + p + 1]] - uVec[[i + 1]]) * nbase[{i, p - 1}, uVec][#], 0] + 
        Check[(uVec[[i + p + 2]] - #)/(uVec[[i + p + 2]] - uVec[[i + 2]]) * nbase[{i + 1, p - 1}, uVec][#], 0]
    ]
];
nbase[{i_, p_}, uVec_, u_] := Simplify[nbase[{i, p}, uVec][u]];

(* Default spline knot vector for a BSpline curve of given degree and nr. of control points. For any knot vector uVec, LessEqual @@ uVec should return true. *)
splineKnotVector[npts_Integer, degree_Integer] /; degree < npts := Module[{
    nKnot = degree + npts + 1,
    nmid
},
    nmid = nKnot - 2 degree - 2;
    Flatten @ {
        ConstantArray[0, degree + 1],
        Divide[Range[nmid], nmid + 1],
        ConstantArray[1, degree + 1]
    }
];

bspCurve[pts_?MatrixQ, degree_Integer] := bspCurve[pts, splineKnotVector[Length[pts], degree]];

bspCurve[pts_?MatrixQ, uVec_?VectorQ] := bspCurve[pts, uVec] = With[{
    n = Length[pts] - 1,
    degree = Length[uVec] - Length[pts] - 1
},
    Function[
        (* Definition of BSpline curve; returns a Function. Mind the common convention to index from 0 *)
        Evaluate @ Simplify @ Sum[
            nbase[{i, degree}, uVec][#] * pts[[i + 1]],
            {i, 0, n}
        ]
    ]
];
bspCurve[pts_, spec_, u_] := bspCurve[pts, spec][u];

BezierControlPointTransformationMatrix[npts_Integer /; npts > 2] := BezierControlPointTransformationMatrix[
    (* Assume that the Bezier curve intercepts the interpolation points at equally spaced parameter intervals *)
    Range[npts - 2]/(npts - 1)
];

BezierControlPointTransformationMatrix[list_List?(VectorQ[#, NumericQ]&)] := Module[{
    npts, pts, 
    ctrlPts,
    eqsBez,
    tVals = Select[list, 0 < # < 1 &], (* t = 0 and t = 1 are the beginning and end points of the curve and are added seperately. Remaining interpolation points are intercepted at the t values specified by tVals *)
    sol,
    mat
},
    Condition[
        npts = Length[tVals] + 2; 
        pts = Array[\[FormalP], {npts, 1}]; (* symbolic data points that need to be interpolated*)
        ctrlPts = Array[\[FormalR], Dimensions[pts]]; (* symbolic control points *)
        eqsBez = Simplify @ Flatten @ {
            (* First and last control points have to align with the data *)
            Thread[First[ctrlPts] == First[pts]],
            Thread[Last[ctrlPts] == Last[pts]],
            
            (* Equations that specify that the Bezier curve has to intercept the interior data points at t_i specified by tVals *)
            Flatten @ MapThread[
                Thread[bezCurve[ctrlPts, #1] == #2]&,
                {
                    tVals,
                    pts[[2 ;; -2]]
                }
            ]
        };
        sol = First @ Solve[eqsBez, Flatten @ ctrlPts]; (* Solve contraint equations *)
        mat = Normal[ (* Convert solution to a matrix equation and extract the transformation matrix between the interpolation points and control points *)
            Last @ CoefficientArrays[
                Thread[(Flatten[ctrlPts] /. sol) == Flatten[ctrlPts]],
                Flatten[pts]
            ]
        ],
        Length[tVals] > 0
    ]
];

BSplineControlPointsMatrix[npts_Integer /; npts > 2, rest___] := BSplineControlPointsMatrix[
    (* Assume that the BSpline curve intercepts the interpolation points at equally spaced parameter intervals *)
    Range[npts - 2]/(npts - 1),
    rest
];

(* 
    Like with BezierControlPointTransformationMatrix, list specifies the t-values (0 <= t <= 1) for which the interpolation function intercepts the data points 
    spec is either the knot vector (_List) or the degree (_Integer) of the BSpline
*)
BSplineControlPointsMatrix[list_List?(VectorQ[#, NumericQ]&), spec : (_List | _Integer?Positive)] := Module[{
    npts, pts,
    ctrlPts,
    eqsBsp,
    tVals = Select[list, 0 < # < 1 &],
    sol,
    mat,
    uVec
},
    Condition[
        npts = Length[tVals] + 2; (* symbolic data points that need to be interpolated*)
        uVec = Replace[spec, degree_Integer :> splineKnotVector[npts, degree]]; 
        pts = Array[\[FormalP], {npts, 1}];
        ctrlPts = Array[\[FormalR], Dimensions[pts]]; (* symbolic control points *)
        eqsBsp = Simplify @ Flatten @ {
            (* First and last control points have to align with the data *)
            Thread[First[ctrlPts] == First[pts]],
            Thread[Last[ctrlPts] == Last[pts]],
            
            (* Equations that specify that the BSpline curve has to intercept the interior data points at t_i specified by tVals *)
            Flatten @ MapThread[
                Thread[bspCurve[ctrlPts, uVec, #1] == #2]&,
                {
                    tVals,
                    pts[[2 ;; -2]]
                }
            ]
        };
        sol = First @ Solve[eqsBsp, Flatten @ ctrlPts]; (* Solve equations *)
        mat = Normal[ (* Convert solution to matrix equation and extract transformation matrix *)
            Last @ CoefficientArrays[
                Thread[(Flatten[ctrlPts] /. sol) == Flatten[ctrlPts]],
                Flatten[pts]
            ]
        ];
        <|
            "TransformationMatrix" -> mat,
            "SplineKnots" -> uVec
        |>
        ,
        Length[tVals] > 0
    ]
];


End[] (* End Private Context *)

EndPackage[]