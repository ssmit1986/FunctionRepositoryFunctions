(* Wolfram Language Package *)

BeginPackage["FunctionRepo`DualNumbers`", {"FunctionRepo`", "GeneralUtilities`"}]
(* Exported symbols added here with SymbolName::usage *)
GeneralUtilities`SetUsage[Dual, "Dual[a$, b$] represents a dual number with standard part a$ and infinitesimal part b$."];
GeneralUtilities`SetUsage[Standard,
    "Standard[d$] extracts the standard part of a dual number d$ (i.e., the first argument)."
];
GeneralUtilities`SetUsage[StandardAll,
    "StandardAll[expr$] replaces all dual numbers in expr$ with their standard parts."
];
GeneralUtilities`SetUsage[NonStandard,
    "NonStandard[d$] extracts the non-standard part of a dual number d$ (i.e., the second argument)."
];
GeneralUtilities`SetUsage[DualExpand,
    "DualExpand[expr$, eps$] replaces each dual number Dual[a$, b$] with a$ + b$ * eps$."
];
GeneralUtilities`SetUsage[DualFactor,
    "DualFactor[expr$, eps$] replaces eps$ with Dual[0, 1] in expr$."
];
GeneralUtilities`SetUsage[DualEpsilon, "DualEpsilon = Dual[0, 1]."];
GeneralUtilities`SetUsage[DualQ, "DualQ[expr$] tests if expr$ is a dual number."];
GeneralUtilities`SetUsage[ScalarQ, "ScalarQ[expr$] = !DualQ[expr$]"];

Begin["`Private`"] (* Begin Private Context *) 

(* 
    Code inspired by the following post on Mathematica StackExchange:
    https://mathematica.stackexchange.com/a/13926/43522
*)

DualQ[Dual[_, _]] := True;
DualQ[_] := False;

ScalarQ[Dual[_, _]] := False;
ScalarQ[_] := True;
scalarPatt = Except[_Dual];

Dual[] = DualEpsilon = Dual[0, 1];
Dual[a_] := Dual[a, 1];

SetAttributes[Standard, Listable];
Standard[Dual[a_, _]] := a;
Standard[x_?NumericQ] := x;

StandardAll[expr_] := ReplaceAll[expr, Dual[a_, _] :> a];

DualExpand[expr_, eps : _ : \[FormalEpsilon]] := ReplaceRepeated[
    expr,
    Dual[a_, b_] :> a + b * eps
];
DualFactor[expr_, eps : _ : \[FormalEpsilon]] := ReplaceRepeated[expr, eps :> Dual[0, 1]];

SetAttributes[NonStandard, Listable];
NonStandard[Dual[_, b_]] := b;
NonStandard[_?NumericQ] := 0;

SetAttributes[std, Listable];
std[Dual[a_, _]] := a;
std[x_] := x;

SetAttributes[nonstd, Listable];
nonstd[Dual[_, b_]] := b;
nonstd[x_] := 0;

Dual[Dual[a_, b_], c_] := Dual[a, b + c];
Dual[a_, Dual[b_, c_]] := Dual[a, b];

Dual /: Dual[a_, 0] := a;
Dual /: (c : scalarPatt) + Dual[a_, b_] := Dual[c + a, b];
Dual /: Dual[a_, b_] + Dual[c_, d_] := Dual[a + c, b + d];
Dual /: (c : scalarPatt) * Dual[a_, b_] := Dual[c * a, c * b];
Dual /: Dual[a_, b_] * Dual[c_, d_] := Dual[a * c, b * c + a * d];

Scan[
    Function[fun,
        Dual /: HoldPattern[fun[Dual[a_, _]]] := fun[a];
    ],
    {
        NumericQ, NumberQ, IntegerQ, Positive, Negative, NonPositive, NonNegative, PossibleZeroQ,
        Re, Im, EvenQ, OddQ, PrimeQ, AlgebraicIntegerQ
    }
];

(* Set upvalues for most built-in numeric functions where possible *)
KeyValueMap[
    Function[{fun, derriv},
        Dual /: fun[Dual[a_, b_]] := Dual[fun[a], derriv[a] * b]
    ],
    KeyDrop[{Plus, Times, Power, Divide, Subtract, Abs, Sign, Mod, Binomial}] @ Select[
        AssociationMap[
            Derivative[1],
            Symbol /@ Select[
                Names["System`*"],
                MemberQ[Attributes[#], NumericFunction]&
            ]
        ],
        And[
            Head[#] === Function,
            !MatchQ[#, Function[D[__]]]
        ]&
    ]
];

(* Set upvalues for some 2-argument functions *)
KeyValueMap[
    Function[{fun, derriv},
        With[{d1 = derriv[[1]], d2 = derriv[[2]]},
            Dual /: fun[Dual[a1_, b1_], Dual[a2_, b2_]] := Dual[
                fun[a1, a2],
                d1[a1, a2] * b1 + d2[a1, a2] * b2
            ];
            Dual /: fun[Dual[a_, b_], c_] := Dual[fun[a, c], d1[a, c] * b];
            Dual /: fun[c_, Dual[a_, b_]] := Dual[fun[c, a], d2[c, a] * b]
        ]
    ],
    AssociationMap[
        Function[f,
            Derivative[##][f]& @@@ IdentityMatrix[2]
        ],
        {Power, Mod, Binomial, Gamma}
    ]
];

Scan[ (* Make sure comparing functions throw away the infinitesimal parts of dual numbers *)
    Function[fun,
        Dual /: fun[first___, d_Dual, rest___] := fun @@ std[{first, d, rest}]
    ],
    {Equal, Unequal, Greater, GreaterEqual, Less, LessEqual}
];

(* Special cases *)
Dual /: Abs[Dual[a_, b_]] := Dual[Abs[a], b Sign[a]];
Dual /: Sign[Dual[a_, b_]] := Sign[a];

(* Special cases for Clip *)
Dual /: Clip[Dual[a_, b_], {xmin : scalarPatt, xmax : scalarPatt}] := Dual[
    Clip[a, {xmin, xmax}],
    b * Piecewise[{{1, xmin<= a <= xmax}}, 0]
];
Dual /: Clip[Dual[a_, b_], {xmin : scalarPatt, xmax : scalarPatt}, {ymin : scalarPatt, ymax : scalarPatt}] := Dual[
    Clip[a, {xmin, xmax}, {ymin, ymax}],
    b * Piecewise[{{1, xmin<= a <= xmax}}, 0]
];
With[{
    clipDerivatives3arg1 = Piecewise[{{1, #[[2]] <= #[[1]] <= #[[3]]}}, 0]&,
    clipDerivatives3arg2 = Piecewise[{{1, #[[1]] < #[[2]]}}, 0]&,
    clipDerivatives3arg3 = Piecewise[{{1, #[[1]] > #[[3]] && #[[1]] >= #[[2]]}}, 0]&,
    clipDerivatives5arg1 = Piecewise[{{1, #[[2]] <= #[[1]] <= #[[3]]}}, 0]&,
    clipDerivatives5arg2 = 0&,
    clipDerivatives5arg3 = 0&,
    clipDerivatives5arg4 = Piecewise[{{1, #[[1]] < #[[2]]}}, 0]&,
    clipDerivatives5arg5 = Piecewise[{{1, #[[1]] > #[[3]] && #[[1]] >= #[[2]]}}, 0]&
},
    Dual /: Clip[Dual[a_, b_], {xmin_, xmax_}] := With[{
        stdargs = {a, std @ xmin, std @ xmax}
    },
        Dual[
            Clip[#[[1]], #[[{2, 3}]]]& @ stdargs,
            Plus[
                b * clipDerivatives3arg1 @ stdargs,
                If[ DualQ[xmin], nonstd[xmin] * clipDerivatives3arg2 @ stdargs, 0],
                If[ DualQ[xmax], nonstd[xmax] * clipDerivatives3arg3 @ stdargs, 0]
            ]
        ]
    ];
    Dual /: Clip[Dual[a_, b_], {xmin_, xmax_}, {ymin_, ymax_}] := With[{
        stdargs = {a, std @ xmin, std @ xmax, std @ ymin, std @ ymax}
    },
        Dual[
            Clip[#[[1]], #[[{2, 3}]], #[[{4, 5}]]]& @ stdargs,
            Plus[
                b * clipDerivatives5arg1 @ stdargs,
                (* these are always 0 anyway *)
                (*
                If[ DualQ[xmin], nonstd[xmin] * clipDerivatives5arg2 @@ stdargs, 0],
                If[ DualQ[xmax], nonstd[xmax] * clipDerivatives5arg3 @@ stdargs, 0],
                *)
                If[ DualQ[ymin], nonstd[ymin] * clipDerivatives5arg4 @ stdargs, 0],
                If[ DualQ[ymax], nonstd[ymax] * clipDerivatives5arg5 @ stdargs, 0]
            ]
        ]
    ]
];

Dual /: f_Symbol[first___, d_Dual, rest___] /; MemberQ[Attributes[f], NumericFunction] := With[{
    args = {first, d, rest}
}, With[{
    dualPos = Flatten @ Position[args, _Dual, {1}, Heads -> False],
    inputs = std[args]
}, With[{
    derrivs = Derivative[##][f]& @@@ IdentityMatrix[Length[args]][[dualPos]]
},
    Dual[
        f @@ inputs,
        Dot[
            Function[# @@ inputs] /@ derrivs,
            args[[dualPos, 2]]
        ]
    ] /; MatchQ[derrivs, {__Function}]
]]];

End[] (* End Private Context *)

EndPackage[]