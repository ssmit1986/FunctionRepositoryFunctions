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
DualEpsilon;
DualQ;

Begin["`Private`"] (* Begin Private Context *) 

(* 
    Code inspired by the following post on Mathematica StackExchange:
    https://mathematica.stackexchange.com/a/13926/43522
*)

DualQ[Dual[_, _]] := True;
DualQ[_] := False;

scalarQ[c_] := !DualQ[c]

Dual[] = DualEpsilon = Dual[0, 1];

Standard[Dual[a_, _]] := a;
StandardAll[expr_] := ReplaceAll[expr, Dual[a_, _] :> a];

NonStandard[Dual[_, b_]] := b;

Dual[Dual[a_, b_], c_] := Dual[a, b + c];
Dual[a_, Dual[b_, c_]] := Dual[a, b];

Dual /: Dual[a_, 0 | 0.] := a;
Dual /: c_?scalarQ + Dual[a_, b_] := Dual[c + a, b];
Dual /: Dual[a_, b_] + Dual[c_, d_] := Dual[a + c, b + d];
Dual /: c_?scalarQ * Dual[a_, b_] := Dual[c * a, c * b];
Dual /: Dual[a_, b_] * Dual[c_, d_] := Dual[a * c, b * c + a * d];
(*
Dual /: Power[d_Dual, n_Integer?Positive] := Fold[
    (If[#2 == 1, d, 1] * #1) * #1 &,
    d,
    Rest[IntegerDigits[n, 2]]
];
Dual /: Power[Dual[a_, b_], 0 | 0.] := Dual[a^0, 0];
Dual /: Power[Dual[a_, b_], -1] := Dual[Divide[1, a], -Divide[b, a^2]];
Dual /: Power[d_Dual, n_Integer?Negative] := Divide[1, Power[d, -n]];
Dual /: Power[Dual[a_, b_], x_?scalarQ] := Dual[a^x, b * x * a^Subtract[x, 1]];
Dual /: Power[base_?scalarQ, Dual[a_, b_]] := Dual[base^a, b * base^a * Log[base]];
Dual /: Power[Dual[a1_, b1_], Dual[a2_, b2_]] := Dual[
    a1^a2,
    a1^Subtract[a2, 1] * a2 * b1 + a1^a2 * b2 * Log[a1]
];
*)
Dual /: Abs[Dual[a_, b_]] := Dual[Abs[a], b Sign[a]];
Dual /: Sign[Dual[a_, b_]] := Sign[a];

(* Set upvalues for most built-in numeric functions where possible *)
KeyValueMap[
    Function[{fun, derriv},
        Dual /: fun[Dual[a_, b_]] := Dual[fun[a], derriv[a] * b]
    ],
    KeyDrop[{Plus, Times, Power, Divide, Subtract, Abs, Sign}] @ Select[
        AssociationMap[
            Derivative[1],
            Symbol /@ Select[
                Names["System`*"],
                MemberQ[Attributes[#], NumericFunction]&
            ]
        ],
        Head[#] === Function &
    ]
];

(* Set upvalues for some 2-argument functions *)
KeyValueMap[
    Function[{fun, derriv},
        With[{d1 = derriv[[1]], d2 = derriv[[2]]},
            Dual /: fun[Dual[a_, b_], c_?scalarQ] := Dual[fun[a, c], d1[a, c] * b];
            Dual /: fun[c_?scalarQ, Dual[a_, b_]] := Dual[fun[c, a], d2[c, a] * b];
            Dual /: fun[Dual[a1_, b1_], Dual[a2_, b2_]] := Dual[
                fun[a1, a2],
                d1[a1, a2] * b1 + d2[a1, a2] * b2
            ]
        ]
    ],
    AssociationMap[
        Function[f,
            Derivative[##][f]& @@@ IdentityMatrix[2]
        ],
        {Power, Mod, Binomial}
    ]
];

Scan[ (* Make sure comparing functions throw away the infinitesimal parts of dual numbers *)
    Function[fun,
        Dual /: (expr : fun[___, _Dual, ___]) := ReleaseHold @ StandardAll[HoldComplete[expr]]
    ],
    {Equal, Unequal, Greater, GreaterEqual, Less, LessEqual}
];

Dual /: f_Symbol[first___, d_Dual, rest___] /; MemberQ[Attributes[f], NumericFunction] := With[{
    args = {first, d, rest}
}, With[{
    dualPos = Flatten @ Position[args, _Dual, {1}, Heads -> False],
    inputs = Replace[args, Dual[a_, _] :> a, {1}]
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