(* Wolfram Language Package *)

BeginPackage["FunctionRepo`DualNumbers`", {"FunctionRepo`", "GeneralUtilities`"}]
(* Exported symbols added here with SymbolName::usage *)
GeneralUtilities`SetUsage[Dual, "mergeByKey[{assoc$1, assoc$2, $$}, {key$1 -> fun$1, key$2 -> fun$2, $$}, default$] merges the assocations using different merge functions for different keys."];
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

DualEpsilon = Dual[0, 1];

Dual[Dual[a_, b_], c_] := Dual[a, b + c];
Dual[a_, Dual[b_, c_]] := Dual[a, b];

Dual /: Dual[a_, 0] := a;
Dual /: c_?scalarQ + Dual[a_, b_] := Dual[c + a, b];
Dual /: Dual[a_, b_] + Dual[c_, d_] := Dual[a + c, b + d];
Dual /: c_?scalarQ * Dual[a_, b_] := Dual[c * a, c * b];
Dual /: Dual[a_, b_] * Dual[c_, d_] := Dual[a * c, b * c + a * d];
Dual /: Power[d_Dual, n_Integer?Positive] := Fold[
    (If[#2 == 1, d, 1] * #1) * #1 &,
    d,
    Rest[IntegerDigits[n, 2]]
];
Dual /: Power[Dual[a_, b_], 0 | 0. | _?(EqualTo[0])] := Dual[a^0, 0];
Dual /: Power[Dual[a_, b_], -1] := Dual[Divide[1, a], -Divide[b, a^2]];
Dual /: Power[d_Dual, n_Integer?Negative] := Divide[1, Power[d, -n]];
Dual /: Power[Dual[a_, b_], x_?scalarQ] := Dual[a^x, b * x * a^Subtract[x, 1]];
Dual /: Power[base_?scalarQ, Dual[a_, b_]] := Dual[base^a, b * base^a * Log[base]];
Dual /: Power[Dual[a1_, b1_], Dual[a2_, b2_]] := Dual[
    a1^a2,
    a1^Subtract[a2, 1] * a2 * b1 + a1^a2 * b2 * Log[a1]
];

Dual /: Abs[Dual[a_, b_]] := Dual[Abs[a], b Sign[a]];
Dual /: Sign[Dual[a_, b_]] := Sign[a];

(* Set upvalues for most built-in numeric functions where possible *)
KeyValueMap[
    Function[{fun, derriv},
        Dual /: fun[Dual[a_, b_]] := Dual[fun[a], derriv[a] * b]
    ],
    KeyDrop[{Plus, Times, Power, Divide, Subtract}] @ Select[
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