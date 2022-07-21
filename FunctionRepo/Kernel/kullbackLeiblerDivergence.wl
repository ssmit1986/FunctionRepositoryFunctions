(* Wolfram Language Package *)

BeginPackage["FunctionRepo`kullbackLeiblerDivergence`", {"FunctionRepo`"}]
(* Exported symbols added here with SymbolName::usage *)
kullbackLeiblerDivergence::usage = "kullbackLeiblerDivergence[P, Q] computes the Kullback-Leibler divergence from distribution Q to P";

Begin["`Private`"] (* Begin Private Context *)

Options[kullbackLeiblerDivergence] = {
	Method -> Expectation,
	Assumptions :> $Assumptions
};
kullbackLeiblerDivergence::method =  "Method `1` is not Expectation or NExpectation.";
kullbackLeiblerDivergence::randomSample = "Unable to sample from `1` and `2`. Cannot use Method NExpectation.";
kullbackLeiblerDivergence::supportPQ =  "The support of `1` is not a subset of the support of `2`.";
kullbackLeiblerDivergence::supportValidationFail  = "Failed to verify that the support of `1` is a subset of the support of `2`. Calculation will still be attempted.";

(* The divergence from a distribution to itself is 0 *)
kullbackLeiblerDivergence[p_, p_, OptionsPattern[]] := 0;

kullbackLeiblerDivergence[p_?DistributionParameterQ, q_?DistributionParameterQ, opts : OptionsPattern[]] := Block[{
	$Assumptions = OptionValue[Assumptions]
},
	Module[{
		methodSpec = Replace[OptionValue[Method], sym : Except[_List] :> {sym}],
		method, methodOpts,
		domainp, domainq,
		assumptions,
		rv, Global`x
	},
		If[ FreeQ[{p, q}, \[FormalX]],
			(* 
				If p and q are free of FormalX it can be used as a dummy variable, which typesets nicer if Expectation returns unevaluated. 
				Otherwise use a temporary Global`x localized within this Module. Most of the time x shouldn't appear in the output anyway.
			*)
			Global`x = \[FormalX]
		];
		{method, methodOpts} = TakeDrop[methodSpec, 1];
		method = First[method];
		Switch[ method,
			Expectation,
				Null,
			NExpectation,
				With[{rand = Quiet[RandomVariate[#, 5] & /@ {p, q}]}, (* 
					Test if p and q can be sampled from *)
					If[! AllTrue[rand, ArrayQ],
						Message[kullbackLeiblerDivergence::randomSample, p, q];
						Return[$Failed, Module]
					]
				],
			_,  (Message[kullbackLeiblerDivergence::method, method]; Return[$Failed, Module])
		];
		domainp = DistributionDomain[p];
		domainq = DistributionDomain[q];
		assumptions = And @@ Map[DistributionParameterAssumptions, {p, q}];
		Switch[(* Test supports of p and q *)
			Assuming[assumptions, Simplify @ supportSubSetQ[domainp, domainq]],
			True, Null,
			False,
			(
				Message[kullbackLeiblerDivergence::supportPQ, p, q];
				Return[Undefined, Module]
			),
			_, Message[kullbackLeiblerDivergence::supportValidationFail, p, q]
		];
		rv = Replace[domainp, (* initialize dummy variable used in Expectation or NExpectation *)
			{
				l : {domainPattern ..} :> Array[Global`x, Length[l]],
				_ -> Global`x
			}
		];
		assumptions = Simplify @ And[
			assumptions,
			Block[{Statistics`Library`RealIntegerQ = Element[# ,Integers]&},
				If[ ListQ[rv],
					And @@ MapThread[Statistics`DistributionsCommonDump`DomainMemberQ, {domainp, rv}],
					Statistics`DistributionsCommonDump`DomainMemberQ[domainp, rv]
				]
			]
		];
		Assuming[ assumptions,
			Simplify[
				method[
					Refine[LogLikelihood[p, {rv}] - LogLikelihood[q, {rv}]],
					Distributed[rv, p],
					Sequence @@ methodOpts
				],
				TimeConstraint -> {2, 10}
			]
		]
	]
];

empiricalDistDomainPattern = {Except[_Span | _Interval | _List] ..};
domainPattern = Alternatives[
	empiricalDistDomainPattern,(* Empirical distributions *)
	_Span,(* Discrete distributions *)
	_Interval  (*Continuous distributions*)
];

(*Multi-dimensional distributions *)
supportSubSetQ[p : {domainPattern ..}, q : {domainPattern ..}] /; Length[p] =!= Length[q] := False;
supportSubSetQ[p : {domainPattern ..}, q : {domainPattern ..}] := And @@ MapThread[supportSubSetQ, {p, q}];

supportSubSetQ[Span[p__?NumericQ], q_] := supportSubSetQ[Range[p], q];
supportSubSetQ[p_, Span[q__?NumericQ]] := supportSubSetQ[p, Range[q]];

supportSubSetQ[p : empiricalDistDomainPattern, q : empiricalDistDomainPattern] := SubsetQ[q, p];

(* Backups for infinite/symbolic spans *)
supportSubSetQ[p_?( VectorQ[#, IntegerQ] &), q : Span[_, __]] := With[{minmaxp = MinMax[p]},
	q[[1]] <= minmaxp[[1]] && minmaxp[[2]] <= q[[2]]
];

supportSubSetQ[p : Span[_, __], q_?( VectorQ[#, IntegerQ] &)] := With[{minmaxq = MinMax[q]},
	minmaxq[[1]] <= p[[1]] && p[[2]] <= minmaxq[[2]]
];

supportSubSetQ[
	Span[pmin_, pmax_] | Interval[{pmin_, pmax_}],
	Span[qmin_, qmax_] | Interval[{qmin_, qmax_}]
] := qmin <= pmin && pmax <= qmax;


supportSubSetQ[p_Interval, q_Interval] := With[{int = IntervalIntersection[p, q]},
	Condition[
		And @@ MapThread[Equal, {First[int], First[p]}],
		Head[int] === Interval
	]
];

supportSubSetQ[p_, q_] /; Head[p] =!= Head[q] := False;
supportSubSetQ[__] := Undefined;

End[] (* End Private Context *)

EndPackage[]