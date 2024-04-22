(* Wolfram Language Package *)

BeginPackage["FunctionRepo`ConfirmedMapping`", {"FunctionRepo`"}]
(* Exported symbols added here with SymbolName::usage *)
GeneralUtilities`SetUsage[ConfirmedMapping,
	"ConfirmedMapping[map$, f$, data$, $$] works like map$[f$, data$, $$], but bails out if f$ returns a failure.
ConfirmedMapping[map$] is the operator form corresponding to map$.
ConfirmedMapping[map$][f$] is the operator form corresponding to map$[f$]. ConfirmedMapping[map$, f$] and ConfirmedMapping[map$[f$]] also work."
];

Begin["`Private`"] (* Begin Private Context *)

ConfirmedMapping[mapper_, f_, dat_, rest___] := Module[{
	data = dat
},
	Enclose[
		mapper[
			Function[Null, Confirm @ f[##], HoldAll],
			data,
			rest
		]
	]
];
ConfirmedMapping[mapper_[f_]][args__] := ConfirmedMapping[mapper, f, args];
ConfirmedMapping[mapper_][f_, args__] := ConfirmedMapping[mapper, f, args];
ConfirmedMapping[mapper_][f_][args__] := ConfirmedMapping[mapper, f, args];
ConfirmedMapping[mapper_, f_][args__] := ConfirmedMapping[mapper, f, args];

End[] (* End Private Context *)

EndPackage[]
