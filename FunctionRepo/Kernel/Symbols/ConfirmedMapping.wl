(* Wolfram Language Package *)

BeginPackage["FunctionRepo`ConfirmedMapping`", {"FunctionRepo`"}]
(* Exported symbols added here with SymbolName::usage *)
GeneralUtilities`SetUsage[ConfirmedMapping,
	"ConfirmedMapping[map$, f$, data$, $$] works like map$[f$, data$, $$], but bails out if f$ returns a failure.
ConfirmedMapping[map$] is the operator form corresponding to map$.
ConfirmedMapping[map$][f$] is the operator form corresponding to map$[f$]. ConfirmedMapping[map$, f$] and ConfirmedMapping[map$[f$]] also work."
];

Begin["`Private`"] (* Begin Private Context *)

ConfirmedMapping[mapper_, f_ -> test_, data_, rest___] := With[{
	tag = CreateUUID[] (* Use the tagged version of Enclose/Confirm because it's faster, especially for big inputs *)
},
	Enclose[
		mapper[
			Function[Null, ConfirmBy[f[##], test, Null, tag], HoldAll],
			data,
			rest
		],
		Identity,
		tag
	]
];
ConfirmedMapping[mapper_, f_, data_, rest___] := With[{
	tag = CreateUUID[] (* Use the tagged version of Enclose/Confirm because it's faster, especially for big inputs *)
},
	Enclose[
		mapper[
			Function[Null, Confirm[f[##], Null, tag], HoldAll],
			data,
			rest
		],
		Identity,
		tag
	]
];

(* Operator forms of ConfirmedMapping *)
With[{
	positionMaps = MapAt | SubsetMap
},
	(* Special case for maps that have an operator form like MapAt[f, pos] *)
	HoldPattern[ConfirmedMapping[(mapper : positionMaps)][f_, pos_][data_]] := ConfirmedMapping[mapper, f, data, pos];
	HoldPattern[ConfirmedMapping[(mapper : positionMaps)][f_, data_, args__]] := ConfirmedMapping[mapper, f, data, args];
	HoldPattern[ConfirmedMapping[(mapper : positionMaps)[f_, pos_]][data_]] := ConfirmedMapping[mapper, f, data, pos];
	With[{
		otherMaps = Except[positionMaps, _Symbol]
	},
		HoldPattern[ConfirmedMapping[(mapper : otherMaps)][f_, args__]] := ConfirmedMapping[mapper, f, args];
		HoldPattern[ConfirmedMapping[(mapper : otherMaps)[f_]][data_, rest___]] := ConfirmedMapping[mapper, f, data, rest];
		HoldPattern[ConfirmedMapping[(mapper : otherMaps)][f_][data_, rest___]] := ConfirmedMapping[mapper, f, data, rest];
	]
];

End[] (* End Private Context *)

EndPackage[]
