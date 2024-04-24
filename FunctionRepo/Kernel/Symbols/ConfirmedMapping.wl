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

(* Special case for maps that have an operator form like MapAt[f, pos] *)
positionMaps = MapAt | SubsetMap;
ConfirmedMapping[map : positionMaps][f_, pos_][data_] := ConfirmedMapping[map, f, data, pos];

(* Generic operator forms *)
ConfirmedMapping[mapper_[f_, rest___]][data_] := ConfirmedMapping[mapper, f, data, rest];
ConfirmedMapping[mapper_][f_][data_] := ConfirmedMapping[mapper, f, data];

(* Operator form of ConfirmedMapping itself *)
ConfirmedMapping[mapper_][f_, args__] := ConfirmedMapping[mapper, f, args];


End[] (* End Private Context *)

EndPackage[]
