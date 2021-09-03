(* Wolfram Language Package *)

BeginPackage["FunctionRepo`KeyComplete`", {"FunctionRepo`", "GeneralUtilities`"}]
(* Exported symbols added here with SymbolName::usage *)
GeneralUtilities`SetUsage[KeyComplete, "KeyComplete[assoc$, {key$1, key$2, $$}, f$] is similar to KeyTake[assoc$, {key$1, key$2, $$}], but any key$i absent \
from assoc$ will be added as key$i -> f$[key$i]."
];

Begin["`Private`"] (* Begin Private Context *)

(* KeyComplete *)

KeyComplete[list_List, f_][arg_] := KeyComplete[arg, list, f];

KeyComplete[assoc_?AssociationQ, keyList_List, f_] := With[{
	keys = Keys[assoc]
},
	With[{complement = Complement[keyList, keys]},
		KeyTake[
			If[ complement === {}
				,
				assoc
				,
				Join[
					assoc,
					AssociationThread[
						complement,
						Map[f, complement]
					]
				]
			],
			keyList
		]
	]
];

KeyComplete[list : {__?AssociationQ}, rest__] := Map[KeyComplete[#, rest]&, list];

End[] (* End Private Context *)

EndPackage[]