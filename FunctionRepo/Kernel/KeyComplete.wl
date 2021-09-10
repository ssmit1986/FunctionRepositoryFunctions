(* Wolfram Language Package *)

BeginPackage["FunctionRepo`KeyComplete`", {"FunctionRepo`", "GeneralUtilities`"}]
(* Exported symbols added here with SymbolName::usage *)
GeneralUtilities`SetUsage[KeyComplete, "KeyComplete[assoc$, {key$1, key$2, $$}, f$] is similar to KeyTake[assoc$, {key$1, key$2, $$}], but any key$i absent \
from assoc$ will be added as key$i -> f$[key$i]."
];

Begin["`Private`"] (* Begin Private Context *)

(* Operator form *)
KeyComplete[list_List, f_][arg_] := KeyComplete[arg, list, f];

(* Trivial cases *)
KeyComplete[<||>, list_, f_] := AssociationThread[list, f /@ list];

KeyComplete[_Association?AssociationQ, {}, _] := <||>; 

(* Main definition *)
KeyComplete[assoc_Association?AssociationQ, keyList_List, f_] := With[{
	complement = Complement[keyList, Keys[assoc]]
},
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
];

KeyComplete[list : {__?AssociationQ}, rest__] := Map[KeyComplete[#, rest]&, list];

End[] (* End Private Context *)

EndPackage[]