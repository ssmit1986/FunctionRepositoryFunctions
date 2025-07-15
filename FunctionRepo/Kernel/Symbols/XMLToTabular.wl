(* Wolfram Language Package *)

BeginPackage["FunctionRepo`XMLToTabular`", {"FunctionRepo`"}]

Needs["GeneralUtilities`" -> "GU`"]

FunctionRepo`XMLElementToAssociation;

(* Exported symbols added here with SymbolName::usage *)
GU`SetUsage[XMLToTabular,
	"XMLToTabular[xml$] converts XML data to a Tabular format."
];

Begin["`Private`"] (* Begin Private Context *)

$indexKey = "Index";
$valuesKey = "XMLValues";
$groupTag = "Grouping";
$tagPath = {};

tagIndex[] := Replace[
	$groupTagCounter[$tagPath],
	{
		_Integer :> (++$groupTagCounter[$tagPath]),
		_ :> ($groupTagCounter[$tagPath] = 1)
	}
]

addIndices[list_] := Module[{newList = list},
	newList[[All, $indexKey]] = Range @ Length[newList];
	newList
];

combineAssociations[assocs_List] := If[
	DuplicateFreeQ[Join @@ Keys[assocs]],
	Join @@ assocs,
	addIndices @ assocs
];

addGroupTag[tag_][assoc_] := Prepend[assoc, $groupTag -> tag <> "-" <> IntegerString[tagIndex[]]];

XMLElementToAssociation[{}] := <||>;
XMLElementToAssociation[XMLObject[_][_, xml_XMLElement, __]] := XMLElementToAssociation[xml];

XMLElementToAssociation[XMLElement[tag_, {}, {body : Except[_XMLElement]}]] := <|tag -> body|>
XMLElementToAssociation[XMLElement[tag_, rules_, body_]] := Block[{
	$tagPath = {$tagPath, tag}
},
	With[{
		base = Association[rules],
		bodyData = XMLElementToAssociation[body]
	},
		If[ ListQ[bodyData],
			Join[
				ConstantArray[
					KeyMap[ExtendedKey[tag, #]&] @ addGroupTag[tag] @ base,
					Length[bodyData]
				],
				KeyMap[ExtendedKey[tag, #]&] /@ bodyData,
				2
			],
			KeyMap[ExtendedKey[tag, #]&] @ Join[
				base,
				bodyData
			]
		]
	]
];

XMLElementToAssociation[{el_XMLElement}] := XMLElementToAssociation[el];
XMLElementToAssociation[l : {__XMLElement}] := combineAssociations @ Flatten @ Map[XMLElementToAssociation, l];
XMLElementToAssociation[{el_}] := <|$valuesKey -> el|>;
XMLElementToAssociation[list_List] := combineAssociations @ Flatten @ Replace[
	list,
	{
		xml_XMLElement :> XMLElementToAssociation[xml],
		other_ :> <|$valuesKey -> other|>
	},
	{1}
];
XMLElementToAssociation[_] := <||>;



sortFun[e1_ExtendedKey, e2_ExtendedKey] /; e1 === e2 := 0;
sortFun[e1 : ExtendedKey[fst__, "Index" | "Grouping"], e2 : ExtendedKey[fst__, _]] := 1;
sortFun[e1 : ExtendedKey[fst__, _], e2 : ExtendedKey[fst__, "Index" | "Grouping"]] := -1;
sortFun[e1_ExtendedKey, e2_ExtendedKey] := With[{
	n1 = Length[e1],
	n2 = Length[e2]
},
	If[ n1 === n2,
		Order[e1, e2],
		Replace[
			Order @@ Take[{e1, e2}, All, Min[n1, n2]],
			0 :> Sign @ Subtract[n2, n1]
		]
	]
];
sortFun[_, _ExtendedKey] := 1;
sortFun[_ExtendedKey, _] := -1
sortFun[o1_, o2_] := Order[o1, o2]

sortColumns[tab_] := KeyTake[tab, Sort[ColumnKeys[tab], sortFun]];

SetAttributes[blockKeys, HoldRest];
blockKeys[{indKey_, valKey_, groupTag_}, expr_] := Block[{
	$indexKey = indKey,
	$valuesKey = valKey,
	$groupTag = groupTag,
	$tagPath = {},
	$groupTagCounter
},
	expr
];

Options[XMLToTabular] = {
	"IndexKey" -> "Index",
	"ValuesKey" -> "XMLValues",
	"GroupTag" -> "Grouping"
};

XMLToTabular[xml_List, opts : OptionsPattern[]] := blockKeys[
	{OptionValue["IndexKey"], OptionValue["ValuesKey"], OptionValue["GroupTag"]},
	sortColumns @ Tabular[addIndices @ Flatten @ Map[XMLElementToAssociation, xml]]
];

XMLToTabular[xml_, opts : OptionsPattern[]] := blockKeys[
	{OptionValue["IndexKey"], OptionValue["ValuesKey"], OptionValue["GroupTag"]},
	sortColumns @ Tabular[Flatten @ {XMLElementToAssociation[xml]}]
];


End[] (* End Private Context *)

EndPackage[]
