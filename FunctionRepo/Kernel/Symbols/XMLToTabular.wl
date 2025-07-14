(* Wolfram Language Package *)

BeginPackage["FunctionRepo`XMLToTabular`", {"FunctionRepo`"}]

Needs["GeneralUtilities`" -> "GU`"]

FunctionRepo`XMLElementToAssociation;

(* Exported symbols added here with SymbolName::usage *)
GU`SetUsage[XMLToTabular,
	"XMLToTabular[xml$] converts XML data to a Tabular format."
];

Begin["`Private`"] (* Begin Private Context *)


XMLElementToAssociation[{}] := <||>;
XMLElementToAssociation[XMLObject["Document"][_, xml_XMLElement, _]] := XMLElementToAssociation[xml];

XMLElementToAssociation[XMLElement[tag_, rules_, body_]] := With[{
	base = KeyMap[ExtendedKey[tag, #]&] @ Association[rules],
	bodyData = XMLElementToAssociation[body]
},
	If[ ListQ[bodyData],
		Join[
			ConstantArray[base, Length[bodyData]],
			KeyMap[ExtendedKey[tag, #]&] /@ bodyData,
			2
		],
		Join[base, bodyData]
	]
];

XMLElementToAssociation[{el_XMLElement}] := XMLElementToAssociation[el];
XMLElementToAssociation[l : {__XMLElement}] := Module[{
	assocs = Flatten @ Map[XMLElementToAssociation, l]
},
	assocs[[All, "Index"]] = Range @ Length[assocs];
	assocs
];
XMLElementToAssociation[{el_}] := <|"XMLValues" -> el|>;
XMLElementToAssociation[l_List] := <|"XMLValues" -> l|>;
XMLElementToAssociation[_] := <||>;


sortColumns[tab_] := KeyTake[tab, Sort @ ColumnKeys[tab]];


XMLToTabular[xml_List] := sortColumns @ Tabular[Flatten @ Map[XMLElementToAssociation, xml]]
XMLToTabular[xml_] := sortColumns @ Tabular[Flatten @ XMLElementToAssociation[xml]];


End[] (* End Private Context *)

EndPackage[]
