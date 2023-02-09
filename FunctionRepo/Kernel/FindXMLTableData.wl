(* Wolfram Language Package *)

BeginPackage["FunctionRepo`FindXMLTableData`", {"FunctionRepo`"}]
(* Exported symbols added here with SymbolName::usage *)
GeneralUtilities`SetUsage[FindXMLTableData,
	"FindXMLTableData[xmlData$] finds tabular data in XML files."
];

GeneralUtilities`SetUsage[FunctionRepo`XMLToData,
	"XMLToData[xml$] tries to convert XML data to WL data automatically."
];

Begin["`Private`"] (* Begin Private Context *)

FindXMLTableData[file_?FileExistsQ, rest___] := With[{
	data = Import[file, "XML"]
},
	If[ !FailureQ[data],
		FindXMLTableData[data, rest],
		$Failed
	]
];

FindXMLTableData[xml_] := FindXMLTableData[xml, _];

FindXMLTableData[xml_, (head : Rule | RuleDelayed)[patt_, val_]] := With[{
	result = FindXMLTableData[xml, patt]
},
	If[ MatchQ[result, {__XMLElement}],
		MapAt[
			Replace[#, head[patt, val], {1}]&,
			result,
			{All, 3}
		],
		result
	]
];

FindXMLTableData[xml_, patt_] := Cases[
	xml,
	xmlTablePattern[patt],
	{0, Infinity}
];

blankPattern = Verbatim[_] | Verbatim[Pattern][_, Verbatim[_]];

xmlTablePattern[blankPattern] := ReleaseHold @ Hold[
	XMLElement[_, _, list : {Repeated[_XMLElement, {2, Infinity}]}] /; And[
		SameQ @@ list[[All, 1]],
		With[{
			template = patternTemplate[First[list]]
		},
			MatchQ[Rest[list], {template ..}]
		]
	]
];

xmlTablePattern[patt_] := ReleaseHold @ Hold[
	XMLElement[_, _, list : {Repeated[_XMLElement, {2, Infinity}]}] /; And[
		SameQ @@ list[[All, 1]],
		AllTrue[list, MatchQ[patt]],
		With[{template = patternTemplate[First[list]]},
			MatchQ[Rest[list], {template ..}]
		]
	]
];

patternTemplate[{}] := {};
patternTemplate[_String] := _String;
patternTemplate[_?NumericQ] := _?NumericQ;
patternTemplate[rules : {__Rule}] := MapAt[
	patternTemplate,
	rules,
	{All, 2}
];

patternTemplate[list_List] := patternTemplate /@ list;
patternTemplate[XMLElement[tag_, tags_, elements_]] := XMLElement[
	tag,
	patternTemplate[tags],
	patternTemplate[elements]
];

XMLToData[{}] := Missing[];

XMLToData[list_List] := With[{
	data = Replace[list, el_XMLElement :> XMLToData[el], {1}]
},
	If[ MatchQ[list, {__XMLElement}] && Not[SameQ @@ list[[All, 1]]],
		Join @@ data,
		data
	]
];

XMLToData[XMLElement[tag_, keyVal : {___Rule}, {}]] := Association[keyVal];

XMLToData[XMLElement[tag_, keyVal : {___Rule}, rest_]] := Prepend[
	Association[keyVal],
	tag -> XMLToData[rest]
];

End[] (* End Private Context *)

EndPackage[]
