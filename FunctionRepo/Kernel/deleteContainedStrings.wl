(* Wolfram Language Package *)

BeginPackage["FunctionRepo`deleteContainedStrings`", {"FunctionRepo`"}]
(* Exported symbols added here with SymbolName::usage *)
deleteContainedStrings::usage = "deleteContainedStrings[{str1, str2, ...}] deletes every string that is a substring of at least one other string in the list. Preserves ordering.";

Begin["`Private`"] (* Begin Private Context *)

Options[deleteContainedStrings] = Options[StringContainsQ];
deleteContainedStrings[{}, ___] := {};
deleteContainedStrings[strings : {__String}, opts : OptionsPattern[]] := Module[{
    sorted = ReverseSortBy[strings, StringLength]
},
    SortBy[
        DeleteDuplicates[sorted, StringContainsQ[##, opts] &],
        FirstPosition[strings, #, Missing[], {1}, Heads -> False] &
    ]
];

End[] (* End Private Context *)

EndPackage[]