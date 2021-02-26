(* Wolfram Language Package *)

BeginPackage["FunctionRepo`MonitorFile`", {"FunctionRepo`", "GeneralUtilities`"}]
(* Exported symbols added here with SymbolName::usage *)
GeneralUtilities`SetUsage[MonitorFile, 
    "MonitorFile[Dynamic[contents$], fileName$] creates a dynamic that shows the contents of file fileName$ and updates contents$."
];

Begin["`Private`"] (* Begin Private Context *) 

MonitorFile[
    Dynamic[contents_], 
    file_String?FileExistsQ, 
    importFunction : _ : Import,
    formatFun : _ : Identity
] := DynamicModule[{
    display = "",
    modificationDate
}, 
    DynamicWrapper[
        DynamicWrapper[
            Column[{
                Dynamic[modificationDate], 
                Dynamic[display, TrackedSymbols :> {display}]
            }, Alignment -> Left]
            ,
            modificationDate; (* This triggers the update *)
            contents = importFunction[file];
            display = formatFun[contents],
            SynchronousUpdating -> False, 
            TrackedSymbols :> {modificationDate}
        ], 
        modificationDate = Information[File[file], "LastModificationDate"],
        SynchronousUpdating -> True,
        UpdateInterval -> 1.
    ]
];

End[] (* End Private Context *)

EndPackage[]