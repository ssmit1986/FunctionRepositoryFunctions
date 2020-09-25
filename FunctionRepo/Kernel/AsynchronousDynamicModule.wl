(* Wolfram Language Package *)

BeginPackage["FunctionRepo`AsynchronousDynamicModule`", {"FunctionRepo`", "GeneralUtilities`"}]
(* Exported symbols added here with SymbolName::usage *)
GeneralUtilities`SetUsage[AsynchronousDynamicModule,
    "AsynchronousDynamicModule[DynamicModule[vars$, expr$, $$, Initialization :> init$, $$]] modifies a DynamicModule to accomodate slow initialization code."
];

Begin["`Private`"] (* Begin Private Context *) 

Attributes[AsynchronousDynamicModule] = {HoldAll};

AsynchronousDynamicModule[
    DynamicModule[{vars___}, content_, opts1___, Initialization :> init_, opts2___], 
    placeHolder : _ : ProgressIndicator[Appearance -> "Necklace"],
    initVar : _Symbol : initQ
] := AsynchronousDynamicModule[
    {vars},
    content,
    init,
    placeHolder,
    initVar,
    opts1,
    opts2
];

AsynchronousDynamicModule[
    {vars___},
    content_,
    init_,
    placeHolder : _ : ProgressIndicator[Appearance -> "Necklace"],
    initVar : _Symbol : initQ,
    opts : OptionsPattern[DynamicModule]
] := DynamicModule[{
    vars,
    initVar = False
},
    Dynamic[
        If[ !TrueQ[initVar],
            placeHolder,
            content
        ],
        TrackedSymbols :> {initVar}
    ],
    SynchronousInitialization -> False,
    Initialization :> (
        initVar = False;
        init;
        initVar = True
    ),
    opts
];

AsynchronousDynamicModule[dm_DynamicModule, ___] := dm;

End[] (* End Private Context *)

EndPackage[]