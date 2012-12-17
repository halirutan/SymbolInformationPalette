(* :Title: Package SymbolInformationPalette *)

(* :Context: SymbolInformationPalette` *)

(* :Author: halirutan *)

(* :Summary: 
    This package provides a palette to show usages, options and attributes of symbols.
*)

(* :Package Version: 1.0 *)

(* :Mathematica Version: 9.0 *)

(* :Copyright: Copyright 2012, halirutan  *)

(* :History:
*)

(* :Keywords:
    palette, options, attributes, usage
*)

(* :Limitations:  *)

(* :Discussion:  *)

BeginPackage["SymbolInformationPalette`"]

InstallSymbolInformationPalette::usage = "InstallAttributesViewer[path] installs the palette into the specified path. \
If the path is omitted then the palette is installed into an appropriate directory inside the $UserBaseDirectory.";

SymbolInformationPalette::usage = "AttributesViewerPalette[] displays the Attributes Viewer palette.";

Begin["`Private`"]

InstallSymbolInformationPalette[path_String /; DirectoryQ[path]] := InstallSymbolInformationPalette[FileNameJoin[path, $paletteName]];
InstallSymbolInformationPalette[] := InstallSymbolInformationPalette[
    FileNameJoin[{$UserBaseDirectory, "SystemFiles", "FrontEnd", "Palettes", $paletteName}]];
InstallSymbolInformationPalette[file_String] := 
    If[Not[FileExistsQ[file]] || 
        ChoiceDialog[
            Row[{
                style["File exists:",$orange],
                Spacer[5],
                Tooltip[style[FileBaseName[file]<>"."<>FileExtension[file],$orange,Bold], style[file]],
                Spacer[5],
                style["Overwrite?",$orange]}
            ]
        ], (* if we are allowed to write *) 
        Export[file, CreatePalette[palette[]]]
    ];

SymbolInformationPalette[] := CreatePalette[palette[]];

palette[] := PaletteNotebook[
    DynamicModule[{},
        Panel[
            Column[{
               button["Usage", showUsageDialog[]],
                button["Options", showOptions[]],
                button["Attributes", showAttributes[]]
            }],
        style["Information", $orange],
        Appearance -> "Frameless", ImageMargins -> 5
        ], SaveDefinitions -> True
    ]
]


(* ::Section:: *)
(* Helper functions and values *)

$orange = RGBColor[203/255, 5/17, 22/255];
$blue = RGBColor[38/255, 139/255, 14/17];

$bright1 = RGBColor[253/255, 82/85, 227/255];
$bright2 = RGBColor[14/15, 232/255, 71/85];

$dark1 = RGBColor[49/85, 161/255, 161/255];
$dark2 = RGBColor[7/255, 18/85, 22/85];

$paletteName = "Attributes Viewer.nb";

buttonlabelstyle = Sequence[12, $bright1, FontFamily -> "Helvetica"];
buttonstyle = Sequence[ImageMargins -> 0, Appearance -> "Frameless", 
    Background -> $dark1, ImageSize -> {90, 90/(2 GoldenRatio)}];

style[str_String, more___] := Style[str, 12, FontFamily -> "Helvetica", $dark1, more];

SetAttributes[button, {HoldRest}];
button[lbl_String, cmd_] := Button[Style[lbl, buttonlabelstyle], cmd, buttonstyle];

errorDialog[msg_String] := MessageDialog[style[msg, $orange], WindowTitle -> "Error"];

(* Extracting an expression from the current cursor-position in the current notebook
   If no Symbol was selected, we try to expand the selection and select the surrounding expression.
*)
getSymbol[nb_] := Block[{sel, heldSymbol},
    If[(sel = NotebookRead[nb]) === {}, SelectionMove[nb, All, Expression];
    sel = NotebookRead[nb];
    ];
    heldSymbol = MakeExpression[sel, StandardForm];
    If[ sel =!= {} && heldSymbol[[1,0]] === Symbol,
        heldSymbol,
        $Failed]
    ];


(* ::Section:: *)
(* Showing the usage of a symbol *)

showUsageDialog[] := Module[{nb = SelectedNotebook[], usg, symbol},
    symbol = getSymbol[nb];
    If[symbol === $Failed,
        errorDialog["No Symbol was selected."],
        usg = MessageName[#, "usage"] & @@ symbol;
        usg = If[Head[usg] === MessageName, "No usage message available.", usg];
        CreateDialog[DisplayForm[Cell[StyleBox[usg, "MSG"], "PrintUsage", CellMargins -> 0, CellSize -> {300, Automatic}]], 
            WindowTitle -> ToString @@ symbol, Background->$bright1, WindowFrame->"Palette"]
    ]
]


showAttributes[] := Module[{nb = SelectedNotebook[], symbol},
    symbol = getSymbol[nb];
    If[symbol === $Failed,
    (* no symbol selected -> error *)
    errorDialog["No Symbol was selected."],
    (* else, a valid symbol was selected *)
    CreateDialog[attributePanel[Attributes @@ symbol], WindowTitle -> ToString @@ symbol]
    ]
];

showOptions[] := Module[{nb = SelectedNotebook[], symbol},
    symbol = getSymbol[nb];
    If[symbol === $Failed,
    (* no symbol selected -> error *)
    errorDialog["No Symbol was selected."],
    (* else, a valid symbol was selected *)
    optionsDialog @@ symbol
    ]
];

$holdAttributes = {HoldFirst, HoldRest, HoldAll, HoldAllComplete, SequenceHold, NHoldFirst, NHoldRest, NHoldAll};
$protectAttributes = {Protected, ReadProtected, Locked};
$mathAttributes = {Listable, Orderless, Constant, Flat, OneIdentity};
$otherAttributes = {NumericFunction, Stub, Temporary};

makeAttributeCol[attr_, inAttr_, title_] := Panel[Grid[
    Table[{style[ToString[a], Which[MemberQ[inAttr,a], $blue, True, $dark1]], Checkbox[MemberQ[inAttr, a], Enabled -> False]},
        {a, attr}], 
    Alignment -> {Right, Top}],
    (* options to Panel *)
    style[title, $orange], ImageSize -> {150, Automatic}
];

attributePanel[in_] := Panel[
    Grid[{{
        makeAttributeCol[$holdAttributes, in, "Hold"],
        makeAttributeCol[$mathAttributes, in, "Math"],
        makeAttributeCol[$otherAttributes, in, "Other"]
        }, {
        SpanFromAbove, 
        SpanFromAbove,
        makeAttributeCol[$protectAttributes, in, "Protection"]}
        }, Alignment -> {Left, Top}]
]

(* ::Section:: *)
(* Get Options from a symbol *)

(*
This represents a button which shows an option name and changes its color hover.
When the button is clicked, it changes itself to a cell showing the usage message if available and the 
default value of this option.
*)
usageButton[{symbol_Symbol, defaultValue_}] := 
    DynamicModule[{state = False, label, dval, usg, sym, pane},
        sym = ToString[symbol];
        dval = ToString[defaultValue, InputForm];
        usg = MessageName[symbol, "usage"] ;
        usg = If[Head[usg] === MessageName, "No usage message available.", usg];        
        pane[textcol_, col_] := Pane[style[sym, textcol], {300, Automatic}, BaseStyle -> {Background -> col}, ImageMargins->0];
  
        label[False] = Mouseover[pane[$dark2, $bright1], pane[$orange, $bright2]];
        label[True] = Tooltip[DisplayForm[Cell[StyleBox[usg, "MSG"], "PrintUsage", CellMargins -> 0, CellSize -> {300, Automatic}]],
            "Default value: "<> dval, TooltipDelay -> 1, TooltipStyle -> {$dark2, FontFamily -> "Helvetica", 
                Background -> $bright2, CellFrameColor -> $dark1, CellFrame -> 2}];

         Dynamic@Deploy[EventHandler[label[state], {"MouseUp" :> (state = Not[state])}]]
]


optionsDialog[symbol_Symbol] := Module[{opts = Options[symbol]},
    Switch[opts,
        {},
        errorDialog["Symbol has no Options."],
        _,
        opts = opts /. (RuleDelayed | Rule) -> List;
        CreateDialog[DialogNotebook[Column[ usageButton /@ opts, Spacings -> 0] 
        ], WindowSize -> {350, 300}, WindowElements -> {"VerticalScrollBar"}, WindowFrame -> "Palette", Background -> $bright1, WindowTitle -> ToString[symbol]
        ]
    ]
];

(* Create buttons to show/install the palette when loading the package *)
With[{imgsz = ImageSize -> {100,30}},
    Print[Panel[
        Column[{
            Button[Style["Show Palette",buttonlabelstyle], SymbolInformationPalette[],Method->"Queued", imgsz, buttonstyle], 
            Button[Style["Install Palette",buttonlabelstyle], InstallSymbolInformationPalette[], Method->"Queued", imgsz, buttonstyle],
            PasteButton[Style["Install to path",buttonlabelstyle], 
                Defer[InstallSymbolInformationPalette["/your/path/PaletteName.nb"]],imgsz, buttonstyle]            
            }],style["Please choose:",$orange],Background->$bright1]
    ];
];

End[];
EndPackage[];


