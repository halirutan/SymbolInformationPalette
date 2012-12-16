(* Mathematica Package *)

BeginPackage["AttributesViewer`"]

InstallAttributesViewer::usage = "InstallAttributesViewer[path] installs the palette into the specified path. \
If the path is omitted then the palette is installed into an appropriate directory inside the $UserBaseDirectory.";

AttributesViewerPalette::usage = "AttributesViewerPalette[] displays the Attributes Viewer palette.";


Begin["`Private`"]

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

InstallAttributesViewer[path_String /; DirectoryQ[path]] := InstallAttributesViewer[FileNameJoin[path, $paletteName]];
InstallAttributesViewer[] := InstallAttributesViewer[
    FileNameJoin[{$UserBaseDirectory, "SystemFiles", "FrontEnd", "Palettes", $paletteName}]];
InstallAttributesViewer[file_String] := 
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

AttributesViewerPalette[] := CreatePalette[palette[]];

palette[] := PaletteNotebook[
    DynamicModule[{},
        Panel[
            Column[{
                button["Attributes", showAttributes[]],
                button["Options", showOptions[]]
            }],
        style["Properties", $orange, Bold],
        Appearance -> "Frameless", ImageMargins -> 5
        ], SaveDefinitions -> True
    ]
]

SetAttributes[button, {HoldRest}];
button[lbl_String, cmd_] := Button[Style[lbl, buttonlabelstyle], cmd, buttonstyle];

errorDialog[msg_String] := MessageDialog[style[msg, $orange], WindowTitle -> "Error"];

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

(* Extracting an expression from the current cursor-position in the current notebook
   If no Symbol was selected, we try to expand the selection and select the surrounding expression.
*)
getSymbol[nb_] := Block[{sel, heldSymbol},
    If[(sel = NotebookRead[nb]) === {}, SelectionMove[nb, All, Expression];
    sel = NotebookRead[nb];
    ];
    heldSymbol = MakeExpression[sel, StandardForm];
    If[ sel =!= {} && Head[First[heldSymbol]] === Symbol,
        heldSymbol,
    $Failed]
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
    Style[title, $orange, Bold], ImageSize -> {150, Automatic}
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

optionButton[{symbol_, defaultValue_}, col_, textcol_] := Button[
    style[ToString[symbol], textcol],
    displayOptionUsage[{symbol, defaultValue}],
    Appearance -> "Frameless", Alignment -> Left, ImageSize -> {300, 22}, Background -> col, ImageMargins -> 0
]

SetAttributes[displayOptionUsage, {HoldAll}];
displayOptionUsage[{symbol_Symbol, defaultValue_}]:= With[{
    dval = ToString[defaultValue,InputForm],
    usg = If[Unevaluated[symbol::usage]===symbol::usage, "No usage message available.", symbol::usage]},
    CreateDialog[{
    Cell[StyleBox[usg, "MSG"], "PrintUsage", CellMargins -> 0],
    Cell[StyleBox[dval, "Item"], CellMargins -> 0, FontFamily->"Courier New", FontSize->12]}, 
    WindowFrame -> "Frameless", Background -> $bright2, WindowSize -> {300,All}];
];
displayOptionUsage[{symbol_String, defaultValue_}] := CreateDialog[{
    Cell[BoxData[StyleBox[RowBox[{symbol,": is a string-option and has no usage message."}],"MSG"]], "PrintUsage", CellMargins -> 0],
    Cell[StyleBox[ToString[defaultValue,InputForm], "Item"], CellMargins -> 0]}, 
    WindowFrame -> "Frameless", Background -> $bright2];

optionsDialog[symbol_Symbol] := Module[{opts = Options[symbol]},
    Switch[opts,
        {},
        errorDialog["Symbol has no Options."],
        _,
        opts = opts /. (RuleDelayed | Rule) -> List;
        CreateDialog[Pane[Column[
            Mouseover[optionButton[#, $bright1, $dark2], optionButton[#, $bright2, $orange]]& /@ opts, Spacings -> 0], 
            ImageSize -> {300, 300}, ImageSizeAction -> "Scrollable", Scrollbars -> {None, Automatic}, AppearanceElements -> None], 
        Background -> $bright2, WindowTitle -> ToString[symbol]
        ]
    ]
];

End[]
EndPackage[]