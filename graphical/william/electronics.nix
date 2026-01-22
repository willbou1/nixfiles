{
  pkgs,
  lib,
  config,
  ...
}:
with lib;
with config.lib.stylix;
let
  kicadThemes = pkgs.fetchFromGitHub {
    owner = "pointhi";
    repo = "kicad-color-schemes";
    rev = "68ea0402f334bdbae175f6ca924743640c07183d";
    hash = "sha256-PYgFOyK5MyDE1vTkz5jGnPWAz0pwo6Khu91ANgJ2OO4=";
  };
  kicadVersion = "${versions.major pkgs.kicad.version}.0";
  intColor = name:
    (fromHexString colors.${"base${name}-hex-r"})
    + (fromHexString colors.${"base${name}-hex-g"}) * 256
    + (fromHexString colors.${"base${name}-hex-b"}) * 65536;
in {
  home.packages = with pkgs; [
    kicad
    ngspice
    ltspice
    tina-ti

    # lab
    novnc
  ];

  home.persistence."/persist/home/william" = {
    files = [
      ".ngspice_history"
    ];
    directories = [
      ".config/kicad"
      ".local/share/tina-ti"
      ".local/share/ltspice"
    ];
  };

  xdg = {
    configFile."kicad/${kicadVersion}/colors/wdark.json".source =
      "${kicadThemes}/wdark/wdark.json";
    dataFile."ltspice/wine/drive_c/users/william/AppData/Roaming/LTspice.ini".text=
      with lib.generators; toINI {} {
        Options = {
          LastRunVersion = pkgs.ltspice.version;
          HighestRunVersion = pkgs.ltspice.version;
          Solver = 0;
          DisableSchSubdirPlaceComp = false;
          ShowCheckSum = false;
          WorkingDirectory = "C:\\users\\william\\Documents\\LTspice";
          UserCmpDir = "C:\\users\\william\\Documents\\LTspice";
          #SymbolSearchPath = ;
          #LibrarySearchPath = ;
          SymbolSearchPathDisabled = false;
          LibrarySearchPathDisabled = false;
          SpicePriority = 1;
          PlotPriority = 1;
          LastFileTypeOpened = 0;
          LastControlPanelPage = -1;
          Tabstopwidth = 4;
          grid = "off";
          GenerateExpandedListing = false;
          DefaultTrtol = 2;
          DefaultTrapIntegration = true;
          DefaultDoAntiTrapRinging = false;
          TSKbypass = false;
          Accept_3k4_Notation = true;
          NoJFETtempAdjIsr = false;
          AsciiRawFile = false;
          DoRelinearization = false;
          DoQuadraticWaves = false;
          RelinIAbsTol = "1e-09";
          RelinRelTol = 0.0025;
          RelinVAbsTol = "1e-05";
          RelinearWindowPnts = 1024;
          CompressTranOnly = true;
          SaveDeviceCurrents = true;
          SaveSubcircuitNodeVoltages = false;
          SaveSubcircuitDeviceCurrents = false;
          saveOneCurrentPerDevice = false;
          ShowSchematicGrid = false;
          OrthogonalDrag = false;
          AutoPan = true;
          DirectCompPinShorts = false;
          MinInductorDamping = true;
          UseClocktoReseedMC = false;
          EnableBetaOptimizations = false;
          BoldSchFont = true;
          ShowTitleBox = true;
          ReverseMouseWheelScroll = false;
          DefaultDeviceModels = true;
          DefaultDeviceLibraries = true;
          EnableWindowTabs = true;
          SchFontSize = 28;
          SchematicFont = 3;
          PenWidth = 1;
          DataPenWidth = 1;
          CursorThickness = 1;
          WindowSize = "30 3410 -9 1335";
          ToolBarStyle = 0;
          ToolBarIconSize = 1;
          AutoSaveOnSim = 0;
          MDIbackgroundImage = 1;
          AutoDeleteRawFiles = 0;
          DraftCursorType = 0;
          FastAccessRAM = 0.4;
          WaveformViewerFont = 0;
          WaveformViewerFontSize = 10;
          WaveformViewerBoldFont = true;
          UseXORcursor = false;
          RawCursorType = 0;
          MonochromePrinting = true;
          NoGreekMus = false;
          RadianMeasure = false;
          DefaultTileType = true;
          UseRawTempDir = false;
          #RawTempDir = ;
          CaptureAnalytics = false;
          UUID = 08145233131780258409;
          WarnOnNoIndRser = true;
        };
        Colors = {
          SchematicColor12      = intColor "00"; # background
          NetlistBackgroundColor = intColor "00";
          Grid                  = intColor "03";
          InActiveAxis          = intColor "02";

          SchematicColor0  = intColor "0C"; # wires
          SchematicColor1  = intColor "0D"; # components
          SchematicColor2  = intColor "0B"; # power nets
          SchematicColor3  = intColor "08"; # ground
          SchematicColor4  = intColor "03"; # inactive
          SchematicColor5  = intColor "05"; # labels
          SchematicColor6  = intColor "05"; # labels
          SchematicColor7  = intColor "0C"; # SPICE directives
          SchematicColor8  = intColor "0A"; # highlight
          SchematicColor9  = intColor "0D"; # alternate comp
          SchematicColor10 = intColor "08"; # errors
          SchematicColor11 = intColor "04"; # comments
          SchematicColor13 = intColor "0B"; # active net

          NetlistNormalTextColor  = intColor "05";
          NetlistCommentTextColor = intColor "03";
          NetlistDotcmdTextColor  = intColor "0D";
          ContinuationLineColor   = intColor "0C";

          WaveColor0  = intColor "00"; # hidden
          WaveColor1  = intColor "05"; # default trace
          WaveColor2  = intColor "0B";
          WaveColor3  = intColor "0D";
          WaveColor4  = intColor "0C";
          WaveColor5  = intColor "0A";
          WaveColor6  = intColor "08";
          WaveColor7  = intColor "0E";
          WaveColor8  = intColor "09";
          WaveColor9  = intColor "0D";
          WaveColor10 = intColor "0B";
          WaveColor11 = intColor "04";
          WaveColor12 = intColor "01";
          WaveColor13 = intColor "0F";
        };
        SchKeyBoardShortCut = {
          Configure_Analysis = "A";
          Run_Simulation = "Alt+R";
          Halt_Simulation = "Alt+S";
          View_SPICE_Error_Log = "Ctrl+L";
          Zoom_Area = "Z";
          Zoom_Back = "Shift+Z";
          Zoom_to_Fit = "Space";
          Draw_Wire_Mode = "W";
          Place_Ground = "G";
          Place_Voltage_Source = "V";
          Place_Resistor = "R";
          Place_Capacitor = "C";
          Place_Inductor = "L";
          Place_Diode = "D";
          Place_Component = "P";
          Place_Netname = "N";
          Place_Comment_Text = "T";
          Place_SPICE_Directive = ". >";
          Move_Mode = "M";
          Drag_Mode = "S";
          Rotate = "Ctrl+R";
          Mirror = "Ctrl+E";
          Delete_Mode = "Backspace";
          Duplicate_Mode = "Ctrl+C";
          Undo = "Ctrl+Z";
          Redo = "Ctrl+Shift+Z";
          Draw_Lines = "(none)";
          Draw_Rectangles = "(none)";
          Draw_Circles = "(none)";
          Draw_Arcs = "(none)";
          Schematic_Grid = "Ctrl+G";
          Unconn_Pin_Marks = "Ctrl+U";
          Text_Anchor_Marks = "Ctrl+A";
          Reset_T_0 = "0";
          Place_BUS_Tap = "B";
          Place_COM = "Alt+G";
        };
        AsyKeyBoardShortCut = {
          Object_Anchors = "O";
          Place_Pin = "P";
          Delete_Mode = "Backspace";
          Duplicate_Mode = "Ctrl+C";
          Move_Mode = "M";
          Drag_Mode = "S";
          Undo = "Ctrl+Z";
          Redo = "Ctrl+Shift+Z";
          Place_Comment_Text = "T";
          Draw_Lines = "L";
          Draw_Rectangles = "R";
          Draw_Arcs = "A";
          Draw_Circles = "C";
          Rotate = "Ctrl+R";
          Mirror = "Ctrl+E";
          Attribute_Editor = "Ctrl+A";
          Attribute_Window = "Ctrl+W";
          Zoom_Back = "Shift+Z";
          Zoom_In = "Z";
          Zoom_to_Fit = "Space";
          Show_Pin_Table = "(none)";
        };
        RawKeyBoardShortCut = {
          Zoom_to_Fit = "Space";
          Zoom_Area = "Z";
          Zoom_Back = "Shift+Z";
          Vertically_Autorange = "Ctrl+Y";
          Redraw_Window = "F5";
          Toggle_Grid = "Ctrl+G";
          Delete_Mode = "Backspace";
          Add_Trace = "A";
          Add_Pane = "P";
          Add_Pane_Below = "B";
          Move_Pane_Up = "U";
          Move_Pane_Down = "D";
          Undo = "Ctrl+Z";
          Redo = "Ctrl+Shift+Z";
          Run_Simulation = "Alt+R";
          Halt_Simulation = "Alt+S";
          #Open_Plot_Settings_Fi
          Open_Plot_Settings_File = "O";
          Reload_Plot_Settings_File = "Ctrl+Space";
          Place_trace_cursor = "C";
          Clear_all_cursors = "Shift+C";
          Label_cursor_position = "L";
          Place_text_on_the_plot = "T";
          Draw_an_arrow_on_the_plot = "(none)";
          Draw_a_line_on_the_plot = "I";
          Draw_a_box_on_the_plot = "R";
          Draw_a_circle_on_the_plot = "(none)";
          "Annotation_Line_Style/Color" = "(none)";
          Copy_Plot_Annotations = "Ctrl+C";
          Move_Plot_Annotations = "M";
          Drag_Plot_Annotations = "S";
          Select_Steps = "Shift+S";
          View_SPICE_Error_Log = "Ctrl+L";
        };
        NetKeyBoardShortCut = {
          Goto_Line_Number = "Ctrl+G";
          Run_Simulation = "Alt+R";
          Halt_Simulation = "Alt+S";
          Undo = "Ctrl+Z";
          Redo = "Ctrl+Shift+Z";
          View_SPICE_Error_Log = "Ctrl+L";
        };
      };
  };
}
