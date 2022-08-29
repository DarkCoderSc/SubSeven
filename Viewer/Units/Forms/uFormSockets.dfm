object FormSockets: TFormSockets
  Left = 0
  Top = 0
  BorderStyle = bsNone
  BorderWidth = 2
  Caption = 'sub7 sockets'
  ClientHeight = 252
  ClientWidth = 498
  Color = clGray
  Constraints.MinHeight = 240
  Constraints.MinWidth = 320
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Arial'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 14
  object CaptionBar: TS7CaptionBar
    Left = 0
    Top = 0
    Width = 498
    Height = 19
    Caption = 'sub7 sockets'
    BorderIcons = [biSystemMenu, biMinimize, biMaximize]
    S7Form = SubSevenForms
    Dockable = False
    Transparent = False
    Collapsible = True
    TextCenter = False
    MainColor = 16744576
    SecondaryColor = clBlack
    Align = alTop
  end
  object PanelClient: TS7Panel
    Left = 0
    Top = 19
    Width = 498
    Height = 233
    BorderTop = 2
    BorderLeft = 2
    BorderRight = 2
    BorderBottom = 2
    Color = 8404992
    BorderColor = clBlack
    Align = alClient
    TabOrder = 0
    object VST: TVirtualStringTree
      AlignWithMargins = True
      Left = 2
      Top = 28
      Width = 494
      Height = 203
      Margins.Left = 2
      Margins.Top = 0
      Margins.Right = 2
      Margins.Bottom = 2
      Align = alClient
      BorderStyle = bsNone
      Color = 8404992
      Colors.BorderColor = 15987699
      Colors.DisabledColor = clGray
      Colors.DropMarkColor = clBlack
      Colors.DropTargetColor = clBlack
      Colors.DropTargetBorderColor = clBlack
      Colors.FocusedSelectionColor = clBlack
      Colors.FocusedSelectionBorderColor = clBlack
      Colors.GridLineColor = 5581568
      Colors.HeaderHotColor = clBlack
      Colors.HotColor = clBlack
      Colors.SelectionRectangleBlendColor = clBlack
      Colors.SelectionRectangleBorderColor = clBlack
      Colors.SelectionTextColor = clWhite
      Colors.TreeLineColor = 9471874
      Colors.UnfocusedColor = clWhite
      Colors.UnfocusedSelectionColor = 5581568
      Colors.UnfocusedSelectionBorderColor = 5581568
      Ctl3D = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      Header.AutoSizeIndex = 1
      Header.Font.Charset = DEFAULT_CHARSET
      Header.Font.Color = clBlack
      Header.Font.Height = -11
      Header.Font.Name = 'Arial'
      Header.Font.Style = []
      Header.Options = [hoAutoResize, hoColumnResize, hoDrag, hoShowSortGlyphs, hoVisible, hoHeaderClickAutoSort]
      Header.ParentFont = False
      Header.SortColumn = 0
      IncrementalSearch = isAll
      ParentCtl3D = False
      ParentFont = False
      PopupMenu = PopupMenuAction
      TabOrder = 0
      TreeOptions.PaintOptions = [toHideFocusRect, toShowButtons, toShowDropmark, toUseBlendedImages, toUseBlendedSelection]
      TreeOptions.SelectionOptions = [toFullRowSelect, toRightClickSelect]
      OnCompareNodes = VSTCompareNodes
      OnFocusChanged = VSTFocusChanged
      OnFreeNode = VSTFreeNode
      OnGetText = VSTGetText
      OnGetNodeDataSize = VSTGetNodeDataSize
      OnInitNode = VSTInitNode
      Touch.InteractiveGestures = [igPan, igPressAndTap]
      Touch.InteractiveGestureOptions = [igoPanSingleFingerHorizontal, igoPanSingleFingerVertical, igoPanInertia, igoPanGutter, igoParentPassthrough]
      Columns = <
        item
          Position = 0
          Text = 'socket'
          Width = 80
        end
        item
          Position = 1
          Text = 'socket type'
          Width = 214
        end
        item
          Position = 2
          Spacing = 2
          Text = 'thread id'
          Width = 80
        end
        item
          Position = 3
          Text = 'started'
          Width = 120
        end>
    end
    object PanelHeader: TS7Panel
      Left = 0
      Top = 0
      Width = 498
      Height = 28
      BorderTop = 1
      BorderLeft = 2
      BorderRight = 2
      BorderBottom = 2
      Color = 8404992
      BorderColor = clBlack
      Align = alTop
      TabOrder = 1
      object ButtonCloseAllSockets: TS7ImageButton
        AlignWithMargins = True
        Left = 30
        Top = 2
        Width = 24
        Height = 24
        Hint = 
          'clear list. this action will terminate all established connectio' +
          'ns with remote server.'
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Align = alLeft
        Background = 8404992
        ImageList = FormMain.ImageSubSeven
        ImageIndex = 1
        OnClick = ButtonCloseAllSocketsClick
        Value = 0
        ExplicitLeft = 2
        ExplicitHeight = 494
      end
      object ButtonCloseSocket: TS7ImageButton
        AlignWithMargins = True
        Left = 2
        Top = 2
        Width = 24
        Height = 24
        Hint = 
          'delete selected row. this action will close also close associate' +
          'd connection with server.'
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Align = alLeft
        Enabled = False
        Background = 8404992
        ImageList = FormMain.ImageSubSeven
        ImageIndex = 6
        OnClick = ButtonCloseSocketClick
        Value = 0
        ExplicitLeft = -10
        ExplicitTop = 5
        ExplicitHeight = 28
      end
      object CheckBoxAutoScroll: TS7CheckBox
        AlignWithMargins = True
        Left = 416
        Top = 2
        Width = 78
        Height = 24
        Hint = 
          'decide whether or not you want to scrollbar to follow the most r' +
          'ecent item of the list.'
        Margins.Left = 0
        Margins.Top = 2
        Margins.Right = 4
        Margins.Bottom = 2
        Caption = 'auto scroll'
        Mode = cbmCheckBox
        Checked = False
        Color = 8404992
        HoverColor = clBlack
        ActiveColor = 8404992
        Align = alRight
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
      end
    end
  end
  object SubSevenForms: TS7Form
    Resizable = True
    ShowBorder = True
    Color = clGray
    Left = 304
    Top = 152
  end
  object PopupMenuAction: TS7PopupMenu
    OwnerDraw = True
    Left = 160
    Top = 107
    object closeselectedsocket1: TMenuItem
      Caption = 'close selected socket'
      OnClick = closeselectedsocket1Click
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object closeallsockets1: TMenuItem
      Caption = 'close all socket(s)'
      OnClick = closeallsockets1Click
    end
  end
end
