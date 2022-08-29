object FormServerHive: TFormServerHive
  Left = 0
  Top = 0
  BorderStyle = bsNone
  BorderWidth = 2
  Caption = 'known servers'
  ClientHeight = 236
  ClientWidth = 353
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
  Position = poMainFormCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 14
  object CaptionBar: TS7CaptionBar
    Left = 0
    Top = 0
    Width = 353
    Height = 19
    Caption = 'known servers'
    BorderIcons = [biSystemMenu, biMinimize, biMaximize]
    S7Form = SubSevenForms
    Dockable = False
    Transparent = False
    Collapsible = True
    TextCenter = False
    MainColor = 16744576
    SecondaryColor = clBlack
    Align = alTop
    ExplicitWidth = 515
  end
  object PanelClient: TS7Panel
    Left = 0
    Top = 19
    Width = 353
    Height = 217
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
      Width = 349
      Height = 187
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
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      Header.AutoSizeIndex = 2
      Header.Font.Charset = DEFAULT_CHARSET
      Header.Font.Color = clBlack
      Header.Font.Height = -11
      Header.Font.Name = 'Arial'
      Header.Font.Style = []
      Header.Options = [hoAutoResize, hoColumnResize, hoDrag, hoShowSortGlyphs, hoVisible, hoHeaderClickAutoSort]
      Header.ParentFont = False
      Header.SortColumn = 0
      IncrementalSearch = isAll
      ParentFont = False
      PopupMenu = PopupMenuAction
      TabOrder = 0
      TreeOptions.PaintOptions = [toHideFocusRect, toShowButtons, toShowDropmark, toUseBlendedImages, toUseBlendedSelection]
      TreeOptions.SelectionOptions = [toFullRowSelect, toRightClickSelect]
      OnChange = VSTChange
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
          Text = 'server name'
          Width = 100
        end
        item
          Position = 1
          Text = 'display name'
          Width = 100
        end
        item
          Position = 2
          Text = 'fingerprint'
          Width = 29
        end
        item
          Position = 3
          Text = 'trusted since'
          Width = 120
        end>
    end
    object PanelHeader: TS7Panel
      Left = 0
      Top = 0
      Width = 353
      Height = 28
      BorderTop = 1
      BorderLeft = 2
      BorderRight = 2
      BorderBottom = 2
      Color = 8404992
      BorderColor = clBlack
      Align = alTop
      TabOrder = 1
      object ButtonDeleteAll: TS7ImageButton
        AlignWithMargins = True
        Left = 30
        Top = 2
        Width = 24
        Height = 24
        Hint = 'delete all trusted server'
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Align = alLeft
        Enabled = False
        Background = 8404992
        ImageList = FormMain.ImageSubSeven
        ImageIndex = 1
        OnClick = ButtonDeleteAllClick
        Value = 0
        ExplicitLeft = 2
        ExplicitHeight = 494
      end
      object ButtonDelete: TS7ImageButton
        AlignWithMargins = True
        Left = 2
        Top = 2
        Width = 24
        Height = 24
        Hint = 'delect selected server trust'
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Align = alLeft
        Enabled = False
        Background = 8404992
        ImageList = FormMain.ImageSubSeven
        ImageIndex = 6
        OnClick = ButtonDeleteClick
        Value = 0
        ExplicitLeft = -10
        ExplicitTop = 5
        ExplicitHeight = 28
      end
      object ButtonRename: TS7ImageButton
        AlignWithMargins = True
        Left = 58
        Top = 2
        Width = 24
        Height = 24
        Hint = 'rename selected server display name'
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Align = alLeft
        Enabled = False
        Background = 8404992
        ImageList = FormMain.ImageSubSeven
        ImageIndex = 14
        OnClick = ButtonRenameClick
        Value = 0
        ExplicitLeft = 70
      end
    end
  end
  object SubSevenForms: TS7Form
    Resizable = True
    ShowBorder = True
    Color = clGray
    Left = 328
    Top = 160
  end
  object PopupMenuAction: TS7PopupMenu
    OwnerDraw = True
    Left = 192
    Top = 155
    object delectselectedservertrust1: TMenuItem
      Caption = 'delect selected server trust'
      OnClick = delectselectedservertrust1Click
    end
    object deletealltrustedservers1: TMenuItem
      Caption = 'delete all trusted servers'
      OnClick = deletealltrustedservers1Click
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object renameselectedserverdisplayname1: TMenuItem
      Caption = 'rename selected server display name'
      OnClick = renameselectedserverdisplayname1Click
    end
  end
end
