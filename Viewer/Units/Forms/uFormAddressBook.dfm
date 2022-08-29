object FormAddressBook: TFormAddressBook
  Left = 0
  Top = 0
  BorderStyle = bsNone
  BorderWidth = 2
  Caption = 'subseven address book (contacts)'
  ClientHeight = 236
  ClientWidth = 352
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
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 14
  object CaptionBar: TS7CaptionBar
    Left = 0
    Top = 0
    Width = 352
    Height = 19
    Caption = 'subseven address book (contacts)'
    BorderIcons = [biSystemMenu, biMinimize, biMaximize]
    S7Form = SubSevenForms
    Dockable = False
    Transparent = False
    Collapsible = True
    TextCenter = False
    MainColor = 16744576
    SecondaryColor = clBlack
    Align = alTop
    ExplicitTop = -3
  end
  object PanelClient: TS7Panel
    Left = 0
    Top = 19
    Width = 352
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
      Width = 348
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
      Header.Options = [hoColumnResize, hoDrag, hoShowSortGlyphs, hoVisible, hoHeaderClickAutoSort]
      Header.ParentFont = False
      Header.SortColumn = 0
      IncrementalSearch = isAll
      ParentCtl3D = False
      ParentFont = False
      PopupMenu = PopupMenuAction
      TabOrder = 0
      TreeOptions.PaintOptions = [toHideFocusRect, toShowButtons, toShowDropmark, toUseBlendedImages, toUseBlendedSelection]
      TreeOptions.SelectionOptions = [toFullRowSelect, toRightClickSelect]
      OnChange = VSTChange
      OnCompareNodes = VSTCompareNodes
      OnDblClick = VSTDblClick
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
          Text = 'display name'
          Width = 140
        end
        item
          Position = 1
          Text = 'password saved'
          Width = 110
        end
        item
          Position = 2
          Spacing = 2
          Text = 'remote address'
          Width = 120
        end
        item
          Position = 3
          Text = 'remote port'
          Width = 120
        end
        item
          Position = 4
          Text = 'description'
          Width = 200
        end>
    end
    object PanelHeader: TS7Panel
      Left = 0
      Top = 0
      Width = 352
      Height = 28
      BorderTop = 1
      BorderLeft = 2
      BorderRight = 2
      BorderBottom = 2
      Color = 8404992
      BorderColor = clBlack
      Align = alTop
      TabOrder = 1
      object ButtonDelete: TS7ImageButton
        AlignWithMargins = True
        Left = 30
        Top = 2
        Width = 24
        Height = 24
        Hint = 'delete contact from address book list.'
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
        ExplicitLeft = 2
        ExplicitHeight = 494
      end
      object ButtonAdd: TS7ImageButton
        AlignWithMargins = True
        Left = 2
        Top = 2
        Width = 24
        Height = 24
        Hint = 'add new contact to address book.'
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Align = alLeft
        Background = 8404992
        ImageList = FormMain.ImageSubSeven
        ImageIndex = 90
        OnClick = ButtonAddClick
        Value = 0
        ExplicitLeft = -10
        ExplicitTop = 5
        ExplicitHeight = 28
      end
      object ButtonEdit: TS7ImageButton
        AlignWithMargins = True
        Left = 86
        Top = 2
        Width = 24
        Height = 24
        Hint = 'edit selected contact'
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Align = alLeft
        Enabled = False
        Background = 8404992
        ImageList = FormMain.ImageSubSeven
        ImageIndex = 14
        OnClick = ButtonEditClick
        Value = 0
        ExplicitTop = 5
      end
      object ButtonConnect: TS7ImageButton
        AlignWithMargins = True
        Left = 58
        Top = 2
        Width = 24
        Height = 24
        Hint = 'edit selected contact'
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Align = alLeft
        Enabled = False
        Background = 8404992
        ImageList = FormMain.ImageSubSeven
        ImageIndex = 28
        OnClick = ButtonConnectClick
        Value = 0
        ExplicitLeft = 86
        ExplicitTop = 5
      end
    end
  end
  object SubSevenForms: TS7Form
    Resizable = True
    ShowBorder = True
    Color = clGray
    Left = 256
    Top = 125
  end
  object PopupMenuAction: TS7PopupMenu
    OwnerDraw = True
    Left = 136
    Top = 91
    object addnewcontacttolist1: TMenuItem
      Caption = 'add new contact to list'
      OnClick = addnewcontacttolist1Click
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object deleteselectedcontact1: TMenuItem
      Caption = 'delete selected contact'
      OnClick = deleteselectedcontact1Click
    end
    object connecttoselectedcontact1: TMenuItem
      Caption = 'connect to selected contact'
      OnClick = connecttoselectedcontact1Click
    end
    object editselectedcontact1: TMenuItem
      Caption = 'edit selected contact'
      OnClick = editselectedcontact1Click
    end
  end
end
