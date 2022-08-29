inherited FrameShell: TFrameShell
  Width = 464
  Height = 353
  ExplicitWidth = 464
  ExplicitHeight = 353
  inherited DockCaption: TS7DockCaption
    Width = 460
    Caption = 'remote shell sessions'
    ExplicitWidth = 476
  end
  object VST: TVirtualStringTree
    AlignWithMargins = True
    Left = 2
    Top = 26
    Width = 460
    Height = 284
    Margins.Left = 2
    Margins.Top = 0
    Margins.Right = 2
    Margins.Bottom = 2
    Align = alClient
    BorderStyle = bsNone
    Color = 8404992
    Colors.DropMarkColor = clBlack
    Colors.DropTargetColor = clBlack
    Colors.DropTargetBorderColor = clBlack
    Colors.FocusedSelectionColor = clBlack
    Colors.FocusedSelectionBorderColor = clBlack
    Colors.GridLineColor = 5581568
    Colors.SelectionRectangleBlendColor = clBlack
    Colors.SelectionRectangleBorderColor = clBlack
    Colors.SelectionTextColor = clWhite
    Colors.UnfocusedColor = clWhite
    Colors.UnfocusedSelectionColor = 5581568
    Colors.UnfocusedSelectionBorderColor = 5581568
    Enabled = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    Header.AutoSizeIndex = -1
    Header.Font.Charset = DEFAULT_CHARSET
    Header.Font.Color = clBlack
    Header.Font.Height = -11
    Header.Font.Name = 'Arial'
    Header.Font.Style = []
    Header.Options = [hoColumnResize, hoDrag, hoShowSortGlyphs, hoVisible, hoHeaderClickAutoSort]
    Header.ParentFont = False
    Header.SortColumn = 0
    IncrementalSearch = isAll
    ParentFont = False
    PopupMenu = PopupMenuAction
    TabOrder = 0
    TreeOptions.MiscOptions = [toAcceptOLEDrop, toEditable, toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning, toEditOnClick]
    TreeOptions.PaintOptions = [toHideFocusRect, toShowButtons, toShowDropmark, toUseBlendedImages, toUseBlendedSelection]
    TreeOptions.SelectionOptions = [toExtendedFocus, toFullRowSelect, toRightClickSelect]
    OnChange = VSTChange
    OnColumnDblClick = VSTColumnDblClick
    OnCompareNodes = VSTCompareNodes
    OnFocusChanged = VSTFocusChanged
    OnFreeNode = VSTFreeNode
    OnGetText = VSTGetText
    OnGetNodeDataSize = VSTGetNodeDataSize
    Touch.InteractiveGestures = [igPan, igPressAndTap]
    Touch.InteractiveGestureOptions = [igoPanSingleFingerHorizontal, igoPanSingleFingerVertical, igoPanInertia, igoPanGutter, igoParentPassthrough]
    ExplicitWidth = 476
    ExplicitHeight = 323
    Columns = <
      item
        Position = 0
        Text = 'Name'
        Width = 120
      end
      item
        Position = 1
        Text = 'status'
        Width = 100
      end
      item
        Position = 2
        Text = 'started'
        Width = 200
      end
      item
        Position = 3
        Text = 'id'
        Width = 100
      end>
  end
  object PanelFooter: TS7Panel
    Left = 0
    Top = 312
    Width = 464
    Height = 41
    BorderTop = 0
    BorderLeft = 0
    BorderRight = 0
    BorderBottom = 0
    Color = clBlack
    BorderColor = clBlack
    Align = alBottom
    TabOrder = 1
    ExplicitTop = 351
    ExplicitWidth = 480
    object ButtonReload: TS7Button
      AlignWithMargins = True
      Left = 55
      Top = 8
      Width = 100
      Height = 28
      Hint = 
        'reload remote shell service. this button is only active if remot' +
        'e shell service connection is lost.'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      Enabled = False
      TextAlign = taCenter
      Down = False
      Chevron = False
      Caption = 'reload service'
      Value = 0
      OnClick = ButtonReloadClick
      Busy = False
    end
    object ButtonNew: TS7Button
      AlignWithMargins = True
      Left = 161
      Top = 10
      Width = 100
      Height = 28
      Hint = 'create a new remote shell session'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      Enabled = False
      TextAlign = taCenter
      Down = False
      Chevron = False
      Caption = 'new session'
      Value = 0
      OnClick = ButtonNewClick
      Busy = False
    end
  end
  object PopupMenuAction: TS7PopupMenu
    OwnerDraw = True
    OnPopup = PopupMenuActionPopup
    Left = 248
    Top = 200
    object CloseSelectedSession1: TMenuItem
      Caption = 'Close Selected Session'
      OnClick = CloseSelectedSession1Click
    end
    object DestroySelectedSession1: TMenuItem
      Caption = 'Destroy Selected Session'
      OnClick = DestroySelectedSession1Click
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object RenameSession1: TMenuItem
      Caption = 'Rename Selected Session'
      OnClick = RenameSession1Click
    end
  end
end
