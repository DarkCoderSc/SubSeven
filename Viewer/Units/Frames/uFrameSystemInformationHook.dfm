inherited FrameSystemInformationHook: TFrameSystemInformationHook
  Width = 430
  Height = 355
  ExplicitWidth = 430
  ExplicitHeight = 355
  inherited DockCaption: TS7DockCaption
    Width = 426
    Caption = 'terminal sessions'
    ExplicitWidth = 426
  end
  object PanelFooter: TS7Panel
    Left = 0
    Top = 314
    Width = 430
    Height = 41
    BorderTop = 0
    BorderLeft = 0
    BorderRight = 0
    BorderBottom = 0
    Color = clBlack
    BorderColor = clBlack
    Align = alBottom
    TabOrder = 0
    object ButtonReload: TS7Button
      AlignWithMargins = True
      Left = 55
      Top = 8
      Width = 100
      Height = 28
      Hint = 
        'use this button in case you "terminal session hook" is lost with' +
        ' remote computer.'
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
    object ButtonPower: TS7Button
      AlignWithMargins = True
      Left = 161
      Top = 10
      Width = 100
      Height = 28
      Hint = 'power actions'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      Enabled = False
      TextAlign = taCenter
      Down = False
      Chevron = True
      Caption = 'power'
      Value = 0
      OnClick = ButtonPowerClick
      Busy = False
    end
  end
  object VST: TVirtualStringTree
    AlignWithMargins = True
    Left = 2
    Top = 26
    Width = 426
    Height = 286
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
    PopupMenu = PopupSession
    TabOrder = 1
    TreeOptions.MiscOptions = [toAcceptOLEDrop, toEditable, toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning, toEditOnClick]
    TreeOptions.PaintOptions = [toHideFocusRect, toShowButtons, toShowDropmark, toUseBlendedImages, toUseBlendedSelection]
    TreeOptions.SelectionOptions = [toExtendedFocus, toFullRowSelect, toRightClickSelect]
    OnChange = VSTChange
    OnCompareNodes = VSTCompareNodes
    OnFocusChanged = VSTFocusChanged
    OnFreeNode = VSTFreeNode
    OnGetText = VSTGetText
    OnGetNodeDataSize = VSTGetNodeDataSize
    Touch.InteractiveGestures = [igPan, igPressAndTap]
    Touch.InteractiveGestureOptions = [igoPanSingleFingerHorizontal, igoPanSingleFingerVertical, igoPanInertia, igoPanGutter, igoParentPassthrough]
    Columns = <
      item
        Position = 0
        Text = 'id'
        Width = 70
      end
      item
        Position = 1
        Text = 'active'
        Width = 60
      end
      item
        Position = 2
        Text = 'remote'
        Width = 60
      end
      item
        Position = 3
        Text = 'state'
        Width = 110
      end
      item
        Position = 4
        Text = 'username / domain'
        Width = 150
      end
      item
        Position = 5
        Text = 'client name'
        Width = 130
      end
      item
        Position = 6
        Text = 'station name'
        Width = 130
      end>
  end
  object PopupSession: TS7PopupMenu
    OwnerDraw = True
    OnPopup = PopupSessionPopup
    Left = 136
    Top = 168
    object Lock1: TMenuItem
      Caption = 'Lock'
      OnClick = Lock1Click
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object Logoff1: TMenuItem
      Caption = 'Logoff'
      OnClick = Logoff1Click
    end
  end
  object PopupPower: TS7PopupMenu
    OwnerDraw = True
    Left = 216
    Top = 168
    object shutdown1: TMenuItem
      Caption = 'shutdown'
      OnClick = shutdown1Click
    end
    object poweroff1: TMenuItem
      Caption = 'poweroff'
      OnClick = poweroff1Click
    end
    object reboot1: TMenuItem
      Caption = 'reboot'
      OnClick = reboot1Click
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object sleep1: TMenuItem
      Caption = 'sleep'
      OnClick = sleep1Click
    end
    object hibernate1: TMenuItem
      Caption = 'hibernate'
      OnClick = hibernate1Click
    end
  end
end
