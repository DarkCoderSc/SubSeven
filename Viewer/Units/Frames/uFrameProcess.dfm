inherited FrameProcess: TFrameProcess
  Width = 407
  Height = 302
  ExplicitWidth = 407
  ExplicitHeight = 302
  inherited DockCaption: TS7DockCaption
    Width = 403
    Caption = 'process manager'
    ExplicitWidth = 403
  end
  object VST: TVirtualStringTree
    AlignWithMargins = True
    Left = 2
    Top = 26
    Width = 403
    Height = 233
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
    TabOrder = 0
    TreeOptions.MiscOptions = [toAcceptOLEDrop, toEditable, toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning, toEditOnClick]
    TreeOptions.PaintOptions = [toHideFocusRect, toShowButtons, toShowDropmark, toUseBlendedImages, toUseBlendedSelection]
    TreeOptions.SelectionOptions = [toExtendedFocus, toFullRowSelect, toMultiSelect, toRightClickSelect]
    OnAddToSelection = VSTAddToSelection
    OnChange = VSTChange
    OnCompareNodes = VSTCompareNodes
    OnFocusChanged = VSTFocusChanged
    OnFreeNode = VSTFreeNode
    OnGetText = VSTGetText
    OnGetNodeDataSize = VSTGetNodeDataSize
    OnRemoveFromSelection = VSTRemoveFromSelection
    Touch.InteractiveGestures = [igPan, igPressAndTap]
    Touch.InteractiveGestureOptions = [igoPanSingleFingerHorizontal, igoPanSingleFingerVertical, igoPanInertia, igoPanGutter, igoParentPassthrough]
    Columns = <
      item
        Position = 0
        Text = 'process name'
        Width = 150
      end
      item
        Position = 1
        Text = 'process id'
        Width = 80
      end
      item
        Position = 2
        Text = 'username / domain'
        Width = 130
      end
      item
        Position = 3
        Text = 'image path'
        Width = 250
      end>
  end
  object PanelFooter: TS7Panel
    Left = 0
    Top = 261
    Width = 407
    Height = 41
    BorderTop = 0
    BorderLeft = 0
    BorderRight = 0
    BorderBottom = 0
    Color = clBlack
    BorderColor = clBlack
    Align = alBottom
    TabOrder = 1
    object ButtonRefresh: TS7Button
      Left = 80
      Top = 3
      Width = 100
      Height = 28
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      Enabled = False
      TextAlign = taCenter
      Down = False
      Chevron = False
      Caption = 'refresh'
      Value = 0
      OnClick = ButtonRefreshClick
      Busy = False
    end
    object ButtonKillSelected: TS7Button
      Left = 186
      Top = 3
      Width = 100
      Height = 28
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      Enabled = False
      TextAlign = taCenter
      Down = False
      Chevron = False
      Caption = 'kill selected'
      Value = 0
      OnClick = ButtonKillSelectedClick
      Busy = False
    end
  end
end
