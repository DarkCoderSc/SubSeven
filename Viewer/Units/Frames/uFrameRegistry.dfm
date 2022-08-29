inherited FrameRegistry: TFrameRegistry
  Width = 583
  Height = 401
  ExplicitWidth = 583
  ExplicitHeight = 401
  inherited DockCaption: TS7DockCaption
    Width = 579
    Caption = 'registry editor'
    ExplicitWidth = 579
  end
  object MultiPanel: TOMultiPanel
    AlignWithMargins = True
    Left = 3
    Top = 29
    Width = 577
    Height = 369
    PanelCollection = <
      item
        Control = VSTTree
        Position = 0.300000000000000000
        Visible = True
        Index = 0
      end
      item
        Control = VST
        Position = 1.000000000000000000
        Visible = True
        Index = 1
      end>
    MinPosition = 0.020000000000000000
    Align = alClient
    TabOrder = 0
    ExplicitLeft = 240
    ExplicitTop = 192
    ExplicitWidth = 185
    ExplicitHeight = 41
    object VST: TVirtualStringTree
      AlignWithMargins = True
      Left = 176
      Top = 0
      Width = 401
      Height = 369
      Margins.Left = 2
      Margins.Top = 0
      Margins.Right = 2
      Margins.Bottom = 2
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
      Images = FrmMain.ImgSystem
      ParentFont = False
      TabOrder = 1
      TreeOptions.MiscOptions = [toAcceptOLEDrop, toEditable, toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning, toEditOnClick]
      TreeOptions.PaintOptions = [toHideFocusRect, toShowButtons, toShowDropmark, toUseBlendedImages, toUseBlendedSelection]
      TreeOptions.SelectionOptions = [toExtendedFocus, toFullRowSelect, toMultiSelect, toRightClickSelect]
      Columns = <
        item
          Position = 0
          Text = 'Name'
          Width = 120
        end
        item
          Position = 1
          Text = 'Type'
          Width = 100
        end
        item
          Position = 2
          Text = 'Data'
          Width = 181
        end>
    end
    object VSTTree: TVirtualStringTree
      AlignWithMargins = True
      Left = 0
      Top = 0
      Width = 173
      Height = 369
      Margins.Left = 2
      Margins.Top = 0
      Margins.Right = 2
      Margins.Bottom = 2
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
      Header.Options = [hoColumnResize, hoDrag, hoShowSortGlyphs, hoHeaderClickAutoSort]
      Header.ParentFont = False
      Header.SortColumn = 0
      Images = FrmMain.ImgSystem
      ParentFont = False
      TabOrder = 0
      TreeOptions.MiscOptions = [toAcceptOLEDrop, toEditable, toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning, toEditOnClick]
      TreeOptions.PaintOptions = [toHideFocusRect, toShowButtons, toShowDropmark, toUseBlendedImages, toUseBlendedSelection]
      TreeOptions.SelectionOptions = [toExtendedFocus, toFullRowSelect, toMultiSelect, toRightClickSelect]
      Columns = <
        item
          Position = 0
          Text = 'Key'
          Width = 150
        end>
    end
  end
end
