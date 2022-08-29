object FormMain: TFormMain
  Left = 0
  Top = 0
  Caption = 'SubSeven Video Generator'
  ClientHeight = 345
  ClientWidth = 451
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object VST: TVirtualStringTree
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 445
    Height = 339
    AccessibleName = 'Filename'
    Align = alClient
    Header.AutoSizeIndex = 0
    Header.Options = [hoAutoResize, hoColumnResize, hoDrag, hoShowSortGlyphs]
    Header.SortColumn = 0
    TabOrder = 0
    TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowRoot, toShowTreeLines, toShowVertGridLines, toThemeAware, toUseBlendedImages, toFullVertGridLines]
    OnChange = VSTChange
    OnGetText = VSTGetText
    OnGetNodeDataSize = VSTGetNodeDataSize
    Touch.InteractiveGestures = [igPan, igPressAndTap]
    Touch.InteractiveGestureOptions = [igoPanSingleFingerHorizontal, igoPanSingleFingerVertical, igoPanInertia, igoPanGutter, igoParentPassthrough]
    Columns = <
      item
        Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coSmartResize, coAllowFocus, coEditable, coStyleColor]
        Position = 0
        Text = 'Filename'
        Width = 441
      end>
  end
  object OpenDialog: TOpenDialog
    Filter = 'PNG Image|*.png'
    Options = [ofHideReadOnly, ofAllowMultiSelect, ofEnableSizing]
    Left = 176
    Top = 176
  end
  object MainMenu: TMainMenu
    Left = 288
    Top = 144
    object File1: TMenuItem
      Caption = 'File'
      object OpenFiles1: TMenuItem
        Caption = 'Open Files'
        OnClick = OpenFiles1Click
      end
      object Generate1: TMenuItem
        Caption = 'Generate'
        OnClick = Generate1Click
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object Clear1: TMenuItem
        Caption = 'Clear'
        OnClick = Clear1Click
      end
    end
  end
end
