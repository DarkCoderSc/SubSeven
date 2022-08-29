object FormQueue: TFormQueue
  Left = 0
  Top = 0
  BorderStyle = bsNone
  BorderWidth = 2
  Caption = 'queue'
  ClientHeight = 236
  ClientWidth = 456
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
  OnHide = FormHide
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 14
  object CaptionBar: TS7CaptionBar
    Left = 0
    Top = 0
    Width = 456
    Height = 19
    Caption = 'queue'
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
    Width = 456
    Height = 198
    BorderTop = 0
    BorderLeft = 2
    BorderRight = 2
    BorderBottom = 2
    Color = 8404992
    BorderColor = clBlack
    Align = alClient
    TabOrder = 0
    object PanelHeader: TS7Panel
      Left = 0
      Top = 0
      Width = 456
      Height = 28
      BorderTop = 1
      BorderLeft = 2
      BorderRight = 2
      BorderBottom = 2
      Color = 8404992
      BorderColor = clBlack
      Align = alTop
      TabOrder = 0
      object ButtonStartStopTransfer: TS7ImageButton
        AlignWithMargins = True
        Left = 2
        Top = 2
        Width = 24
        Height = 24
        Hint = 'start transfer'
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Align = alLeft
        Enabled = False
        Background = 8404992
        ImageList = FormMain.ImageSubSeven
        ImageIndex = 13
        OnClick = ButtonStartStopTransferClick
        Value = 0
        OnValueChanged = ButtonStartStopTransferValueChanged
        ExplicitLeft = -10
        ExplicitTop = 5
        ExplicitHeight = 28
      end
      object ButtonDelete: TS7ImageButton
        AlignWithMargins = True
        Left = 30
        Top = 2
        Width = 24
        Height = 24
        Hint = 'delete selected transfers'
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
        ExplicitLeft = 102
      end
      object ButtonRetryTransfer: TS7ImageButton
        AlignWithMargins = True
        Left = 58
        Top = 2
        Width = 24
        Height = 24
        Hint = 'retry failed transfer'
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Align = alLeft
        Enabled = False
        Background = 8404992
        ImageList = FormMain.ImageSubSeven
        ImageIndex = 0
        OnClick = ButtonRetryTransferClick
        Value = 0
        ExplicitLeft = 170
        ExplicitTop = 5
      end
      object ButtonOpenTransferWindow: TS7ImageButton
        AlignWithMargins = True
        Left = 86
        Top = 2
        Width = 24
        Height = 24
        Hint = 'open transfer window'
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Align = alLeft
        Enabled = False
        Background = 8404992
        ImageList = FormMain.ImageSubSeven
        ImageIndex = 18
        OnClick = ButtonOpenTransferWindowClick
        Value = 0
        ExplicitLeft = 134
        ExplicitTop = 5
      end
      object ButtonOptions: TS7ImageButton
        AlignWithMargins = True
        Left = 114
        Top = 2
        Width = 24
        Height = 24
        Hint = 'show options'
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Align = alLeft
        Background = 8404992
        ImageList = FormMain.ImageSubSeven
        ImageIndex = 30
        OnClick = ButtonOptionsClick
        Value = 0
        ExplicitLeft = 190
      end
    end
    object VST: TVirtualStringTree
      AlignWithMargins = True
      Left = 2
      Top = 28
      Width = 452
      Height = 168
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
      Header.Options = [hoColumnResize, hoDrag, hoShowSortGlyphs, hoVisible]
      Header.ParentFont = False
      IncrementalSearch = isAll
      ParentFont = False
      PopupMenu = PopupMenuAction
      TabOrder = 1
      TreeOptions.PaintOptions = [toHideFocusRect, toShowButtons, toShowDropmark, toUseBlendedImages, toUseBlendedSelection]
      TreeOptions.SelectionOptions = [toExtendedFocus, toFullRowSelect, toRightClickSelect]
      OnChange = VSTChange
      OnDblClick = VSTDblClick
      OnFocusChanged = VSTFocusChanged
      OnFreeNode = VSTFreeNode
      OnGetText = VSTGetText
      OnGetNodeDataSize = VSTGetNodeDataSize
      Touch.InteractiveGestures = [igPan, igPressAndTap]
      Touch.InteractiveGestureOptions = [igoPanSingleFingerHorizontal, igoPanSingleFingerVertical, igoPanInertia, igoPanGutter, igoParentPassthrough]
      Columns = <
        item
          Position = 0
          Text = 'name (origin)'
          Width = 130
        end
        item
          Position = 1
          Text = 'destination'
          Width = 130
        end
        item
          Position = 2
          Text = 'direction'
          Width = 60
        end
        item
          Position = 3
          Text = 'size'
          Width = 60
        end
        item
          Position = 4
          Text = 'progress'
          Width = 72
        end>
    end
  end
  object PanelFooter: TS7Panel
    Left = 0
    Top = 217
    Width = 456
    Height = 19
    Margins.Left = 0
    Margins.Top = 0
    Margins.Right = 0
    Margins.Bottom = 0
    BorderTop = 0
    BorderLeft = 2
    BorderRight = 2
    BorderBottom = 2
    Color = 8404992
    BorderColor = clBlack
    Align = alBottom
    TabOrder = 1
    object LabelGaugeSelected: TLabel
      AlignWithMargins = True
      Left = 6
      Top = 1
      Width = 62
      Height = 13
      Margins.Left = 6
      Margins.Top = 1
      Margins.Right = 0
      Margins.Bottom = 2
      Align = alLeft
      Caption = 'selected file: '
      Color = 8404992
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentColor = False
      ParentFont = False
    end
    object GaugeSelected: TGauge
      AlignWithMargins = True
      Left = 71
      Top = 2
      Width = 80
      Height = 13
      Margins.Top = 2
      Margins.Bottom = 4
      Align = alLeft
      BackColor = clBlack
      Color = 8404992
      ForeColor = 8404992
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentColor = False
      ParentFont = False
      Progress = 0
      ExplicitHeight = 11
    end
    object LabelGaugeTotal: TLabel
      AlignWithMargins = True
      Left = 344
      Top = 1
      Width = 23
      Height = 13
      Margins.Left = 6
      Margins.Top = 1
      Margins.Right = 0
      Margins.Bottom = 2
      Align = alRight
      Caption = 'total:'
      Color = 8404992
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentColor = False
      ParentFont = False
    end
    object GaugeTotal: TGauge
      AlignWithMargins = True
      Left = 370
      Top = 2
      Width = 80
      Height = 13
      Margins.Top = 2
      Margins.Right = 6
      Margins.Bottom = 4
      Align = alRight
      BackColor = clBlack
      Color = 8404992
      ForeColor = 8404992
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentColor = False
      ParentFont = False
      Progress = 0
      ExplicitLeft = 329
      ExplicitHeight = 11
    end
  end
  object SubSevenForms: TS7Form
    Resizable = True
    ShowBorder = True
    Color = clGray
    Left = 200
    Top = 104
  end
  object Timer: TTimer
    Enabled = False
    Interval = 500
    OnTimer = TimerTimer
    Left = 264
    Top = 107
  end
  object Options: TS7OptionDialog
    Options = <
      item
        Kind = okCheckBox
        Caption = 'overwrite remote file'
        Checked = True
        Hint = 
          'decide whether or not a transfer will overwrite an existing file' +
          '. if not checked and target file exists, it will be suffixed wit' +
          'h a first available number'
        Enabled = True
        Name = 'overwrite'
      end
      item
        Kind = okCheckBox
        Caption = 'auto-transfer'
        Checked = True
        Hint = 'start transfer automatically when added to queue'
        Enabled = True
        Name = 'autotransfer'
      end
      item
        Kind = okCheckBox
        Caption = 'open transfer window'
        Checked = True
        Hint = 'open file transfer window when a new transfer is added to queue'
        Enabled = True
        Name = 'opentranswindow'
      end>
    Caption = 'queue options'
    Width = 215
    Setting = FormMain.SettingHandler
    Left = 120
    Top = 107
  end
  object PopupMenuAction: TS7PopupMenu
    OwnerDraw = True
    Left = 48
    Top = 107
    object starttransfer1: TMenuItem
      Caption = 'start'
      OnClick = starttransfer1Click
    end
    object deletetransfer1: TMenuItem
      Caption = 'delete'
      OnClick = deletetransfer1Click
    end
    object retry1: TMenuItem
      Caption = 'retry'
      OnClick = retry1Click
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object opentransferwindow1: TMenuItem
      Caption = 'open transfer window'
      OnClick = opentransferwindow1Click
    end
  end
  object WindowsTaskbar: TTaskbar
    TaskBarButtons = <>
    TabProperties = []
    Left = 339
    Top = 117
  end
end
