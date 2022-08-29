object FormEditConfiguration: TFormEditConfiguration
  Left = 0
  Top = 0
  Caption = 'Edit SubSeven Legacy Server Configuration'
  ClientHeight = 621
  ClientWidth = 756
  Color = clWhite
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object TopSeparator: TShape
    Left = 0
    Top = 0
    Width = 756
    Height = 2
    Align = alTop
    Pen.Color = 14867930
  end
  object Shape1: TShape
    Left = 0
    Top = 619
    Width = 756
    Height = 2
    Align = alBottom
    Pen.Color = 14867930
    ExplicitTop = 537
  end
  object MultiPanel: TS7CMultiPanel
    Left = 0
    Top = 2
    Width = 756
    Height = 617
    Orientation = poVertical
    Panels = <
      item
        Control = SynConfig
        Position = 0.700000000000000000
        Visible = True
        Index = 0
      end
      item
        Control = PanelMessages
        Position = 1.000000000000000000
        Visible = True
        Index = 1
      end>
    MinPosition = 0.020000000000000000
    Align = alClient
    TabOrder = 0
    object SynConfig: TSynEdit
      Left = 0
      Top = 0
      Width = 756
      Height = 432
      Color = clWhite
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -14
      Font.Name = 'Consolas'
      Font.Style = []
      TabOrder = 0
      CodeFolding.GutterShapeSize = 11
      CodeFolding.CollapsedLineColor = clGrayText
      CodeFolding.FolderBarLinesColor = clGrayText
      CodeFolding.IndentGuidesColor = clGray
      CodeFolding.IndentGuides = True
      CodeFolding.ShowCollapsedLine = False
      CodeFolding.ShowHintMark = True
      UseCodeFolding = False
      BorderStyle = bsNone
      Gutter.Color = 14867930
      Gutter.BorderColor = 14867930
      Gutter.Font.Charset = DEFAULT_CHARSET
      Gutter.Font.Color = 9735043
      Gutter.Font.Height = -14
      Gutter.Font.Name = 'Consolas'
      Gutter.Font.Style = []
      Gutter.LeftOffset = 12
      Gutter.RightOffset = 4
      Gutter.RightMargin = 4
      Gutter.ShowLineNumbers = True
      Gutter.Gradient = True
      Gutter.GradientStartColor = clWhite
      Gutter.GradientEndColor = clWhite
      Highlighter = SynJSONSyn1
      Options = [eoAutoIndent, eoDragDropEditing, eoEnhanceEndKey, eoGroupUndo, eoScrollPastEol, eoShowScrollHint, eoSmartTabDelete, eoTabsToSpaces]
      RightEdgeColor = 14867930
      WantTabs = True
      OnChange = SynConfigChange
      OnSpecialLineColors = SynConfigSpecialLineColors
      FontSmoothing = fsmClearType
    end
    object PanelMessages: TPanel
      Left = 0
      Top = 435
      Width = 756
      Height = 182
      BevelOuter = bvNone
      Color = 14867930
      ParentBackground = False
      TabOrder = 1
      object LabelMessages: TLabel
        AlignWithMargins = True
        Left = 8
        Top = 8
        Width = 745
        Height = 16
        Margins.Left = 8
        Margins.Top = 8
        Margins.Bottom = 4
        Align = alTop
        Caption = 'Messages'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = 2301723
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        ExplicitWidth = 64
      end
      object PanelRichEdit: TPanel
        AlignWithMargins = True
        Left = 8
        Top = 28
        Width = 740
        Height = 146
        Margins.Left = 8
        Margins.Top = 0
        Margins.Right = 8
        Margins.Bottom = 8
        Align = alClient
        BevelOuter = bvNone
        Color = 14867930
        ParentBackground = False
        TabOrder = 0
        object Shape2: TShape
          Left = 0
          Top = 0
          Width = 740
          Height = 146
          Align = alClient
          Pen.Color = 13221560
          ExplicitLeft = 48
          ExplicitTop = 32
          ExplicitWidth = 750
          ExplicitHeight = 148
        end
        object RichEditResult: TRichEdit
          AlignWithMargins = True
          Left = 4
          Top = 4
          Width = 732
          Height = 138
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Align = alClient
          BorderStyle = bsNone
          Font.Charset = ANSI_CHARSET
          Font.Color = 2301723
          Font.Height = -14
          Font.Name = 'Consolas'
          Font.Style = []
          ParentFont = False
          ReadOnly = True
          ScrollBars = ssBoth
          TabOrder = 0
          Zoom = 100
        end
      end
    end
  end
  object MainMenu: TMainMenu
    Left = 264
    Top = 408
    object File1: TMenuItem
      Caption = 'File'
      object Save1: TMenuItem
        Caption = 'Save'
        ShortCut = 16467
        OnClick = Save1Click
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object ResetConfig1: TMenuItem
        Caption = 'Reset Config'
        OnClick = ResetConfig1Click
      end
    end
    object ools1: TMenuItem
      Caption = 'Tools'
      object HashPassword1: TMenuItem
        Caption = 'Hash Password (Sha512)'
        ShortCut = 16455
        OnClick = HashPassword1Click
      end
    end
    object Help1: TMenuItem
      Caption = 'Help'
      ShortCut = 16467
      object Openonlinedocumentation1: TMenuItem
        Caption = 'Open online documentation'
        ShortCut = 16456
        OnClick = Openonlinedocumentation1Click
      end
    end
  end
  object SynJSONSyn1: TSynJSONSyn
    Options.AutoDetectEnabled = False
    Options.AutoDetectLineLimit = 0
    Options.Visible = False
    AttributeAttri.Foreground = 12076039
    NumberAttri.Foreground = 12076039
    SymbolAttri.Foreground = 2301723
    ValueAttri.Foreground = 5185798
    Left = 464
    Top = 464
  end
end
