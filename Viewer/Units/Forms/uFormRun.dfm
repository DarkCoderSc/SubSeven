object FormRun: TFormRun
  Left = 0
  Top = 0
  BorderStyle = bsNone
  BorderWidth = 2
  Caption = 'run'
  ClientHeight = 438
  ClientWidth = 313
  Color = clGray
  Constraints.MinHeight = 80
  Constraints.MinWidth = 200
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Arial'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  OnKeyUp = FormKeyUp
  PixelsPerInch = 96
  TextHeight = 14
  object CaptionBar: TS7CaptionBar
    Left = 0
    Top = 0
    Width = 313
    Height = 19
    Caption = 'run'
    BorderIcons = [biSystemMenu, biMinimize]
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
    Width = 313
    Height = 378
    BorderTop = 0
    BorderLeft = 0
    BorderRight = 0
    BorderBottom = 0
    Color = clBlack
    BorderColor = clBlack
    Align = alClient
    TabOrder = 0
  end
  object PanelFooter: TS7Panel
    Left = 0
    Top = 397
    Width = 313
    Height = 41
    BorderTop = 0
    BorderLeft = 0
    BorderRight = 0
    BorderBottom = 0
    Color = clBlack
    BorderColor = clBlack
    Align = alBottom
    TabOrder = 1
    object ButtonCancel: TS7Button
      AlignWithMargins = True
      Left = 44
      Top = 7
      Width = 100
      Height = 28
      Hint = 'cancel and close window (all parameters will be lost)'
      Margins.Left = 0
      Margins.Top = 4
      Margins.Right = 0
      Margins.Bottom = 4
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      Enabled = True
      TextAlign = taCenter
      Down = False
      Chevron = False
      Caption = 'cancel'
      Value = 0
      OnClick = ButtonCancelClick
      Busy = False
    end
    object ButtonRun: TS7Button
      AlignWithMargins = True
      Left = 160
      Top = 7
      Width = 100
      Height = 28
      Hint = 'run program'
      Margins.Left = 0
      Margins.Top = 4
      Margins.Right = 0
      Margins.Bottom = 4
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      Enabled = True
      TextAlign = taCenter
      Down = False
      Chevron = False
      Caption = 'run'
      Value = 0
      OnClick = ButtonRunClick
      Busy = False
    end
  end
  object PanelForm: TS7Panel
    AlignWithMargins = True
    Left = 12
    Top = 31
    Width = 289
    Height = 354
    Margins.Left = 12
    Margins.Top = 12
    Margins.Right = 12
    Margins.Bottom = 12
    BorderTop = 0
    BorderLeft = 0
    BorderRight = 0
    BorderBottom = 0
    Color = clBlack
    BorderColor = clBlack
    Align = alClient
    TabOrder = 2
    ExplicitWidth = 292
    ExplicitHeight = 152
    object LabelProgram: TLabel
      AlignWithMargins = True
      Left = 0
      Top = 4
      Width = 289
      Height = 14
      Margins.Left = 0
      Margins.Top = 4
      Margins.Right = 0
      Margins.Bottom = 4
      Align = alTop
      Caption = 'program'
      Color = clBlack
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentColor = False
      ParentFont = False
      ExplicitWidth = 40
    end
    object LabelArguments: TLabel
      AlignWithMargins = True
      Left = 0
      Top = 53
      Width = 289
      Height = 14
      Margins.Left = 0
      Margins.Top = 4
      Margins.Right = 0
      Margins.Bottom = 4
      Align = alTop
      Caption = 'arguments'
      Color = clBlack
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentColor = False
      ParentFont = False
      ExplicitWidth = 51
    end
    object EditProgram: TS7Edit
      AlignWithMargins = True
      Left = 0
      Top = 22
      Width = 289
      Height = 22
      Hint = 'full path of the program to run'
      Margins.Left = 0
      Margins.Top = 0
      Margins.Right = 0
      Margins.Bottom = 5
      Align = alTop
      AutoSize = False
      Color = clBlack
      Enabled = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -13
      Font.Name = 'Courier New'
      Font.Style = [fsBold]
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      AlternativeTheme = False
      Status = csNormal
      Validators = [reqFilled]
    end
    object EditArguments: TS7Edit
      AlignWithMargins = True
      Left = 0
      Top = 71
      Width = 289
      Height = 22
      Hint = 'optionnal program arguments'
      Margins.Left = 0
      Margins.Top = 0
      Margins.Right = 0
      Margins.Bottom = 5
      Align = alTop
      AutoSize = False
      Color = clBlack
      Enabled = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -13
      Font.Name = 'Courier New'
      Font.Style = [fsBold]
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      AlternativeTheme = False
      Status = csNormal
      Validators = []
    end
    inline FrameRunAsGroup1: TFrameRunAsGroup
      Left = 0
      Top = 98
      Width = 289
      Height = 253
      Align = alTop
      Color = clBlack
      ParentBackground = False
      ParentColor = False
      TabOrder = 2
      ExplicitTop = 98
      ExplicitWidth = 292
      ExplicitHeight = 253
      inherited GroupRunAs: TS7GroupBox
        Width = 289
        ExplicitWidth = 292
        inherited RadioSession: TS7CheckBox
          Width = 277
          ExplicitWidth = 277
        end
        inherited RadioWinUser: TS7CheckBox
          Width = 277
          ExplicitWidth = 277
        end
        inherited GroupCredential: TS7GroupBox
          Width = 273
          ExplicitWidth = 276
          inherited Label3: TLabel
            Width = 257
          end
          inherited Label4: TLabel
            Width = 257
          end
          inherited EditPassword: TS7Edit
            Width = 257
            ExplicitWidth = 257
          end
          inherited FrameComboUser1: TFrameComboUser
            Width = 257
            Enabled = False
            ExplicitWidth = 260
            inherited ComboUser: TS7ComboBox
              Width = 257
              ExplicitWidth = 257
            end
          end
        end
        inherited RadioNTSYS: TS7CheckBox
          Width = 277
          ExplicitWidth = 277
        end
        inherited FrameComboSessions1: TFrameComboSessions
          Width = 283
          Enabled = False
          ExplicitWidth = 286
          inherited ComboSessions: TS7ComboBox
            Width = 275
            ExplicitWidth = 275
          end
        end
      end
    end
  end
  object SubSevenForms: TS7Form
    Resizable = False
    ShowBorder = True
    Color = clGray
    Left = 232
    Top = 32
  end
end
