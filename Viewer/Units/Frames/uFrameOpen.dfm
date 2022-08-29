inherited FrameOpen: TFrameOpen
  Width = 485
  Height = 348
  ExplicitWidth = 485
  ExplicitHeight = 348
  inherited DockCaption: TS7DockCaption
    Width = 481
    Caption = 'open (ShellExecute)'
    ExplicitLeft = 2
    ExplicitTop = 0
    ExplicitWidth = 481
  end
  object PanelForm: TS7Panel
    Left = 80
    Top = 80
    Width = 289
    Height = 145
    BorderTop = 0
    BorderLeft = 0
    BorderRight = 0
    BorderBottom = 0
    Color = clBlack
    BorderColor = clBlack
    TabOrder = 0
    object LabelOpenCommand: TLabel
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
      Caption = 'open command'
      Color = clBlack
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentColor = False
      ParentFont = False
      ExplicitWidth = 73
    end
    object LabelSession: TLabel
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
      Caption = 'open in session'
      Color = clBlack
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentColor = False
      ParentFont = False
      ExplicitWidth = 76
    end
    object ButtonOpen: TS7Button
      AlignWithMargins = True
      Left = 96
      Top = 116
      Width = 100
      Height = 28
      Hint = 'run shellexecute api'
      Margins.Left = 0
      Margins.Top = 4
      Margins.Right = 0
      Margins.Bottom = 4
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      Enabled = False
      TextAlign = taCenter
      Down = False
      Chevron = False
      Caption = 'run'
      Value = 0
      OnClick = ButtonOpenClick
      Busy = False
    end
    object EditCommand: TS7Edit
      AlignWithMargins = True
      Left = 0
      Top = 22
      Width = 289
      Height = 22
      Hint = 
        'enter any commands that may run with shellexecute api "open" ver' +
        'b.'
      Margins.Left = 0
      Margins.Top = 0
      Margins.Right = 0
      Margins.Bottom = 5
      Align = alTop
      AutoSize = False
      Color = clBlack
      Enabled = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGray
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
    inline FrameComboSessions1: TFrameComboSessions
      Left = 0
      Top = 71
      Width = 289
      Height = 21
      Align = alTop
      AutoSize = True
      DoubleBuffered = True
      Enabled = False
      Color = clBlack
      ParentBackground = False
      ParentColor = False
      ParentDoubleBuffered = False
      TabOrder = 1
      ExplicitTop = 71
      ExplicitWidth = 289
      ExplicitHeight = 21
      inherited ComboSessions: TS7ComboBox
        Width = 289
        ExplicitWidth = 289
      end
    end
  end
end
