inherited FrameComboSessions: TFrameComboSessions
  Width = 286
  Height = 45
  ExplicitWidth = 286
  ExplicitHeight = 45
  object ComboSessions: TS7ComboBox
    Left = 0
    Top = 0
    Width = 286
    Height = 21
    Hint = 'active windows session list you may interact with'
    Margins.Left = 6
    Margins.Top = 0
    Margins.Right = 6
    Margins.Bottom = 8
    ArrowColor = clGray
    ButtonBackgroundColor = clBlack
    BorderColor = clGray
    Enabled = True
    Status = csNormal
    Validators = [reqFilled]
    Align = alTop
    Style = csDropDownList
    Color = clBlack
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    ParentShowHint = False
    ShowHint = True
    TabOrder = 0
    ItemIndex = -1
  end
end
