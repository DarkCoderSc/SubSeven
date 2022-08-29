inherited FrameComboUser: TFrameComboUser
  Height = 76
  ExplicitHeight = 76
  object ComboUser: TS7ComboBox
    Left = 0
    Top = 0
    Width = 320
    Height = 21
    Hint = 'existing windows user list.'
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
