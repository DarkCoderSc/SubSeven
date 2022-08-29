object FormHashPassword: TFormHashPassword
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Hash Password (Sha512)'
  ClientHeight = 159
  ClientWidth = 343
  Color = clWhite
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object LabelTitle: TLabel
    AlignWithMargins = True
    Left = 16
    Top = 16
    Width = 311
    Height = 13
    Margins.Left = 16
    Margins.Top = 16
    Margins.Right = 16
    Margins.Bottom = 4
    Align = alTop
    Caption = 'Enter password to hash:'
    ExplicitWidth = 118
  end
  object LabelResult: TLabel
    AlignWithMargins = True
    Left = 16
    Top = 66
    Width = 311
    Height = 13
    Margins.Left = 16
    Margins.Top = 8
    Margins.Right = 16
    Margins.Bottom = 4
    Align = alTop
    Caption = 'Result:'
    ExplicitWidth = 34
  end
  object ButtonOk: TS7CButton
    Left = 227
    Top = 119
    Width = 100
    Height = 34
    Enabled = True
    Caption = 'Ok'
    Value = 0
    OnClick = ButtonOkClick
  end
  object ButtonCopy: TS7CButton
    Left = 121
    Top = 119
    Width = 100
    Height = 34
    Enabled = False
    Caption = 'Copy'
    Value = 0
    OnClick = ButtonCopyClick
  end
  object Shape: TShape
    AlignWithMargins = True
    Left = 3
    Top = 111
    Width = 337
    Height = 2
    Align = alTop
    Pen.Color = 16448250
    ExplicitLeft = 0
    ExplicitTop = 108
    ExplicitWidth = 343
  end
  object EditPassword: TEdit
    AlignWithMargins = True
    Left = 16
    Top = 33
    Width = 311
    Height = 21
    Margins.Left = 16
    Margins.Top = 0
    Margins.Right = 16
    Margins.Bottom = 4
    Align = alTop
    TabOrder = 0
    OnChange = EditPasswordChange
  end
  object EditResult: TEdit
    AlignWithMargins = True
    Left = 16
    Top = 83
    Width = 311
    Height = 21
    Margins.Left = 16
    Margins.Top = 0
    Margins.Right = 16
    Margins.Bottom = 4
    Align = alTop
    ReadOnly = True
    TabOrder = 1
    OnChange = EditResultChange
  end
end
