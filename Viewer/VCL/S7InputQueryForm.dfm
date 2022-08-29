object Sub7FormInputQuery: TSub7FormInputQuery
  Left = 0
  Top = 0
  BorderStyle = bsNone
  BorderWidth = 2
  Caption = 'N/A'
  ClientHeight = 130
  ClientWidth = 294
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
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  OnKeyUp = FormKeyUp
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 14
  object CaptionBar: TS7CaptionBar
    Left = 0
    Top = 0
    Width = 294
    Height = 19
    Caption = 'N/A'
    BorderIcons = [biSystemMenu]
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
    Width = 294
    Height = 111
    BorderTop = 0
    BorderLeft = 0
    BorderRight = 0
    BorderBottom = 0
    Color = clBlack
    BorderColor = clBlack
    Align = alClient
    TabOrder = 0
    object ImageIcon: TImage
      Left = 3
      Top = 6
      Width = 32
      Height = 32
      AutoSize = True
      Picture.Data = {
        0954506E67496D61676589504E470D0A1A0A0000000D49484452000000200000
        00200806000000737A7AF4000000017352474200AECE1CE90000000467414D41
        0000B18F0BFC6105000000097048597300000EC300000EC301C76FA864000002
        504944415478DACDD72150C2501800E047231808068281605830100C04C38261
        C14030100C04C3026181B04058202C1016080B06828160201816080403C1B060
        20180806828160803B3C7CEF77E3B6BDFFB1B7E11DFE6577FB7FFE7DF7B6FDBC
        15C891A3F09F00DB635C3B06A041C83721EBCD9AAC562B423684AC3689233DCF
        9DA347613D9233DA8600B0D9C2C5532F2840C8D69BA689034269F1A428B58693
        C904EAD56B55AADEB66DA8373B7280B407749B00A4D6A703BE7E97AA542AC901
        C613A8D7344D0ED0B5A1DEB22C1CB05C2E41583A9503789E07F5DA8D1C002E4C
        EBADEE1E00BB0DE57299CB611D13002C623DD2019F4B58220CE0BFFAC2D70D3B
        A7EB3A0FE85890836701032C160B68503EE301D3E934D33CD05B3C005E3F9AB3
        7B22C0C7029A542A15A95B8085EBBA6240DB84FE8EE36406C8C6D6EDBBD0C330
        0CAE7F2A603E9F83BE729E1F00CD13E336CC018AE69CBE08F03E07A1A2280701
        D873101DB73B40CB80FE709B8400FA63E5E20040CFE1E6BD3460369BC1120500
        D988F5C0C66D98835793E6DC0711E06D96E92FB6D96CF20064DCEE00F73AE406
        83010EF07D3FD31E4008A0F5D1691703D0DCE0310550BDACE6BE05D8B80D7300
        A639312018B7B55A2D99930D74DCEE00774DC80D87431C108EDBDAD58100BACC
        D1711BE61A8D06F41F3EED01B01FABAA9A1B80CDFB2880F51F8D4602C0CB1496
        E82000326E7780DB06E484808C5B2C14808DDB3057AFD721377ADE03604B146C
        B1720503B01EC1B48B0503B01C6C645040B0C7FBB36F0241BD10106EB1B27E68
        64057A630120F7BAE70B0E7094383AE007B0EACE3F9720BF2F0000000049454E
        44AE426082}
    end
    object LabelMessage: TLabel
      Left = 49
      Top = 6
      Width = 20
      Height = 13
      AutoSize = False
      Caption = 'N/A'
      Color = clBlack
      EllipsisPosition = epEndEllipsis
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentColor = False
      ParentFont = False
    end
    object PanelFooter: TS7Panel
      Left = 0
      Top = 71
      Width = 294
      Height = 40
      BorderTop = 0
      BorderLeft = 0
      BorderRight = 0
      BorderBottom = 0
      Color = clBlack
      BorderColor = clBlack
      Align = alBottom
      TabOrder = 0
      object ButtonRight: TS7Button
        Left = 181
        Top = 3
        Width = 97
        Height = 28
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        Enabled = True
        TextAlign = taCenter
        Down = False
        Chevron = False
        Caption = 'R'
        Value = 0
        OnClick = ButtonRightClick
        Busy = False
      end
      object ButtonLeft: TS7Button
        Left = 78
        Top = 3
        Width = 97
        Height = 28
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        Enabled = True
        TextAlign = taCenter
        Down = False
        Chevron = False
        Caption = 'L'
        Value = 0
        OnClick = ButtonLeftClick
        Busy = False
      end
    end
    object EditResponse: TS7Edit
      Left = 49
      Top = 32
      Width = 229
      Height = 19
      AutoSize = False
      Color = clBlack
      Enabled = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      AlternativeTheme = False
      Status = csNormal
      Validators = []
    end
  end
  object SubSevenForms: TS7Form
    Resizable = True
    ShowBorder = True
    Color = clGray
    Left = 544
    Top = 224
  end
end
