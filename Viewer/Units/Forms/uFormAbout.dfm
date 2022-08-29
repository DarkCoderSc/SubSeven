object FormAbout: TFormAbout
  Left = 0
  Top = 0
  BorderStyle = bsNone
  Caption = 'about subseven legacy - www.sub7crew.org'
  ClientHeight = 400
  ClientWidth = 600
  Color = clBlack
  Constraints.MinHeight = 80
  Constraints.MinWidth = 200
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Arial'
  Font.Style = []
  Position = poOwnerFormCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnResize = FormResize
  OnShow = FormShow
  TextHeight = 14
  object PanelScene: TPanel
    Left = 0
    Top = 0
    Width = 600
    Height = 400
    Align = alClient
    BevelOuter = bvNone
    Color = clBlack
    ParentBackground = False
    TabOrder = 0
    object ScrollingCredits: TS7ScrollingCredit
      AlignWithMargins = True
      Left = 154
      Top = -155
      Width = 450
      Height = 220
      Margins.Left = 128
      Margins.Right = 128
      Margins.Bottom = 32
      Active = False
      Text.Strings = (
        '#SubSeven Legacy'
        ''
        '%version%'
        'Compiled with Delphi 11'
        ''
        ''
        'Made with love from'
        'RA 3h 17m 46s Dec -62'#176' 34'#8242' 31'#8243
        ''
        ''
        'Coded By DarkCoderSc'
        'Music  Maktone'
        ''
        ''
        ''
        ''
        '_https://www.twitter.com/darkcodersc'
        '_https://github.com/darkcodersc'
        ''
        ''
        ''
        '_www.sub7crew.org'
        '')
      LogoBegin.Data = {
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
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Courier'
      Font.Style = []
    end
    object Scene: TImage
      Left = 0
      Top = 0
      Width = 600
      Height = 400
      Align = alClient
      OnMouseDown = SceneMouseDown
      ExplicitLeft = 56
      ExplicitTop = 80
      ExplicitWidth = 105
      ExplicitHeight = 105
    end
    object CaptionBar: TS7CaptionBar
      Left = 15
      Top = 8
      Width = 500
      Height = 25
      Caption = 'about subseven legacy - www.sub7crew.org'
      BorderIcons = [biSystemMenu]
      Dockable = False
      Transparent = True
      Collapsible = False
      TextCenter = True
      MainColor = clBlack
      SecondaryColor = clGray
    end
  end
  object TimerScene: TS7Timer
    DueTime = 80
    Period = 80
    OnTimer = TimerSceneTimer
    Left = 248
    Top = 211
  end
end
