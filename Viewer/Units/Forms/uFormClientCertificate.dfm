object FormClientCertificate: TFormClientCertificate
  Left = 0
  Top = 0
  BorderStyle = bsNone
  BorderWidth = 2
  Caption = 'certificate manager'
  ClientHeight = 280
  ClientWidth = 316
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
  Position = poMainFormCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 14
  object CaptionBar: TS7CaptionBar
    Left = 0
    Top = 0
    Width = 316
    Height = 19
    Caption = 'certificate manager'
    BorderIcons = [biSystemMenu, biMinimize]
    S7Form = SubSevenForms
    Dockable = False
    Transparent = False
    Collapsible = True
    TextCenter = False
    MainColor = 16744576
    SecondaryColor = clBlack
    Align = alTop
    ExplicitWidth = 306
  end
  object PanelClient: TS7Panel
    Left = 0
    Top = 19
    Width = 316
    Height = 261
    Hint = 
      'copy fingerprint to keyboard. this is useful when using pubkey a' +
      'uthentication for server.'
    BorderTop = 1
    BorderLeft = 2
    BorderRight = 2
    BorderBottom = 2
    Color = clBlack
    BorderColor = clBlack
    Align = alClient
    TabOrder = 0
    object GaugeMarquee: TS7Gauge
      Left = 0
      Top = 0
      Width = 316
      Height = 9
      Background = clBlack
      Border = clBlack
      Foreground = 8404992
      BorderWidth = 1
      Max = 100
      Progress = 50
      State = gsNormal
      Mode = gmProgressBar
      Text = ''
      TextMode = gtmNone
      Align = alTop
      Visible = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ExplicitLeft = 64
      ExplicitTop = 192
      ExplicitWidth = 190
    end
    object PanelButtons: TS7Panel
      Left = 64
      Top = 45
      Width = 169
      Height = 141
      BorderTop = 0
      BorderLeft = 0
      BorderRight = 0
      BorderBottom = 0
      Color = clBlack
      BorderColor = clBlack
      TabOrder = 0
      object ButtonCopyFingerprint: TS7Button
        AlignWithMargins = True
        Left = 0
        Top = 108
        Width = 169
        Height = 32
        Hint = 
          'copy fingerprint to keyboard. this is useful when using pubkey a' +
          'uthentication for server.'
        Margins.Left = 0
        Margins.Top = 0
        Margins.Right = 0
        Margins.Bottom = 4
        Align = alTop
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        Enabled = False
        TextAlign = taCenter
        Down = False
        Chevron = False
        Caption = 'Copy Certificate Fingerprint'
        Value = 0
        OnClick = ButtonCopyFingerprintClick
        Busy = False
        ExplicitLeft = 12
        ExplicitTop = 106
        ExplicitWidth = 157
      end
      object ButtonExport: TS7Button
        AlignWithMargins = True
        Left = 0
        Top = 72
        Width = 169
        Height = 32
        Hint = 'export both public and private key to file (pem format)'
        Margins.Left = 0
        Margins.Top = 0
        Margins.Right = 0
        Margins.Bottom = 4
        Align = alTop
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        Enabled = False
        TextAlign = taCenter
        Down = False
        Chevron = False
        Caption = 'export certificate to file'
        Value = 0
        OnClick = ButtonExportClick
        Busy = False
        ExplicitTop = 68
      end
      object ButtonGenerate: TS7Button
        AlignWithMargins = True
        Left = 0
        Top = 0
        Width = 169
        Height = 32
        Hint = 
          'generate a new subseven certificate (containing both public and ' +
          'private key)'
        Margins.Left = 0
        Margins.Top = 0
        Margins.Right = 0
        Margins.Bottom = 4
        Align = alTop
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        Enabled = True
        TextAlign = taCenter
        Down = False
        Chevron = False
        Caption = 'generate new certificate'
        Value = 0
        OnClick = ButtonGenerateClick
        Busy = False
        ExplicitTop = -12
      end
      object ButtonImport: TS7Button
        AlignWithMargins = True
        Left = 0
        Top = 36
        Width = 169
        Height = 32
        Hint = 'import and existing and compatible certificate to project.'
        Margins.Left = 0
        Margins.Top = 0
        Margins.Right = 0
        Margins.Bottom = 4
        Align = alTop
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        Enabled = True
        TextAlign = taCenter
        Down = False
        Chevron = False
        Caption = 'import existing certificate'
        Value = 0
        OnClick = ButtonImportClick
        Busy = False
        ExplicitTop = 56
      end
    end
  end
  object SubSevenForms: TS7Form
    Resizable = False
    ShowBorder = True
    Color = clGray
    Left = 368
    Top = 152
  end
  object OpenDialog: TOpenDialog
    Filter = 'OpenSSL PEM Files|*.pem'
    Left = 24
    Top = 219
  end
  object SaveDialog: TSaveDialog
    DefaultExt = 'pem'
    Filter = 'OpenSSL PEM File|*.pem'
    Left = 184
    Top = 219
  end
end
