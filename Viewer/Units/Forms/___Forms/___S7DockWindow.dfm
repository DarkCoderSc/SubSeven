object S7WindowDock: TS7WindowDock
  Left = 0
  Top = 0
  BorderStyle = bsNone
  BorderWidth = 2
  ClientHeight = 324
  ClientWidth = 360
  Color = clGray
  Constraints.MinHeight = 80
  Constraints.MinWidth = 200
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Arial'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 14
  object CaptionBar: TS7CaptionBar
    Left = 0
    Top = 0
    Width = 360
    Height = 19
    Caption = ''
    BorderIcons = [biSystemMenu, biMinimize, biMaximize]
    S7Form = S7Form1
    Dockable = True
    Transparent = False
    Collapsible = True
    TextCenter = False
    MainColor = 16744576
    SecondaryColor = clBlack
    OnDock = CaptionBarDock
    Align = alTop
  end
  object S7Form1: TS7Form
    Resizable = True
    ShowBorder = True
    Color = clGray
    Left = 304
    Top = 160
  end
end
