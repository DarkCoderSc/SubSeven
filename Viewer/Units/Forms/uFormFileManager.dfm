object FormFileManager: TFormFileManager
  Left = 0
  Top = 0
  BorderStyle = bsNone
  BorderWidth = 2
  Caption = 'file manager'
  ClientHeight = 347
  ClientWidth = 405
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
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 14
  object CaptionBar: TS7CaptionBar
    Left = 0
    Top = 0
    Width = 405
    Height = 19
    Caption = 'file manager'
    BorderIcons = [biSystemMenu, biMinimize, biMaximize]
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
    Width = 405
    Height = 328
    BorderTop = 1
    BorderLeft = 2
    BorderRight = 2
    BorderBottom = 2
    Color = 8404992
    BorderColor = clBlack
    Align = alClient
    TabOrder = 0
    object PanelRight: TS7Panel
      Left = 371
      Top = 0
      Width = 34
      Height = 328
      BorderTop = 1
      BorderLeft = 2
      BorderRight = 2
      BorderBottom = 2
      Color = 8404992
      BorderColor = clBlack
      Align = alRight
      TabOrder = 0
      object ButtonRefreshCurrentDirectory: TS7ImageButton
        AlignWithMargins = True
        Left = 2
        Top = 2
        Width = 30
        Height = 24
        Hint = 'refresh current folder'
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 0
        Align = alTop
        Enabled = False
        Background = 8404992
        ImageList = FormMain.ImageSubSeven
        ImageIndex = 0
        OnClick = ButtonRefreshCurrentDirectoryClick
        Value = 0
      end
      object ButtonGotoRoot: TS7ImageButton
        AlignWithMargins = True
        Left = 2
        Top = 26
        Width = 30
        Height = 24
        Hint = 'go to root folder'
        Margins.Left = 2
        Margins.Top = 0
        Margins.Right = 2
        Margins.Bottom = 0
        Align = alTop
        Enabled = False
        Background = 8404992
        ImageList = FormMain.ImageSubSeven
        ImageIndex = 2
        OnClick = ButtonGotoRootClick
        Value = 0
        ExplicitTop = 30
      end
      object ButtonDownload: TS7ImageButton
        AlignWithMargins = True
        Left = 2
        Top = 50
        Width = 30
        Height = 24
        Hint = 'download file (add to queue)'
        HelpType = htKeyword
        HelpKeyword = 'download selected file(s)'
        Margins.Left = 2
        Margins.Top = 0
        Margins.Right = 2
        Margins.Bottom = 0
        Align = alTop
        Enabled = False
        Background = 8404992
        ImageList = FormMain.ImageSubSeven
        ImageIndex = 3
        OnClick = ButtonDownloadClick
        Value = 0
        ExplicitTop = 58
      end
      object ButtonUpload: TS7ImageButton
        AlignWithMargins = True
        Left = 2
        Top = 74
        Width = 30
        Height = 24
        Hint = 'upload file to current directory'
        HelpType = htKeyword
        HelpKeyword = 'upload file to current path'
        Margins.Left = 2
        Margins.Top = 0
        Margins.Right = 2
        Margins.Bottom = 0
        Align = alTop
        Enabled = False
        Background = 8404992
        ImageList = FormMain.ImageSubSeven
        ImageIndex = 4
        OnClick = ButtonUploadClick
        Value = 0
        ExplicitTop = 86
      end
      object ButtonCreateFolder: TS7ImageButton
        AlignWithMargins = True
        Left = 2
        Top = 98
        Width = 30
        Height = 24
        Hint = 'create directory'
        Margins.Left = 2
        Margins.Top = 0
        Margins.Right = 2
        Margins.Bottom = 0
        Align = alTop
        Enabled = False
        Background = 8404992
        ImageList = FormMain.ImageSubSeven
        ImageIndex = 7
        OnClick = ButtonCreateFolderClick
        Value = 0
        ExplicitTop = 114
      end
      object ButtonDeleteFile: TS7ImageButton
        AlignWithMargins = True
        Left = 2
        Top = 122
        Width = 30
        Height = 24
        Hint = 'delete selected file(s)'
        Margins.Left = 2
        Margins.Top = 0
        Margins.Right = 2
        Margins.Bottom = 0
        Align = alTop
        Enabled = False
        Background = 8404992
        ImageList = FormMain.ImageSubSeven
        ImageIndex = 6
        OnClick = ButtonDeleteFileClick
        Value = 0
        ExplicitTop = 142
      end
      object ButtonRenameFile: TS7ImageButton
        AlignWithMargins = True
        Left = 2
        Top = 146
        Width = 30
        Height = 24
        Hint = 'rename selected file / folder'
        Margins.Left = 2
        Margins.Top = 0
        Margins.Right = 2
        Margins.Bottom = 0
        Align = alTop
        Enabled = False
        Background = 8404992
        ImageList = FormMain.ImageSubSeven
        ImageIndex = 14
        OnClick = ButtonRenameFileClick
        Value = 0
        ExplicitTop = 170
      end
      object ButtonSearchForFile: TS7ImageButton
        AlignWithMargins = True
        Left = 2
        Top = 194
        Width = 30
        Height = 24
        Hint = 'search for files'
        Margins.Left = 2
        Margins.Top = 0
        Margins.Right = 2
        Margins.Bottom = 0
        Align = alTop
        Visible = False
        Background = 8404992
        ImageList = FormMain.ImageSubSeven
        ImageIndex = 58
        Value = 0
        ExplicitTop = 198
      end
      object ButtonLocalClipboard: TS7ImageButton
        AlignWithMargins = True
        Left = 2
        Top = 170
        Width = 30
        Height = 24
        Hint = 'rename selected file / folder'
        Margins.Left = 2
        Margins.Top = 0
        Margins.Right = 2
        Margins.Bottom = 0
        Align = alTop
        Enabled = False
        Background = 8404992
        ImageList = FormMain.ImageSubSeven
        ImageIndex = 91
        OnClick = ButtonLocalClipboardClick
        Value = 0
        ExplicitTop = 194
      end
    end
    object PanelBody: TS7Panel
      AlignWithMargins = True
      Left = 2
      Top = 2
      Width = 369
      Height = 324
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 0
      Margins.Bottom = 2
      BorderTop = 0
      BorderLeft = 0
      BorderRight = 0
      BorderBottom = 0
      Color = 8404992
      BorderColor = clBlack
      Align = alClient
      TabOrder = 1
      object PanelBodyHeader: TS7Panel
        Left = 0
        Top = 0
        Width = 369
        Height = 30
        Margins.Left = 0
        Margins.Top = 0
        Margins.Right = 0
        Margins.Bottom = 0
        BorderTop = 0
        BorderLeft = 0
        BorderRight = 0
        BorderBottom = 2
        Color = 8404992
        BorderColor = clBlack
        Align = alTop
        TabOrder = 0
        object LabelDrive: TLabel
          AlignWithMargins = True
          Left = 4
          Top = 6
          Width = 27
          Height = 14
          Margins.Left = 4
          Margins.Top = 6
          Align = alLeft
          Caption = 'drive:'
          Color = 8404992
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWhite
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = []
          ParentColor = False
          ParentFont = False
        end
        object ButtonRefreshDrives: TS7ImageButton
          AlignWithMargins = True
          Left = 318
          Top = 2
          Width = 49
          Height = 26
          Hint = 'refresh devices list'
          Margins.Left = 2
          Margins.Top = 2
          Margins.Right = 2
          Margins.Bottom = 2
          Align = alRight
          Background = 8404992
          ImageList = FormMain.ImageSubSeven
          ImageIndex = 0
          OnClick = ButtonRefreshDrivesClick
          Value = 0
          ExplicitHeight = 28
        end
        object ComboDrive: TS7ComboBox
          Left = 34
          Top = 3
          Width = 279
          Height = 22
          ArrowColor = clGray
          ButtonBackgroundColor = clBlack
          BorderColor = clGray
          Enabled = True
          Status = csNormal
          Validators = []
          Style = csDropDownList
          Color = clBlack
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWhite
          Font.Height = -11
          Font.Name = 'Courier New'
          Font.Style = []
          ParentFont = False
          ParentShowHint = False
          ShowHint = True
          TabOrder = 0
          ItemIndex = -1
          OnChange = ComboDriveChange
        end
      end
      object PanelPath: TS7Panel
        Left = 0
        Top = 305
        Width = 369
        Height = 19
        BorderTop = 2
        BorderLeft = 0
        BorderRight = 0
        BorderBottom = 0
        Color = 8404992
        BorderColor = clBlack
        Align = alBottom
        TabOrder = 1
        object EditPath: TS7Edit
          AlignWithMargins = True
          Left = 0
          Top = 2
          Width = 369
          Height = 17
          Margins.Left = 0
          Margins.Top = 2
          Margins.Right = 0
          Margins.Bottom = 0
          Align = alClient
          AutoSize = False
          Color = 8404992
          Enabled = True
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWhite
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
          ParentShowHint = False
          ReadOnly = True
          ShowHint = True
          TabOrder = 0
          OnChange = EditPathChange
          AlternativeTheme = True
          Status = csNormal
          Validators = []
        end
      end
      object VST: TVirtualStringTree
        AlignWithMargins = True
        Left = 2
        Top = 30
        Width = 365
        Height = 273
        Margins.Left = 2
        Margins.Top = 0
        Margins.Right = 2
        Margins.Bottom = 2
        Align = alClient
        BorderStyle = bsNone
        Color = 8404992
        Colors.DropMarkColor = clBlack
        Colors.DropTargetColor = clBlack
        Colors.DropTargetBorderColor = clBlack
        Colors.FocusedSelectionColor = clBlack
        Colors.FocusedSelectionBorderColor = clBlack
        Colors.GridLineColor = 5581568
        Colors.SelectionRectangleBlendColor = clBlack
        Colors.SelectionRectangleBorderColor = clBlack
        Colors.SelectionTextColor = clWhite
        Colors.UnfocusedColor = clWhite
        Colors.UnfocusedSelectionColor = 5581568
        Colors.UnfocusedSelectionBorderColor = 5581568
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        Header.AutoSizeIndex = 0
        Header.Font.Charset = DEFAULT_CHARSET
        Header.Font.Color = clBlack
        Header.Font.Height = -11
        Header.Font.Name = 'Arial'
        Header.Font.Style = []
        Header.Options = [hoAutoResize, hoColumnResize, hoDrag, hoShowSortGlyphs, hoVisible, hoHeaderClickAutoSort]
        Header.ParentFont = False
        Header.SortColumn = 0
        Images = FormMain.ImageSystem
        IncrementalSearch = isAll
        ParentFont = False
        PopupMenu = PopupMenuAction
        TabOrder = 2
        TreeOptions.MiscOptions = [toAcceptOLEDrop, toEditable, toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning, toEditOnClick]
        TreeOptions.PaintOptions = [toHideFocusRect, toShowButtons, toShowDropmark, toUseBlendedImages, toUseBlendedSelection]
        TreeOptions.SelectionOptions = [toExtendedFocus, toFullRowSelect, toMultiSelect, toRightClickSelect]
        OnAddToSelection = VSTAddToSelection
        OnCompareNodes = VSTCompareNodes
        OnFocusChanged = VSTFocusChanged
        OnFreeNode = VSTFreeNode
        OnGetText = VSTGetText
        OnGetImageIndex = VSTGetImageIndex
        OnGetNodeDataSize = VSTGetNodeDataSize
        OnNewText = VSTNewText
        OnNodeDblClick = VSTNodeDblClick
        OnRemoveFromSelection = VSTRemoveFromSelection
        Touch.InteractiveGestures = [igPan, igPressAndTap]
        Touch.InteractiveGestureOptions = [igoPanSingleFingerHorizontal, igoPanSingleFingerVertical, igoPanInertia, igoPanGutter, igoParentPassthrough]
        Columns = <
          item
            Position = 0
            Text = 'file name'
            Width = 285
          end
          item
            Position = 1
            Text = 'size'
            Width = 80
          end>
      end
    end
  end
  object SubSevenForms: TS7Form
    Resizable = True
    ShowBorder = True
    Color = clGray
    Left = 200
    Top = 168
  end
  object OpenDialog: TOpenDialog
    Options = [ofHideReadOnly, ofAllowMultiSelect, ofFileMustExist, ofEnableSizing]
    Left = 122
    Top = 165
  end
  object SaveDialog: TSaveDialog
    Left = 146
    Top = 245
  end
  object PopupMenuAction: TS7PopupMenu
    OwnerDraw = True
    OnPopup = PopupMenuActionPopup
    Left = 58
    Top = 125
    object refreshdrives1: TMenuItem
      Caption = 'refresh drives'
      OnClick = refreshdrives1Click
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object refreshcurrentfolder1: TMenuItem
      Caption = 'refresh current folder'
      OnClick = refreshcurrentfolder1Click
    end
    object gotorootfolder1: TMenuItem
      Caption = 'go to root path'
      Enabled = False
      OnClick = gotorootfolder1Click
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object downloadselectedfiles1: TMenuItem
      Caption = 'download selected file(s)'
      OnClick = downloadselectedfiles1Click
    end
    object uploadfilestocurrentfolder1: TMenuItem
      Caption = 'upload file(s) to current folder'
      OnClick = uploadfilestocurrentfolder1Click
    end
    object N3: TMenuItem
      Caption = '-'
    end
    object createdirectory1: TMenuItem
      Caption = 'create new folder'
      OnClick = createdirectory1Click
    end
    object N4: TMenuItem
      Caption = '-'
    end
    object deleteselectedfilesfolders1: TMenuItem
      Caption = 'delete selected file(s) / folder(s)'
      OnClick = deleteselectedfilesfolders1Click
    end
    object renameselectedfilefolder1: TMenuItem
      Caption = 'rename selected file / folder'
      OnClick = renameselectedfilefolder1Click
    end
    object N6: TMenuItem
      Caption = '-'
    end
    object Clipboard1: TMenuItem
      Caption = 'Clipboard'
    end
  end
  object PopupMenuClipboard: TS7PopupMenu
    OwnerDraw = True
    OnPopup = PopupMenuClipboardPopup
    Left = 242
    Top = 237
    object Copy1: TMenuItem
      Caption = 'Copy'
      OnClick = Copy1Click
    end
    object Cut1: TMenuItem
      Caption = 'Cut'
      OnClick = Cut1Click
    end
    object N5: TMenuItem
      Caption = '-'
    end
    object Paste1: TMenuItem
      Caption = 'Paste to selected folder'
      Enabled = False
      OnClick = Paste1Click
    end
    object Pastetocurrentfolder1: TMenuItem
      Caption = 'Paste to current folder'
      Enabled = False
      OnClick = Pastetocurrentfolder1Click
    end
  end
end
