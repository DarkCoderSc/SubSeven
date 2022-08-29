{******************************************************************************}
{                                                                              }
{         ____             _     ____          _           ____                }
{        |  _ \  __ _ _ __| | __/ ___|___   __| | ___ _ __/ ___|  ___          }
{        | | | |/ _` | '__| |/ / |   / _ \ / _` |/ _ \ '__\___ \ / __|         }
{        | |_| | (_| | |  |   <| |__| (_) | (_| |  __/ |   ___) | (__          }
{        |____/ \__,_|_|  |_|\_\\____\___/ \__,_|\___|_|  |____/ \___|         }
{                                                                              }
{                           +++++++++++++++++++++                              }
{                           +       +           +                              }
{                           +       +           +                              }
{                           +    +++++++++      +                              }
{                           +            +      +                              }
{                           +            +      +                              }
{                           +++++++      +      +                              }
{                           +            +      +                              }
{                           +            +      +                              }
{                           +++++++++++++++++++++                              }
{                                 SubSeven Legacy                              }
{                                                                              }
{                                                                              }
{                   Author: DarkCoderSc (Jean-Pierre LESUEUR)                  }
{                   https://www.twitter.com/                                   }
{                   https://github.com/darkcodersc                             }
{                   License: Apache License 2.0                                }
{                                                                              }
{                                                                              }
{  Disclaimer:                                                                 }
{  -----------                                                                 }
{    We are doing our best to prepare the content of this app and/or code.     }
{    However, The author cannot warranty the expressions and suggestions       }
{    of the contents, as well as its accuracy. In addition, to the extent      }
{    permitted by the law, author shall not be responsible for any losses      }
{    and/or damages due to the usage of the information on our app and/or      }
{    code.                                                                     }
{                                                                              }
{    By using our app and/or code, you hereby consent to our disclaimer        }
{    and agree to its terms.                                                   }
{                                                                              }
{    Any links contained in our app may lead to external sites are provided    }
{    for convenience only.                                                     }
{    Any information or statements that appeared in these sites or app or      }
{    files are not sponsored, endorsed, or otherwise approved by the author.   }
{    For these external sites, the author cannot be held liable for the        }
{    availability of, or the content located on or through it.                 }
{    Plus, any losses or damages occurred from using these contents or the     }
{    internet generally.                                                       }
{                                                                              }
{                                                                              }
{                                                                              }
{    I dedicate this work to my daughter.                                      }
{                                                                              }
{******************************************************************************}

unit Sub7.Viewer.Clipboard.FileSystem;

interface

uses Sub7.Viewer.Clipboard, System.Classes;

type
  TOnPasteFiles = procedure(Sender : TObject; const AFiles : TStringList) of object;
  TOnPasteFile  = procedure(Sender : TObject; const AFile : String) of object;

  TFileVirtualClipboard = class(TCustomVirtualClipboard)
  private
    FFileList     : TStringList;
    FOnPasteFiles : TOnPasteFiles;
    FOnPasteFile  : TOnPasteFile;
  protected
    ///
  public
    {@C}
    constructor Create(); override;
    destructor Destroy(); override;

    {@M}
    procedure Clear();

    procedure Add(const AFileName : String);

    procedure Copy(const ACopyMode : TClipboardCopyMode = ccmCopy); overload;

    procedure Paste(var AFiles : TStringList); overload;
    procedure Paste(); overload;

    {@S}
    property OnPasteFiles : TOnPasteFiles write FOnPasteFiles;
    property OnPasteFile  : TOnPasteFile  write FOnPasteFile;

    {@G}
    property HasContent;
    property CopyMode;
  end;

implementation

uses System.SysUtils, System.IOUtils, Winapi.Windows;

{ TFileVirtualClipboard.Create }

constructor TFileVirtualClipboard.Create();
begin
  inherited Create();
  ///

  FFileList := TStringList.Create();

  FOnPasteFiles := nil;
  FOnPasteFile  := nil;
end;

{ TFileVirtualClipboard.Destroy }

destructor TFileVirtualClipboard.Destroy();
begin
  if Assigned(FFileList) then
    FreeAndNil(FFileList);

  ///
  inherited Destroy();
end;

{ TFileVirtualClipboard.Clear }

procedure TFileVirtualClipboard.Clear();
begin
  self.ClearClipboard();
  ///

  if Assigned(FFileList) then
    FFileList.Clear();
end;

{ TFileVirtualClipboard.Add }

procedure TFileVirtualClipboard.Add(const AFileName : String);
begin
  if Assigned(FFileList) then
    FFileList.Add(AFileName);
end;

{ TFileVirtualClipboard.Paste }

procedure TFileVirtualClipboard.Paste(var AFiles : TStringList);
var I        : Integer;
    ATmpList : TStringList;
begin
  if not Assigned(AFiles) then
    Exit();
  ///

  ATmpList := self.GetContentStrList();
  if not Assigned(ATmpList) then
    Exit();
  try
    AFiles.Assign(ATmpList);
  finally
    FreeAndNil(ATmpList);
  end;

  if Assigned(FOnPasteFiles) then
    FOnPasteFiles(self, AFiles);

  if Assigned(FOnPasteFile) then begin
    for I := 0 to AFiles.count -1 do
      FOnPasteFile(self, AFiles.Strings[i]);
  end;
end;

{ TFileVirtualClipboad.Paste }

procedure TFileVirtualClipboard.Paste();
var AFiles : TStringList;
begin
  AFiles := TStringList.Create();
  try
    self.Paste(AFiles);
  finally
    if Assigned(AFiles) then
      FreeAndNil(AFiles);
  end;
end;

{ TFileVirtualClipboard.Copy }

procedure TFileVirtualClipboard.Copy(const ACopyMode : TClipboardCopyMode = ccmCopy);
begin
  self.Copy(FFileList, ACopyMode);
  ///

  if Assigned(FFileList) then
    FFileList.Clear();
end;

end.
