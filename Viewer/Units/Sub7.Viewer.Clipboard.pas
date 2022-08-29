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

unit Sub7.Viewer.Clipboard;

interface

uses System.Classes;

type
  TClipboardCopyMode = (
    ccmCut,
    ccmCopy
  );

  TClipboardDataKind = (
    cdkRaw,
    cdkWideString,
    cdkStringList
  );

  TCustomVirtualClipboard = class
  private
    FContent     : Pointer;
    FContentSize : Int64;
    FContentKind : TClipboardDataKind;
    FCopyMode    : TClipboardCopyMode;

    {@M}
    function GetHasContent() : Boolean;
  protected
    {@M}
    procedure Copy(const AContent : String; const ACopyMode : TClipboardCopyMode = ccmCopy); overload;
    procedure Copy(const pBuffer : Pointer; const ABufferSize : Int64); overload;
    procedure Copy(const AContent : TStringList; const ACopyMode : TClipboardCopyMode = ccmCopy); overload;

    function GetContentStr()     : String;      overload;
    function GetContentPtr()     : Pointer;     overload;
    function GetContentStrList() : TStringList; overload;

    procedure ClearClipboard(); virtual;

    {@G}
    property HasContent  : Boolean            read GetHasContent;
    property ContentSize : Int64              read FContentSize;
    property CopyMode    : TClipboardCopyMode read FCopyMode;
  public
    {@C}
    constructor Create(); virtual;
  end;

implementation

uses System.SysUtils, Winapi.Windows, Sub7.Core.Utils.Memory, Sub7.Core.Exceptions,
     Sub7.Core.Bundle;

{ TCustomVirtualClipboard.Create }

constructor TCustomVirtualClipboard.Create();
begin
  inherited Create();
  ///

  FCopyMode    := ccmCopy;
  FContentKind := cdkRaw;

  self.ClearClipboard();
end;

{ TCustomVirtualClipboard.ClearClipboard }

procedure TCustomVirtualClipboard.ClearClipboard();
begin
  if self.GetHasContent() then
    FreeMem(FContent, FContentSize);

  ///
  FContent := nil;
end;

{ TCustomVirtualClipboard.Copy }

procedure TCustomVirtualClipboard.Copy(const AContent : String; const ACopyMode : TClipboardCopyMode = ccmCopy);
begin
  self.ClearClipboard();
  ///

  FContentSize := Length(AContent) * SizeOf(WideChar);

  GetMem(FContent, FContentSize);

  StrPCopy(PWideChar(FContent), AContent);

  FCopyMode := ACopyMode;

  FContentKind := cdkWideString;
end;

{ TCustomVirtualClipboard.Copy }

procedure TCustomVirtualClipboard.Copy(const pBuffer : Pointer; const ABufferSize : Int64);
begin
  self.ClearClipboard();
  ///

  if not Assigned(pBuffer) or (ABufferSize <= 0) then
    Exit();

  CopyMemory(FContent, pBuffer, ABufferSize);

  FContentKind := cdkRaw;
end;

{ TCustomVirtualClipboard.Copy }

procedure TCustomVirtualClipboard.Copy(const AContent : TStringList; const ACopyMode : TClipboardCopyMode = ccmCopy);
begin
  self.ClearClipboard();
  ///

  FContent := CopyStringListToMemory(AContent, FContentSize);

  FCopyMode := ACopyMode;

  FContentKind := cdkStringList;
end;

{ TCustomVirtualClipboard.GetContentStr }

function TCustomVirtualClipboard.GetContentStr() : String;
begin
  result := '';
  ///

  if FContentKind <> cdkWideString then
    raise ES7CastException.Create(Format(ERR_INVALID_STORED_CAST, ['WideString']));

  if not self.GetHasContent() then
    Exit();

  SetString(result, PWideChar(FContent), FContentSize div SizeOf(WideChar));
end;

{ TCustomVirtualClipboard.GetContentPtr }

function TCustomVirtualClipboard.GetContentPtr() : Pointer;
begin
  result := nil;
  ///

  if FContentKind <> cdkRaw then
    raise ES7CastException.Create(Format(ERR_INVALID_STORED_CAST, ['Pointer']));

  if not self.GetHasContent() then
    Exit();

  result := FContent;
end;

{ TCustomVirtualClipboard.GetContentStrList }

function TCustomVirtualClipboard.GetContentStrList() : TStringList;
begin
  if FContentKind <> cdkStringList then
    raise ES7CastException.Create(Format(ERR_INVALID_STORED_CAST, ['TStringList']));
  ///

  result := CreateStringListFromMemory(FContent, FContentSize);
end;

{ TCustomVirtualClipboard.GetHasContent }

function TCustomVirtualClipboard.GetHasContent() : Boolean;
begin
  result := Assigned(FContent) and (FContentSize > 0);
end;

end.
