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

unit Sub7.Core.Utils.Memory;

interface

uses System.Classes;

function CopyMemoryStreamToMemory(const AStream : TMemoryStream; var ABufferSize : Int64) : Pointer;
function CopyStringListToMemory(const AStringList : TStringList; var ABufferSize : Int64) : Pointer;

function CreateMemoryStreamFromMemory(const pMemoryAddress : Pointer; const ABufferSize : Int64) : TMemoryStream;
function CreateStringListFromMemory(const pMemoryAddress : Pointer; const ABufferSize : Int64) : TStringList;

implementation

uses System.SysUtils, Winapi.Windows;

{ _.CopyMemoryStreamToMemory }

function CopyMemoryStreamToMemory(const AStream : TMemoryStream; var ABufferSize : Int64) : Pointer;
begin
  result := nil;
  ///

  if not Assigned(AStream) then
    Exit();

  if AStream.Size = 0 then
    Exit();

  GetMem(result, AStream.Size);

  CopyMemory(result, AStream.Memory, AStream.Size);

  ABufferSize := AStream.Size;
end;

{ _.CreateMemoryStreamFromMemory }

function CreateMemoryStreamFromMemory(const pMemoryAddress : Pointer; const ABufferSize : Int64) : TMemoryStream;
begin
  result := nil;
  ///

  if not Assigned(pMemoryAddress) or (ABufferSize <= 0) then
    Exit();
  ///

  result := TMemoryStream.Create();

  result.SetSize(ABufferSize);

  result.Position := 0;

  result.Write(PByte(pMemoryAddress)^, ABufferSize);

  result.Position := 0;
end;

{ _.CopyStringListToMemory }

function CopyStringListToMemory(const AStringList : TStringList; var ABufferSize : Int64) : Pointer;
var AStream : TMemoryStream;
begin
  result := nil;
  ///

  if not Assigned(AStringList) then
    Exit();

  if AStringList.Count = 0 then
    Exit();

  AStream := TMemoryStream.Create();
  try
    AStringList.SaveToStream(AStream);
    ///

    result := CopyMemoryStreamToMemory(AStream, ABufferSize);
  finally
    if Assigned(AStream) then
      FreeAndNil(AStream);
  end;
end;

{ _.CreateStringListFromMemory }

function CreateStringListFromMemory(const pMemoryAddress : Pointer; const ABufferSize : Int64) : TStringList;
var AStream : TMemoryStream;
begin
  result := nil;
  ///

  AStream := CreateMemoryStreamFromMemory(pMemoryAddress, ABufferSize);
  if not Assigned(AStream) then
    Exit();
  try
    result := TStringList.Create();

    result.LoadFromStream(AStream);
  finally
    if Assigned(AStream) then
      FreeAndNil(AStream);
  end;
end;

end.
