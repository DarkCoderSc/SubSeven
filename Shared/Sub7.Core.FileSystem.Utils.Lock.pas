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

unit Sub7.Core.FileSystem.Utils.Lock;

interface

uses System.Classes;

type
  TFileLock = class
  private
    FFlag     : Integer;
    FLock     : TFileStream;
    FFileName : String;
  public
    {@C}
    constructor Create(const AFileName : String; const ACanBeRead : Boolean);
    destructor Destroy(); override;

    {@M}
    procedure Lock();
    procedure Unlock();
  end;

implementation

uses System.SysUtils;

{ TFileLock.Create }

constructor TFileLock.Create(const AFileName : String; const ACanBeRead : Boolean);
var AFlag : Integer;
begin
  inherited Create();
  ///

  if ACanBeRead then
    AFlag := fmShareDenyWrite
  else
    AFlag := fmShareExclusive;

  FLock     := nil;
  FFlag     := fmOpenRead or AFlag;
  FFileName := AFileName;
end;

{ TFileLock.Destroy }

destructor TFileLock.Destroy();
begin
  self.Unlock();

  ///
  inherited Destroy();
end;

{ TFileLock.Lock }

procedure TFileLock.Lock();
begin
  if not Assigned(FLock) then
    FLock := TFileStream.Create(FFileName, fmOpenRead or FFlag);
end;

{ TFileLock.Unlock }

procedure TFileLock.Unlock();
begin
  if Assigned(FLock) then
    FreeAndNil(FLock);
end;

end.
