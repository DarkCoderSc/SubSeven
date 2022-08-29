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

unit Sub7.Thread.Net.Client.NonBlockingCmd;

interface

uses System.Classes, Sub7.Thread.Net.Client.Base, System.SyncObjs, Sub7.Core.Protocol,
     XSuperObject, Sub7.Net.Client.Context, Generics.Collections, Sub7.Core.Thread;

type
  TSub7CommandObject = class
  private
    FCommand : TS7Command;
    FData    : ISuperObject;
  public
    {@C}
    constructor Create(const ACommand : TS7Command; const AData : ISuperObject = nil);

    {@G}
    property Command : TS7Command   read FCommand;
    property Data    : ISuperObject read FData;
  end;

  TSub7NonBlockingCmd = class(TSub7ClientBase)
  private
    FQueue : TThreadedQueue<TSub7CommandObject>;

    {@M}
    procedure OnClientExecute(); override;
  protected
    procedure OnDestroyObjects(); override;
    procedure OnIdle(); virtual;
    procedure OnBeforeCommandLoop(); virtual; abstract;
    procedure OnCommand(const ACommand : TS7Command; AData : ISuperObject = nil); virtual; abstract;
    procedure OnAfterCommandLoop(); virtual; abstract;
  public
    {@M}
    procedure SendCommand(const ACommand : TS7Command; AJsonData : ISuperObject = nil);

    {@C}
    constructor Create(const AContext : TSub7ClientContext); override;
  end;

implementation

uses Winapi.Windows, System.SysUtils, Sub7.OpenSSL.TLS.Exceptions, Sub7.Core.Exceptions;

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


  TSub7NonBlockingCmd


+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

{-------------------------------------------------------------------------------
  ___constructor
-------------------------------------------------------------------------------}
constructor TSub7NonBlockingCmd.Create(const AContext : TSub7ClientContext);
begin
  inherited Create(AContext);
  ///

  FQueue := TThreadedQueue<TSub7CommandObject>.Create(100, INFINITE, 1);
end;

{-------------------------------------------------------------------------------
  ___free
-------------------------------------------------------------------------------}
procedure TSub7NonBlockingCmd.OnDestroyObjects();
begin
  if Assigned(FQueue) then begin
	FQueue.DoShutdown();
	
    FreeAndNil(FQueue);
  end;

  ///
  inherited OnDestroyObjects();
end;

{-------------------------------------------------------------------------------
  ___idle
-------------------------------------------------------------------------------}
procedure TSub7NonBlockingCmd.OnIdle();
begin
  Sleep(10);
end;

{-------------------------------------------------------------------------------
  ___execute
-------------------------------------------------------------------------------}
procedure TSub7NonBlockingCmd.OnClientExecute();
var AObject  : TSub7CommandObject;
    ACommand : TS7Command;
    AData    : ISuperObject;
begin
  try
    self.OnBeforeCommandLoop();
    ///

    while not Terminated do begin
      try
        self.OnIdle();
        ///

        {***********************************************************************
          Get commands available in pool to send to server
        ***********************************************************************}
        while (FQueue.PopItem(AObject) = TWaitResult.wrSignaled) do begin
          if Terminated then
            break;
          ///

          if Assigned(FIOHandler) and Assigned(AObject) then begin
            try
              FIOHandler.SendCommand(AObject.Command, AObject.FData);
            finally
              FreeAndNil(AObject);
            end;
          end;
        end;

        {***********************************************************************
          Receive commands from server (RESP)
        ***********************************************************************}
        if not FIOHandler.DataAvailable then
          continue;

        FIOHandler.GetCommand(ACommand, AData);

        self.OnCommand(ACommand, AData);
      except
        on E : Exception do begin
          if (E is ETLSSocketException) or (E is EOpenSSLBaseException) then begin
            raise;
          end else

          if (E is ES7Exception) or (E is ES7ServerException) then begin
            self.ExceptionHandler(E);

            ///
            continue;
          end

          else
            raise; // Other exception are considered fatal
        end;
      end;
    end;
  finally
    self.OnAfterCommandLoop();
  end;
end;

{-------------------------------------------------------------------------------
  Push a new Sub7 command to command queue. Since OpenSSL doesn't guarantee that
  SSL_write() and SSL_read() can be used with synchronicity, we must make it
  threadsafe using smart locks.
-------------------------------------------------------------------------------}

procedure TSub7NonBlockingCmd.SendCommand(const ACommand : TS7Command; AJsonData : ISuperObject = nil);
begin
  if (not Assigned(FQueue)) or (not Assigned(FIOHandler)) then
    Exit();
  ///

  FQueue.PushItem(TSub7CommandObject.Create(ACommand, AJsonData));
end;

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


  TSub7CommandObject


+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

{-------------------------------------------------------------------------------
  ___constructor
-------------------------------------------------------------------------------}
constructor TSub7CommandObject.Create(const ACommand : TS7Command; const AData : ISuperObject = nil);
begin
  inherited Create();
  ///

  FCommand := ACommand;
  FData    := AData;
end;

end.
