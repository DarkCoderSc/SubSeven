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

unit Sub7.Core.Thread;

interface

uses System.Classes, System.SyncObjs, System.SysUtils,
     Sub7.Core.Synchronization.CriticalSectionEx;

type
  TOnThreadExecute = TNotifyEvent;

  TCoreThread = class(TThread)
  private
    FStartTick : Int64;
    FUniqueId  : String;
    FForceKill : Boolean;

    {@M}
    function GetActive() : Boolean;
  protected
    FThreadStarted : TSafeBoolean;

    {@M}
    procedure Execute(); override;

    procedure OnThreadExecute(); virtual; abstract;
    procedure OnExitThread(); virtual;
    procedure ExceptionHandler(const E : Exception); virtual;
    procedure OnDestroyObjects(); virtual;

    procedure SafeSynchronize(AThreadProc: TThreadProcedure);
  public
    {@C}
    constructor Create(); overload;
    destructor Destroy(); override;

    {@G}
    property Active    : Boolean read GetActive;
    property StartTick : Int64   read FStartTick;
    property UniqueId  : String  read FUniqueId;

    {@G/S}
    property ForceKill : Boolean read FForceKill write FForceKill;
  end;

implementation

uses Sub7.Core.Diagnostic, Sub7.Core.Utils, Winapi.Windows, System.Hash,
     System.Diagnostics;

{ TCoreThread.ExceptionHandler

  Handle exceptions that occurs on thread execution. }

procedure TCoreThread.ExceptionHandler(const E : Exception);
begin

end;

{-------------------------------------------------------------------------------
  ___constructor
-------------------------------------------------------------------------------}
constructor TCoreThread.Create();
begin
  inherited Create(False);
  ///

  self.FreeOnTerminate := False;
  self.Priority        := tpNormal;

  FForceKill           := False;

  FThreadStarted := TSafeBoolean.Create(False);
  FStartTick     := GetTickCount();

  FUniqueId := System.Hash.THashSHA2.GetHashString(
                                                      Format('%s%s%d', [
                                                        TGUID.NewGuid.ToString(),
                                                        RandomString(32),
                                                        GetTickCount()
                                                      ]),

                                                      SHA512
  );
end;

{-------------------------------------------------------------------------------
  ___destructor
-------------------------------------------------------------------------------}
destructor TCoreThread.Destroy();
var AStopwatch : TStopwatch;
begin
  {
    If thread is active, we call terminate and wait for it to finish.
  }
  if GetActive() then begin
    self.Terminate();

    if not FForceKill then begin
      self.WaitFor();
    end else begin
      AStopwatch := TStopwatch.Create();
      AStopwatch.Start();
      ///

      while True do begin
        if not self.GetActive() then
          break;

        if WaitForSingleObject(self.Handle, 1000) <> WAIT_TIMEOUT then
          break;

        // Wait for 30 seconds for thread to close or we kill it
        if AStopwatch.ElapsedMilliseconds > 30 * 1000 then begin
          Log(Format('Could not gracefully stop "%s"(%d) thread execution. Hard kill attempt.', [self.ClassName, self.ThreadID]));

          TerminateThread(self.Handle, 0);

          break;
        end;
      end;
    end;
  end;

  {
    Destroy allocated objects during thread creation / execution.
  }
  self.OnDestroyObjects();

  Log(Format('Thread "%s" Destroyed, thread_id=[%d].', [self.ClassName, self.ThreadID]));

  ///
  inherited Destroy();
end;

{-------------------------------------------------------------------------------
  Find if current thread is still active
-------------------------------------------------------------------------------}
function TCoreThread.GetActive() : Boolean;
var AExitCode : Cardinal;
begin
  result := False;
  ///

  if not FThreadStarted.GetValue() then
    Exit();
  ///

  GetExitCodeThread(self.Handle, AExitCode);
  result := (AExitCode = STILL_ACTIVE)
end;

{-------------------------------------------------------------------------------
  This is the method to override when we want to free objects from thread.
-------------------------------------------------------------------------------}
procedure TCoreThread.OnDestroyObjects();
begin

  ///
  if Assigned(FThreadStarted) then
    FreeAndNil(FThreadStarted);
end;

{-------------------------------------------------------------------------------
  This method is called at the very end of ___Execute() to exit gracefully the
  thread.
-------------------------------------------------------------------------------}
procedure TCoreThread.OnExitThread();
begin
  Log(Format('Thread "%s" Has Exit, thread_id=[%d].', [self.ClassName, self.ThreadID]));

  ///
  ExitThread(0);
end;

{-------------------------------------------------------------------------------
  Better approach for main GUI synchronisation
-------------------------------------------------------------------------------}
procedure TCoreThread.SafeSynchronize(AThreadProc: TThreadProcedure);
begin
  if (GetCurrentThreadId <> System.MainThreadID) then
    Synchronize(AThreadProc)
  else
    AThreadProc();
end;

{-------------------------------------------------------------------------------
  ___execute
-------------------------------------------------------------------------------}
procedure TCoreThread.Execute();
begin
  try
    Log(Format('Thread "%s" Execute, thread_id=[%d].', [self.ClassName, self.ThreadID]));
    ///

    ///
    FThreadStarted.SetValue(True);
    ///

    try
      self.OnThreadExecute();
    except
      on E : Exception do begin
        CrashLog(E, self);

        ///
        self.ExceptionHandler(E);
      end;
    end;
  finally
    self.OnExitThread();
  end;
end;

end.
