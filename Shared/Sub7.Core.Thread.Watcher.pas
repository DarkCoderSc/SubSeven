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

unit Sub7.Core.Thread.Watcher;

interface

uses System.Classes, System.SyncObjs, Sub7.Core.IntervalThread, Sub7.Core.Thread,
     Generics.Collections;

type
  TThreadWatcher = class(TCoreIntervalThread)
  private
    var
      FThreadPool : TObjectDictionary<TCoreThread {Thread}, TCoreThread {Depends On}>;
      FLock       : TCriticalSection;
  protected
    {@M}
    procedure OnDestroyObjects(); override;
    procedure ExecuteInterval(); override;
    procedure TerminatedSet(); override;
  public
    {@C}
    constructor Create(); overload;

    {@M}
    procedure Add(const AThread : TCoreThread; const ADependsOnThread : TCoreThread = nil);
    function Get(const AThreadUniqueId : String) : TCoreThread;

    function Count(const AThreadClass : TClass) : Integer;

    procedure Terminate(const ADependingOn : TCoreThread = nil);
  end;

  var GLOBAL_THREAD_WATCHER : TThreadWatcher;

implementation

uses System.SysUtils, Winapi.Windows, Sub7.Core.Diagnostic, Sub7.Core.Bundle;

{ TThreadWatcher.Count }

function TThreadWatcher.Count(const AThreadClass : TClass) : Integer;
var AThread : TCoreThread;
begin
  result := 0;
  ///

  FLock.Acquire();
  try
    for AThread in FThreadPool.Keys do begin
      if String.Compare(AThread.ClassName, AThreadClass.ClassName, True) = 0 then
        Inc(result);
    end;
  finally
    FLock.Leave();
  end;
end;

{-------------------------------------------------------------------------------
  ___constructor
-------------------------------------------------------------------------------}
constructor TThreadWatcher.Create();
begin
  inherited Create(5000);
  ///

  self.Priority := tpLowest;

  FLock       := TCriticalSection.Create();
  FThreadPool := TObjectDictionary<TCoreThread, TCoreThread>.Create([doOwnsKeys]);
end;

{-------------------------------------------------------------------------------
  ___free
-------------------------------------------------------------------------------}
procedure TThreadWatcher.OnDestroyObjects();
begin
  FLock.Acquire();
  try
    if Assigned(FThreadPool) then
      FreeAndNil(FThreadPool);
  finally
    FLock.Leave();
  end;

  if Assigned(FLock) then
    FreeAndNil(FLock);

  ///
  inherited OnDestroyObjects();
end;

{-------------------------------------------------------------------------------
  ___add
-------------------------------------------------------------------------------}
procedure TThreadWatcher.Add(const AThread : TCoreThread; const ADependsOnThread : TCoreThread = nil);
begin
  if not Assigned(AThread) then
    Exit();
  ///

  FLock.Acquire();
  try
    FThreadPool.Add(AThread, ADependsOnThread);
  finally
    FLock.Leave();
  end;
end;

{-------------------------------------------------------------------------------
  ___get
-------------------------------------------------------------------------------}
function TThreadWatcher.Get(const AThreadUniqueId : String) : TCoreThread;
var AThread : TCoreThread;
begin
  result := nil;

  FLock.Acquire();
  try
    for AThread in FThreadPool.Keys do begin
      if AThread.UniqueId = AThreadUniqueId then begin
        result := AThread;

        break;
      end;
    end;
  finally
    FLock.Leave();
  end;
end;

{ TThreadWatcher.Terminate

  Terminate all threads in pool or thread that depends on "ADependingOn"
  parameter}

procedure TThreadWatcher.Terminate(const ADependingOn : TCoreThread = nil);
var AParent : TCoreThread;
    AThread : TCoreThread;
    AList   : TList<TCoreThread>;
begin
  FLock.Acquire();
  try
    if ADependingOn = nil then
      FThreadPool.Clear()
    else begin
      AList := TList<TCoreThread>.Create();
      try
        // Enumerate threads to terminate
        for AThread in FThreadPool.Keys do begin
          if not FThreadPool.TryGetValue(AThread, AParent) then
            continue;

          if AParent = ADependingOn then
            AList.Add(AThread);
        end;

        // Remove them one by one
        for AThread in AList do
          FThreadPool.Remove(AThread);
      finally
        if Assigned(AList) then
          FreeAndNil(AList);
      end;
    end;
  finally
    FLock.Leave();
  end;
end;

{-------------------------------------------------------------------------------
  ___interval
-------------------------------------------------------------------------------}
procedure TThreadWatcher.ExecuteInterval();

    procedure GetThreadListToTerminate(var AList : TList<TCoreThread>; AParent : TCoreThread = nil);
    var AThreadItem      : TCoreThread;
        ADependsOnThread : TCoreThread;
    begin
      if not Assigned(AList) then
        Exit();
      ///

      for AThreadItem in FThreadPool.Keys do begin
        if not FThreadPool.TryGetValue(AThreadItem, ADependsOnThread) then
          continue;
        ///

        if not Assigned(AParent) then begin
          if not AThreadItem.Active then begin
            GetThreadListToTerminate(AList, AThreadItem);

            ///
            if not AList.Contains(AThreadItem) then
              AList.Add(AThreadItem);
          end;
        end else begin
          if ADependsOnThread = AParent then begin
            GetThreadListToTerminate(AList, AThreadItem);

            ///
            if not AList.Contains(AThreadItem) then
              AList.Add(AThreadItem);
          end;
        end;
      end;
    end;

    var AThread : TCoreThread;
        AList   : TList<TCoreThread>;
begin
  inherited ExecuteInterval();
  ///

  FLock.Acquire();
  try
    AList := TList<TCoreThread>.Create();
    try
      GetThreadListToTerminate(AList);
      ///

      for AThread in AList do
        FThreadPool.Remove(AThread);
    finally
      if Assigned(AList) then
        FreeAndNil(AList);
    end;
  finally
    FLock.Leave();
  end;
end;

{-------------------------------------------------------------------------------

-------------------------------------------------------------------------------}
procedure TThreadWatcher.TerminatedSet();
begin
  inherited TerminatedSet();
  ///

end;

initialization
  GLOBAL_THREAD_WATCHER := TThreadWatcher.Create();

finalization
  if Assigned(GLOBAL_THREAD_WATCHER) then
    FreeAndNil(GLOBAL_THREAD_WATCHER);

end.
