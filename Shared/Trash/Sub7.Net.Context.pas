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

unit Sub7.Net.Context;

interface

uses System.Classes, Sub7.OpenSSL.TLS.Context, Generics.Collections, Winapi.Winsock2,
     Sub7.Core.Thread, System.SyncObjs;

type
  TSub7Context = class(TOpenSSLContext)
  private
    FSocketList   : TThreadList<TSocket>;
    FWorkers      : TObjectList<TCoreThread>;
    FWorkersLock  : TCriticalSection;
  public
    {@C}
    constructor Create(const AMode : TContextMode; const ACertFile : AnsiString); override;
    destructor Destroy(); override;

    {@M}
    procedure RegisterSocket(const ASocket : TSocket);
    procedure UnregisterSocket(const ASocket : TSocket);
    procedure DisconnectAll();

    procedure AddWorker(const AWorker : TCoreThread);

    procedure CleanDeadWorkers();
  end;

implementation

uses System.SysUtils, UntFunctions;

{-------------------------------------------------------------------------------
  ___constructor
-------------------------------------------------------------------------------}
constructor TSub7Context.Create(const AMode : TContextMode; const ACertFile : AnsiString);
begin
  inherited Create(cmClient, ACertFile);
  ///

  FSocketList   := TThreadList<TSocket>.Create();
  FWorkersLock  := TCriticalSection.Create();
  FWorkers      := TObjectList<TCoreThread>.Create(True);
end;

{-------------------------------------------------------------------------------

-------------------------------------------------------------------------------}
procedure TSub7Context.DisconnectAll();
var ASocketList : TList<TSocket>;
    ASocket     : TSocket;
begin
  {
    First we kill all sockets associated with this context
  }
  if Assigned(FSocketList) then begin
    ASocketList := FSocketList.LockList;
    try
      for ASocket in ASocketList do
        Winapi.Winsock2.closesocket(ASocket);
    finally
      FSocketList.UnlockList();
    end;
  end;
end;

{-------------------------------------------------------------------------------
  ___destructor
-------------------------------------------------------------------------------}
destructor TSub7Context.Destroy();
begin

  {
    Then we destroy all Workers associated with this context
  }
  FWorkersLock.Enter();
  try
    if Assigned(FWorkers) then
      FreeAndNil(FWorkers);
  finally
    FWorkersLock.Leave();

    ///
    FreeAndNil(FWorkersLock);
  end;

  {
    We can now free socket list
  }
  if Assigned(FSocketList) then
    FreeAndNil(FSocketList);

  ///
  inherited Destroy();
end;

{-------------------------------------------------------------------------------
  Remove dead Workers (threads) from Worker List
-------------------------------------------------------------------------------}
procedure TSub7Context.CleanDeadWorkers();
var AWorker : TCoreThread;
begin
  FWorkersLock.Enter();
  try
    if not Assigned(FWorkers) then
      Exit();
    ///

    for AWorker in self.FWorkers do begin
      if (not IsThreadActive(AWorker)) then
        FWorkers.Remove(AWorker);
    end;
  finally
    FWorkersLock.Leave();
  end;
end;

{-------------------------------------------------------------------------------
  Add new Worker to list
-------------------------------------------------------------------------------}
procedure TSub7Context.AddWorker(const AWorker : TCoreThread);
begin
  FWorkersLock.Enter();
  try
    if Assigned(AWorker) then
      FWorkers.Add(AWorker);
  finally
    FWorkersLock.Leave();
  end;
end;

{-------------------------------------------------------------------------------
  Register / Unregister socket to/from socket list
-------------------------------------------------------------------------------}

procedure TSub7Context.RegisterSocket(const ASocket : TSocket);
var ASocketList : TList<TSocket>;
begin
  if not Assigned(FSocketList) then
    Exit();
  ///

  ASocketList := FSocketList.LockList;
  try
    ASocketList.Add(ASocket);
  finally
    FSocketList.UnlockList();
  end;
end;

procedure TSub7Context.UnregisterSocket(const ASocket : TSocket);
var ASocketList : TList<TSocket>;
begin
  if not Assigned(FSocketList) then
    Exit();
  ///

  ASocketList := FSocketList.LockList;
  try
    ASocketList.Remove(ASocket);
  finally
    FSocketList.UnlockList();
  end;
end;

end.
