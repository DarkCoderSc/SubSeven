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

unit Sub7.OpenSSL.Cert.Thread.Generate;

interface

uses Classes;

type
  TOnThreadException = procedure(Sender : TObject; const AMessage : String) of object;

  TGenerateCertificate = class;

  TGenerateCertificateThread = class(TThread)
  private
    FOutputFile        : String;

    FOnThreadBegin     : TNotifyEvent;
    FOnThreadEnd       : TNotifyEvent;
    FOnSuccess         : TNotifyEvent;
    FOnThreadException : TOnThreadException;

    {@M}
    procedure DoGenerateCertificate();
  protected
    {@M}
    procedure Execute(); override;
  public
    {@C}
    constructor Create(const AOwner : TGenerateCertificate); overload;
    destructor Destroy(); override;

    {@M}
    procedure TerminateAndWaitFor();
  end;

  TGenerateCertificate = class
  private
    FOutputFile        : String;

    FOnThreadBegin     : TNotifyEvent;
    FOnThreadEnd       : TNotifyEvent;
    FOnSuccess         : TNotifyEvent;
    FOnThreadException : TOnThreadException;

    FThreadInstance    : TGenerateCertificateThread;
  public
    {@C}
    constructor Create(const AOutputFile : String);
    destructor Destroy(); override;

    procedure Generate();

    {@G/S}
    property OutputFile        : String              read FOutputFile        write FOutputFile;
    property OnThreadBegin     : TNotifyEvent        read FOnThreadBegin     write FOnThreadBegin;
    property OnThreadEnd       : TNotifyEvent        read FOnThreadEnd       write FOnThreadEnd;
    property OnSuccess         : TNotifyEvent        read FOnSuccess         write FOnSuccess;
    property OnThreadException : TOnThreadException  read FOnThreadException write FOnThreadException;
  end;

implementation

uses Winapi.Windows, Sub7.OpenSSL.Cert.Utils, System.SysUtils;

{
  /// TGenerateCertificate ///
}

{ TGenerateCertificate.Create }

constructor TGenerateCertificate.Create(const AOutputFile : String);
begin
  inherited Create();
  ///

  FOutputFile          := AOutputFile;

  FOnThreadBegin       := nil;
  FOnThreadEnd         := nil;
  FOnThreadException   := nil;
  FOnSuccess           := nil;

  FThreadInstance      := nil;
end;

{ TGenerateCertificate.Destroy }

destructor TGenerateCertificate.Destroy();
begin
  if Assigned(FThreadInstance) then begin
    FThreadInstance.TerminateAndWaitFor();

    FreeAndNil(FThreadInstance);
  end;

  ///
  inherited Destroy();
end;

{ TGenerateCertificate.Generate }

procedure TGenerateCertificate.Generate();
begin
  FThreadInstance := TGenerateCertificateThread.Create(self);
end;

{
  /// TGenerateCertificateThread ///
}

{ TGenerateCertificateThread.Create }

constructor TGenerateCertificateThread.Create(const AOwner : TGenerateCertificate);
begin
  inherited Create(False);
  ///

  self.Priority        := tpNormal;
  self.FreeOnTerminate := False;

  FOutputFile          := AOwner.OutputFile;

  FOnThreadBegin       := AOwner.OnThreadBegin;
  FOnThreadEnd         := AOwner.OnThreadEnd;
  FOnThreadException   := AOwner.OnThreadException;
  FOnSuccess           := AOwner.OnSuccess;
end;

{ TGenerateCertificateThread.Destroy }

destructor TGenerateCertificateThread.Destroy();
begin

  ///
  inherited Destroy();
end;

{ TGenerateCertificateThread.DoGenerateCertificate }

procedure TGenerateCertificateThread.DoGenerateCertificate();
begin
  OpenSSL_GenerateSelfSignedCertificate(FOutputFile);
end;

{ TGenerateCertificateThread.Execute }

procedure TGenerateCertificateThread.Execute();
begin
  try
    if Assigned(FOnThreadBegin) then
      Synchronize(procedure begin
        FOnThreadBegin(self);
      end);
    try
      try
        ///

        self.DoGenerateCertificate();

        if Assigned(FOnSuccess) then
          Synchronize(procedure begin
            FOnSuccess(self);
          end);

        ///
      except
        on E : Exception do begin
          if Assigned(FOnThreadException) then
            Synchronize(procedure begin
              FOnThreadException(self, E.Message);
            end);
        end;
      end;
    finally
      if Assigned(FOnThreadEnd) then
        Synchronize(procedure begin
          FOnThreadEnd(self);
        end);
    end;
  finally
    ExitThread(0);
  end;
end;

{ TGenerateCertificateThread.TerminateAndWaitFor }

procedure TGenerateCertificateThread.TerminateAndWaitFor();
var AExitCode : Cardinal;
begin
  GetExitCodeThread(self.Handle, AExitCode);
  if (AExitCode = STILL_ACTIVE) then begin
    self.Terminate();

    self.WaitFor();
  end;
end;

end.
