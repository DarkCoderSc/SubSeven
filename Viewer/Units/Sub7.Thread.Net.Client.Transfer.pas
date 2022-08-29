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

unit Sub7.Thread.Net.Client.Transfer;

interface

uses System.Classes, Winapi.Windows, Sub7.Core.Protocol, VirtualTrees, System.SysUtils,
     Sub7.Net.Client.Context, Sub7.Thread.Net.Client.Base;

type
  TThreadTransfer = class(TSub7ClientBase)
  private

  protected
    FOrigin      : String;
    FDestination : String;
    FNode        : PVirtualNode;
    FFileStream  : TFileStream;
    FSuccess     : Boolean;
    FCanceled    : Boolean;

    {@M}
    procedure SetStatus(const AStatus : TS7TransferStatus);

    procedure OnClientExecute(); override;

    procedure Transfer(); virtual; abstract;

    procedure ExceptionHandler(const E : Exception); override;
  public
    {@C}
    constructor Create(const AContext : TSub7ClientContext; const ANode : PVirtualNode);
    destructor Destroy(); override;

    {@M}
    procedure Close(const ACancel : Boolean = False); overload;
  end;

implementation

uses uFormQueue, Sub7.Core.Bundle, Sub7.Thread.Net.Client.Transfer.Download,
     Sub7.Thread.Net.Client.Transfer.Upload, Sub7.Core.Exceptions, Sub7.Core.Types,
     Sub7.OpenSSL.TLS.Exceptions;

{-------------------------------------------------------------------------------
  Transfer Exception
-------------------------------------------------------------------------------}
procedure TThreadTransfer.ExceptionHandler(const E : Exception);
var ASocketException : Boolean;
begin
  ASocketException := (E is ETLSSocketException) or
                      (E is EOpenSSLBaseException);
  ///

  if not FCanceled then
    SafeSynchronize(procedure begin
      if ASocketException then
        FormQueue.DeleteTransfer(FNode)
      else
        FormQueue.OnTransferException(E, FNode);
    end)
  else
    SafeSynchronize(procedure begin
      FormQueue.ReQueue(FNode);
    end);
end;

{-------------------------------------------------------------------------------
  ___constructor
-------------------------------------------------------------------------------}
constructor TThreadTransfer.Create(const AContext : TSub7ClientContext; const ANode : PVirtualNode);
var AWorkerKind  : TWorkerKind;
    pData        : PTreeData;
    ADirection   : TS7TransferDirection;

    AStreamFile  : String;
    AStreamFlags : Integer;
begin
  inherited Create(AContext);
  ///

  FNode        := ANode;
  FFileStream  := nil;
  FSuccess     := False;
  FCanceled    := False;
  AStreamFlags := 0;

  if self is TThreadTransferDownload then begin
    AWorkerKind  := wkDownloadHandler;
    ADirection   := tdDownload;
  end else if self is TThreadTransferUpload then begin
    AWorkerKind  := wkUploadHandler;
    ADirection   := tdUpload;
  end else
    raise Exception.Create(ERR_INVALID_CLASS);
  ///

  if not Assigned(ANode) then
    raise Exception.Create(ERR_MISSING_INSTANCE);

  SafeSynchronize(procedure begin
    pData := ANode.GetData;
    ///

    FOrigin          := pData^.Origin;
    FDestination     := pData^.Destination;
    pData^.Client    := self;
    pData^.Direction := ADirection;
  end);

  case AWorkerKind of
    wkDownloadHandler : begin
      AStreamFlags := (fmShareExclusive or fmCreate);
      AStreamFile  := FDestination;

      if FileExists(AStreamFile) then
        raise ES7FileException.Create(Format(ERR_FILE_EXISTS, [AStreamFile]));
    end;

    wkUploadHandler : begin
      AStreamFlags := fmShareDenyWrite;
      AStreamFile  := FOrigin;
    end;
  end;

  FFileStream := TFileStream.Create(AStreamFile, AStreamFlags);

  FFileStream.Position := 0;
end;

{-------------------------------------------------------------------------------
  ___destructor
-------------------------------------------------------------------------------}
destructor TThreadTransfer.Destroy();
var AStreamFile : String;
begin
  if Assigned(FFileStream) then begin
    AStreamFile := FFileStream.FileName;

    FreeAndNil(FFileStream);

    { Delete incomplete downloaded stream }
    if not FSuccess and (self is TThreadTransferDownload) then
      DeleteFile(AStreamFile);
  end;

  ///
  inherited Destroy();
end;

{-------------------------------------------------------------------------------
  Handle Close Request
-------------------------------------------------------------------------------}
procedure TThreadTransfer.Close(const ACancel : Boolean = False);
begin
  FCanceled := ACancel;

  self.Terminate();
end;

{-------------------------------------------------------------------------------
  Set Listview Status
-------------------------------------------------------------------------------}
procedure TThreadTransfer.SetStatus(const AStatus : TS7TransferStatus);
begin
  SafeSynchronize(procedure begin
    FormQueue.SetStatus(FNode, AStatus);
  end);
end;

{-------------------------------------------------------------------------------
  ___execute
-------------------------------------------------------------------------------}
procedure TThreadTransfer.OnClientExecute();
begin
  try
    ////////////////////////////////////////////////////////////////////////////

    SafeSynchronize(procedure begin
      self.SetStatus(tsProgress);
    end);

    { Do Transfer Process }
    self.Transfer();

    FSuccess := (not self.Terminated) and (not FCanceled);

    { Delete Queue Node }
    SafeSynchronize(procedure begin
      FormQueue.TransferEnded(FNode);
    end);

    ////////////////////////////////////////////////////////////////////////////
  except
    on E : Exception do
      self.ExceptionHandler(E);
  end;
end;

end.
