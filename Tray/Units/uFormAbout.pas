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

unit uFormAbout;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Imaging.pngimage,
  Vcl.ExtCtrls, S7Panel, Vcl.StdCtrls;

type
  TFormAbout = class(TForm)
    PanelClient: TS7Panel;
    ImageLogo: TImage;
    LabelName: TLabel;
    LabelVersion: TLabel;
    LabelWebSite: TLabel;
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure LabelWebSiteClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    {@M}
    procedure DoResize();
  public
    { Public declarations }
  end;

var
  FormAbout: TFormAbout;

implementation

uses Winapi.ShellAPI, Sub7.Core.Windows.PE.Version, Sub7.Core.Application.Env;

{$R *.dfm}

procedure TFormAbout.DoResize();
begin
  ImageLogo.Left    := (PanelClient.Width div 2) - (ImageLogo.Width div 2);
  LabelName.Left    := (PanelClient.Width div 2) - (LabelName.Width div 2);
  LabelVersion.Left := (PanelClient.Width div 2) - (LabelVersion.Width div 2);
  LabelWebSite.Left := (PanelClient.Width div 2) - (LabelWebSite.Width div 2);
end;

procedure TFormAbout.FormCreate(Sender: TObject);
begin
  LabelVersion.Caption := Format('%s %s (REL %s)', [
    TVersion.GetFileVersion(GetModuleName(0)),
    APP_ENV_ReleaseName,
    APP_ENV_ReleaseDate
  ]);
end;

procedure TFormAbout.FormResize(Sender: TObject);
begin
  DoResize();
end;

procedure TFormAbout.FormShow(Sender: TObject);
begin
  DoResize();
end;

procedure TFormAbout.LabelWebSiteClick(Sender: TObject);
begin
  ShellExecute(0, 'open', 'https://www.sub7crew.org', nil, nil, SW_SHOW);
end;

end.
