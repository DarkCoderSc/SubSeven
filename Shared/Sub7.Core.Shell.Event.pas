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

unit Sub7.Core.Shell.Event;

interface


uses System.Classes, Winapi.Windows;

type
  TS7ShellEvent = class(TObject)
  private
    FGUID : String;
  public
    {@C}
    constructor Create(const AGUID : String);

    {@G}
    property GUID : String read FGUID;
  end;

  TS7ShellBufferChunk = class(TS7ShellEvent)
  private
    FBuffer     : PVOID;
    FBufferSize : Int64;
  public
    {@C}
    constructor Create(const AGUID : String; pBuffer : PVOID; const ABufferSize : Int64);
    destructor Destroy(); override;

    {@G}
    property Buffer     : PVOID read FBuffer;
    property BufferSize : Int64 read FBufferSize;
  end;

  TS7ShellSessionEnded = class(TS7ShellEvent);

implementation

uses System.SysUtils;

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


  TS7ShellEvent


+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

{-------------------------------------------------------------------------------
  ___constructor
-------------------------------------------------------------------------------}
constructor TS7ShellEvent.Create(const AGUID : String);
begin
  inherited Create();
  ///

  FGUID := AGUID;
end;

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


  TS7ShellBufferChunk


+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

{-------------------------------------------------------------------------------
  ___constructor
-------------------------------------------------------------------------------}
constructor TS7ShellBufferChunk.Create(const AGUID : String; pBuffer : PVOID; const ABufferSize : Int64);
begin
  inherited Create(AGUID);
  ///

  FBufferSize := ABufferSize;

  GetMem(FBuffer, FBufferSize);

  CopyMemory(FBuffer, pBuffer, FBufferSize);
end;

{-------------------------------------------------------------------------------
  ___destructor
-------------------------------------------------------------------------------}
destructor TS7ShellBufferChunk.Destroy();
begin
  if Assigned(FBuffer) then
    FreeMem(FBuffer, FBufferSize);

  ///
  inherited Destroy();
end;

end.
