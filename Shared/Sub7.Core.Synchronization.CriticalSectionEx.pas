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

unit Sub7.Core.Synchronization.CriticalSectionEx;

interface

uses System.Classes, System.SyncObjs;

type
  TSafeVariable = class
  protected
    FLock : TCriticalSection;
  public
    {@C}
    constructor Create(); virtual;
    destructor Destroy(); override;
  end;

  TSafeBoolean = class(TSafeVariable)
  private
    FValue : Boolean;
  public
    {@C}
    constructor Create(const AValue : Boolean); overload;

    {@M}
    procedure SetValue(const AValue : Boolean);
    function GetValue() : Boolean;
  end;

implementation

uses System.SysUtils;

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


  TSafeVariable


+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

{-------------------------------------------------------------------------------
  ___constructor
-------------------------------------------------------------------------------}
constructor TSafeVariable.Create();
begin
  inherited Create();
  ///

  FLock := TCriticalSection.Create();
end;

{-------------------------------------------------------------------------------
  ___destructor
-------------------------------------------------------------------------------}
destructor TSafeVariable.Destroy();
begin
  if Assigned(FLock) then
    FreeAndNil(FLock);

  ///
  inherited Destroy();
end;

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


  TSafeBoolean


+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

{-------------------------------------------------------------------------------
  ___constructor
-------------------------------------------------------------------------------}
constructor TSafeBoolean.Create(const AValue : Boolean);
begin
  inherited Create();
  ///

  FValue := AValue;
end;

{-------------------------------------------------------------------------------
  ___getters___setters
-------------------------------------------------------------------------------}

function TSafeBoolean.GetValue() : Boolean;
begin
  FLock.Acquire();
  try
    result := FValue;
  finally
    FLock.Leave();
  end;
end;

procedure TSafeBoolean.SetValue(const AValue : Boolean);
begin
  FLock.Acquire();
  try
    FValue := AValue;
  finally
    FLock.Leave();
  end;
end;

end.
