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

unit S7Reg;

interface

uses System.Classes, Sub7.Viewer.VCL.Button, Sub7.Viewer.VCL.CaptionBar, S7Edit, Sub7.Viewer.VCL.SubSevenForm, 
     Sub7.Viewer.VCL.CheckBox, S7Panel,
     S7StatusBar, S7TreeView, S7ImageButton, S7ComboBox, S7THeme, S7MessageBox,
     S7Hint, S7OptionDialog, S7Gauge, S7PopupMenu, S7DockCaption, S7GroupBox,
     S7SettingHandler, S7ScrollingCredit, S7Timer, S7PaintScene;

procedure register;

implementation

{-------------------------------------------------------------------------------
  ___Register
-------------------------------------------------------------------------------}
procedure register;
begin
  RegisterComponents('S7C', [
                              TS7Button,
                              TS7Captionbar,
                              TS7Edit,
                              TS7Form,
                              TS7CheckBox,
                              TS7Panel,
                              TS7StatusBar,
                              TS7VirtualStringTree,
                              TS7ImageButton,
                              TS7ComboBox,
                              TS7MessageBox,
                              TS7Hint,
                              TS7OptionDialog,
                              TS7Gauge,
                              TS7PopupMenu,
                              TS7DockCaption,
                              TS7GroupBox,
                              TS7SettingHandler,
                              TS7ScrollingCredit,
                              TS7Timer,
                              TS7PaintScene
  ]);
end;

end.
