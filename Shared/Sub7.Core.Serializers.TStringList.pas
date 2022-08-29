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

unit Sub7.Core.Serializers.TStringList;

interface

uses System.Classes, XSuperObject;

type
  TStringList_Serializer = class
    class function FlatSerialize(const AStringList : TStringList) : ISuperArray; static;
    class function Serialize(const AStringList : TStringList) : ISuperObject; static;

    class function FlatDeSerialize(const ASerializedArray : ISuperArray) : TStringList; static;
    class function DeSerialize(const ASerializedObject : ISuperObject) : TStringList; static;
  end;

implementation

uses Sub7.Core.Exceptions, Sub7.Core.Bundle, System.SysUtils;

{ TStringList_Serializer.FlatSerialize }

class function TStringList_Serializer.FlatSerialize(const AStringList : TStringList) : ISuperArray;
var I : Integer;
begin
  result := TSuperArray.Create();
  ///

  if not Assigned(AStringList) then
    Exit();

  for I := 0 to AStringList.count -1 do
    result.Add(AStringList.Strings[i]);
end;

{ TStringList_Serializer.Serialize }

class function TStringList_Serializer.Serialize(const AStringList : TStringList) : ISuperObject;
begin
  result := TSuperObject.Create();

  result.S['class_name'] := TStringList.ClassName;

  result.A['strings'] := TStringList_Serializer.FlatSerialize(AStringList);
end;

{ TStringList_Serializer.FlatDeSerialize }

class function TStringList_Serializer.FlatDeSerialize(const ASerializedArray : ISuperArray) : TStringList;
var I : Integer;
begin
  result := TStringList.Create();
  ///

  if not Assigned(ASerializedArray) then
    Exit();

  for I := 0 to ASerializedArray.Length -1 do
    result.Add(ASerializedArray.S[I]);
end;

{ TStringList_Serializer.DeSerialize }

class function TStringList_Serializer.DeSerialize(const ASerializedObject : ISuperObject) : TStringList;
begin
  result := nil;
  ///

  if not Assigned(ASerializedObject) then
    raise ES7DeserializationError.Create(ERR_MISSING_INSTANCE);

  if not ASerializedObject.Contains('class_name') or not ASerializedObject.Contains('strings') then
    raise ES7DeserializationError.Create(ERR_SERIALIZED_PROPERTIES);

  if String.Compare(ASerializedObject.S['class_name'], TStringList.Classname, True) <> 0 then
    raise ES7DeserializationError.Create(ERR_SERIALIZED_CLASS);

  ///
  result := TStringList_Serializer.FlatDeSerialize(ASerializedObject.A['strings']);
end;

end.
