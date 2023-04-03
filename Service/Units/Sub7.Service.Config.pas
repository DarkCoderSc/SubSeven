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

unit Sub7.Service.Config;

interface

uses System.Classes, Generics.Collections, Sub7.Core.OOP.Interfaces, XSuperObject;

type
  TAuthenticationMode = (
    amPassword,
    amPubKey,
    amBoth
  );

  TServerConfig = class(TInterfacedObject, IS7Serializable)
  private
    FBindAddress        : String;
    FBindPort           : Word;
    FConcurrency        : Boolean;
    FCertificate        : String;
    FAuthenticationMode : TAuthenticationMode;
    FAuthorizedKeys     : TList<String>;
    FPassword           : String;
  public
    {@C}
    constructor Create();
    destructor Destroy(); override;

    {@M}
    procedure Clear();
    function Serialize() : ISuperObject;
    procedure DeSerialize(const ASerializedObject : ISuperObject);

    function FingerprintExists(const AFingerprint : String) : Boolean;

    {@G}
    property BindAddress        : String              read FBindAddress;
    property BindPort           : Word                read FBindPort;
    property Concurrency        : Boolean             read FConcurrency;
    property Certificate        : String              read FCertificate;
    property AuthenticationMode : TAuthenticationMode read FAuthenticationMode;
    property AuthorizedKeys     : TList<String>       read FAuthorizedKeys;
    property Password           : String              read FPassword;
  end;

  TServiceConfig = class(TInterfacedObject, IS7Serializable)
  private
    FServerConfig : TServerConfig;
  public
    {@C}
    constructor Create();
    destructor Destroy(); override;

    {@M}
    function Serialize() : ISuperObject;
    procedure DeSerialize(const ASerializedObject : ISuperObject);

    procedure LoadFromFile(const AFileName : String);

    {@G}
    property ServerConfig : TServerConfig read FServerConfig;
  end;

  const ERR_MISSING_ARR  = '"%s" array is missing from serialized configuration. Please check documentation.';
        ERR_MISSING_NODE = '"%s" node is missing from serialized configuration. Please check documentation.';

implementation

uses System.SysUtils, Sub7.Core.Bundle, Sub7.Core.Exceptions, Sub7.Core.Input.Validators,
     Sub7.Core.Application.Env, Winapi.Windows, Sub7.Core.utils, Sub7.Core.FileSystem.Utils;

{-------------------------------------------------------------------------------
  @TServerConfig
-------------------------------------------------------------------------------}

{ TServerConfig.Create }

constructor TServerConfig.Create();
begin
  inherited Create();
  ///

  FAuthorizedKeys := TList<String>.Create();

  self.Clear();
end;

{ TServerConfig.Destroy }

destructor TServerConfig.Destroy();
begin
  if Assigned(FAuthorizedKeys) then
    FreeAndNil(FAuthorizedKeys);

  ///
  inherited Destroy();
end;

{ TServerConfig.Clear }

procedure TServerConfig.Clear();
begin
  FBindAddress        := '127.0.0.1';
  FBindPort           := 2801;
  FConcurrency        := True;
  FCertificate        := '';
  FAuthenticationMode := amPubKey;
  FPassword           := '';

  ///
  FAuthorizedKeys.Clear();
end;

{ TServerConfig.FingerprintExists }

function TServerConfig.FingerprintExists(const AFingerprint : String) : Boolean;
begin
  result := FAuthorizedKeys.Contains(AFingerprint);
end;

{ TServerConfig.Serialize }

function TServerConfig.Serialize() : ISuperObject;
begin
  /// TODO
end;

{ TServerConfig.DeSerialize }

procedure TServerConfig.DeSerialize(const ASerializedObject : ISuperObject);
var AString          : String;
    AInteger         : Integer;
    ANode            : ISuperObject;
    AArray           : ISuperArray;
    I                : Integer;
    AFingerprintNode : ISuperObject;
begin
  self.Clear();
  ///

  try
    // Bind Address
    if ASerializedObject.Contains('bind_address') then begin
      AString := ASerializedObject.S['bind_address'];

      if not IsValidIpAddress(AString) then
        raise ES7DeserializationError.Create(Format('"%s" is not a valid IP Address.', [AString]));

      self.FBindAddress := AString;
    end;

    // Bind Port
    if ASerializedObject.Contains('bind_port') then begin
      AInteger := ASerializedObject.I['bind_port'];

      if not IsValidPort(AInteger) then
        raise ES7DeserializationError.Create(Format('"%d" is not a valid IP/TCP Port.', [AInteger]));

      self.FBindPort := AInteger;
    end;

    // Concurrency
    if ASerializedObject.Contains('concurrency') then
      self.FConcurrency := ASerializedObject.B['concurrency'];


    // Certificate
    AString := '';
    if ASerializedObject.Contains('certificate') then begin
      AString := ASerializedObject.S['certificate'];

      if (not FileExists(AString)) and (AString <> '') then
        raise ES7DeserializationError.Create(Format(ERR_FILE_MISSING, [AString]))
    end;

    if AString = '' then
      AString := APP_ENV_ServerCertificateFile; // Use default PEM Certificate if present.

    self.FCertificate := AString;

    // Authentication Node
    if ASerializedObject.Contains('authentication') then begin
      ANode := ASerializedObject.O['authentication'];

      if ANode.Contains('method') then begin
        AString := ANode.S['method'];

        if String.Compare(AString, 'password', True) = 0 then
          self.FAuthenticationMode := amPassword
        else if String.Compare(AString, 'pubkey', True) = 0 then
          self.FAuthenticationMode := amPubKey
        else if String.Compare(AString, 'password-pubkey', True) = 0 then
          self.FAuthenticationMode := amBoth
        else
          raise ES7DeserializationError.Create(Format('"%s" is not a valid authentication mode.', [AString]));

        // For pubkey authentication
        if (FAuthenticationMode = amPubKey) or (FAuthenticationMode = amBoth) then begin
          if ANode.Contains('authorized_keys') then begin
            AArray := ANode.A['authorized_keys'];
            ///

            for I := 0 to AArray.Length -1 do begin
              try
                AFingerprintNode := AArray.O[I];
              except
                on E : Exception do
                  raise ES7DeserializationError.Create(Format('Fingerprint n°%d format is invalid in "authorized_keys" node.', [I+1]));
              end;

              if not AFingerprintNode.Contains('fingerprint') then
                raise ES7DeserializationError.Create(Format('Fingerprint attribute is missing from fingerprint node n°%d.', [I+1]));

              AString := AFingerprintNode.S['fingerprint'];

              if not IsValidSha512Fingerprint(AString) then
                raise ES7DeserializationError.Create(Format('"%s" is not a valid sha512 fingerprint.', [AString]));

              self.FAuthorizedKeys.Add(AString);
            end;
          end;
        end;

        // For Password Authentication
        if (FAuthenticationMode = amBoth) or (FAuthenticationMode = amPassword) then begin
          if not ANode.Contains('password') then
            raise Exception.Create('Password required when using password authentication.');

          AString := ANode.S['password'];

          if not IsValidSha512(AString) then
            raise ES7DeserializationError.Create(Format('"%s" is not a valid sha512 string.', [AString]));

          self.FPassword := AString;
        end;
      end;
    end;
  except
    on E : Exception do begin
      self.Clear();

      raise ES7DeserializationError.Create(Format('An error occured during config deserialization: %s', [E.Message]));
    end;
  end;
end;

{-------------------------------------------------------------------------------
  @TServiceConfig
-------------------------------------------------------------------------------}

{ TServiceConfig.Create }

constructor TServiceConfig.Create();
begin
  inherited Create();
  ///

  FServerConfig := TServerConfig.Create();
end;

{ TServiceConfig.Destroy }

destructor TServiceConfig.Destroy();
begin
  if Assigned(FServerConfig) then
    FreeAndNil(FServerConfig);

  ///
  inherited Destroy();
end;

{ TServiceConfig.Serialize }

function TServiceConfig.Serialize() : ISuperObject;
begin
  /// TODO
end;

{ TServiceConfig.DeSerialize }

procedure TServiceConfig.DeSerialize(const ASerializedObject : ISuperObject);
var AServer  : ISuperObject;
    AServers : ISuperArray;
    I        : Integer;
begin
  if not ASerializedObject.Contains('servers') then
    raise ES7DeserializationError.Create(Format(ERR_MISSING_ARR, ['servers']));
  ///

  AServers := ASerializedObject.A['servers'];

  for I := 0 to AServers.Length -1 do begin
    AServer := AServers.O[I];

    FServerConfig.Deserialize(AServer);

    break;
  end;
end;

{ TServiceConfig.LoadFromFile }

procedure TServiceConfig.LoadFromFile(const AFileName : String);
var AJson       : ISuperObject;
    AJsonString : String;
begin
  ReadTextFile(AFileName, AJsonString);

  AJson := TSuperObject.Create(AJsonString);

  self.DeSerialize(AJson);
end;

end.
