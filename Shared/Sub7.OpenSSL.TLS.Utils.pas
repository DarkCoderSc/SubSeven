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

unit Sub7.OpenSSL.TLS.Utils;

interface

uses Sub7.OpenSSL.Headers;

function Peer_SHA512FingerPrint(pSSL : Pointer) : String;
function X509_SHA512FingerPrint(pX509 : Pointer) : String;
function PadFingerPrint(const AFingerprint : String; const APadLength : Cardinal) : String;

implementation

uses Winapi.Windows, Sub7.OpenSSL.TLS.Exceptions, System.SysUtils,
     System.Classes;

{ PadFingerPrint

  Adjust number of columns for the fingerprint. Useful to display on small width
  controls }

function PadFingerPrint(const AFingerprint : String; const APadLength : Cardinal) : String;
var AStringList : TStringList;
    I, N        : Integer;
begin
  result := '';
  ///

  AStringList := TStringList.Create();
  try
    AStringList.Delimiter       := ':';
    AStringList.DelimitedText   := AFingerprint;
    AStringList.StrictDelimiter := True;

    N := 0;
    for I := 0 to AStringList.Count -1 do begin
      result := result + AStringList.Strings[I] + ':';

      Inc(N);

      if N >= APadLength then begin
        Delete(result, Length(result), 1);

        if I <> AStringList.Count -1 then
          result := result + #13#10;

        N := 0;
      end;
    end;
  finally
    if Assigned(AStringList) then
      FreeAndNil(AStringList);
  end;
end;

{ Peer_SHA512FingerPrint

  Retrieve X509 Certificate of peer then call "X509_SHA512FingerPrint" to
  retrieve its fingerprint }

function Peer_SHA512FingerPrint(pSSL : Pointer) : String;
var pX509 : Pointer;
begin
  pX509 := SSL_get_peer_certificate(pSSL);

  if not Assigned(pX509) then
    raise EOpenSSLLibraryException.Create('Could not retrieve peer X509 certificate.');

  result := X509_SHA512FingerPrint(pX509);
end;

{ X509_SHA512FingerPrint

  Retrieve X509 Certificate SHA512 signature }

function X509_SHA512FingerPrint(pX509 : Pointer) : String;
var ASha512 : TSSL_SHA512;
    I       : Integer;
begin
  ZeroMemory(@ASha512, SizeOf(TSSL_SHA512));
  ///

  if X509_digest(pX509, EVP_sha512(), PByte(@ASha512.Sha512), ASha512.Length) <> 1 then
    raise EOpenSSLLibraryException.Create('Could not retrieve X509 certificate digest.');

  ///

  result := '';
  for i := 0 to ASha512.Length -1 do begin
    if i <> 0 then
      result := result + ':';

    result := result + Format('%.2x', [Byte(ASha512.Sha512[i])]);
  end;
end;

end.
