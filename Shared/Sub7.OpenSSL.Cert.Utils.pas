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

unit Sub7.OpenSSL.Cert.Utils;

interface

uses Sub7.OpenSSL.Headers;

type
  TX509Certificate = record
    pX509        : Pointer;
    pPrivKey     : Pointer;

    Fingerprint  : String;
  end;

  TExportKind = (
    ekPrivateKey,
    ekCertificate,
    ekBoth
  );

function OpenSSL_GeneratePrivateKey() : Pointer;
function OpenSSL_GenerateX509(pKey : Pointer) : Pointer;
procedure OpenSSL_GenerateSelfSignedCertificate(const AOutputFile : AnsiString);
procedure OpenSSL_ExportToFile(const ACert : TX509Certificate; const AOutputFile : AnsiString; const AExportKind : TExportKind);
procedure OpenSSL_LoadCertificate(const AInputFile : AnsiString; var ACert : TX509Certificate);
procedure OpenSSL_LoadCertificateInformation(var ACert : TX509Certificate);
procedure OpenSSL_CheckKeyPair(const ACertFile : String);

implementation

uses Sub7.OpenSSL.TLS.Exceptions, System.SysUtils, Winapi.Windows, Sub7.OpenSSL.TLS.Utils,
     Sub7.OpenSSL.TLS.Context;

{ _.OpenSSL_GeneratePrivateKey }

function OpenSSL_GeneratePrivateKey() : Pointer;
var pTempKey : Pointer;
    pRSA     : Pointer;
begin
  CheckOpenSSL();
  ///

  pTempKey := EVP_PKEY_new();
  if not Assigned(pTempKey) then
    raise EOpenSSLLibraryException.Create('Could not generate private key structure.');
  try
    pRSA := RSA_generate_key(4096, RSA_F4, nil, nil);
    if not Assigned(pRSA) then
      raise EOpenSSLLibraryException.Create('Could not generate private key.');

    if EVP_PKEY_assign(pTempKey, 6, pRSA) <> 1 then
      raise EOpenSSLLibraryException.Create('Could not assign private key.');

    ///
    result := pTempKey;
  except
    on E : Exception do begin
      EVP_PKEY_free(pTempKey);

      raise;
    end;
  end;
end;

{ _.OpenSSL_GenerateX509 }

function OpenSSL_GenerateX509(pKey : Pointer) : Pointer;
var pTempX509 : PX509;
    pX509Name : Pointer;
    ARet      : Integer;
begin
  if not Assigned(pKey) then
    raise Exception.Create('Missing private key structure to generate X509 Certificate.');

  pTempX509 := X509_new();
  if not Assigned(pTempX509) then
    raise EOpenSSLLibraryException.Create('Could not generate new certificate.');
  try
    if ASN1_INTEGER_set(X509_get_serialNumber(pTempX509), 1) <> 1 then
      raise EOpenSSLLibraryException.Create('Could not set certificate serial number.');

    X509_gmtime_adj(pTempX509.cert_info.validity.not_before, 0);
    X509_gmtime_adj(pTempX509.cert_info.validity.not_after, 31536000);

    X509_set_pubkey(pTempX509, pKey);

    pX509Name := X509_get_subject_name(pTempX509);

    X509_NAME_add_entry_by_txt(pX509Name, 'C',  MBSTRING_ASC, 'RO', -1, -1, 0);
    X509_NAME_add_entry_by_txt(pX509Name, 'O',  MBSTRING_ASC, 'SubSevenCrew', -1, -1, 0);
    X509_NAME_add_entry_by_txt(pX509Name, 'CN', MBSTRING_ASC, 'localhost', -1, -1, 0);

    X509_set_issuer_name(pTempX509, pX509Name);

    ARet := X509_sign(pTempX509, pKey, EVP_sha512);

    if ARet <= 0 then
      raise EOpenSSLLibraryException.Create('Could not sign certificate.');

    ///
    result := pTempX509;
  except
    on E : Exception do begin
      X509_free(pTempX509);

      raise;
    end;
  end;
end;

{ _.OpenSSL_ExportToFile }

procedure OpenSSL_ExportToFile(const ACert : TX509Certificate; const AOutputFile : AnsiString; const AExportKind : TExportKind);
var pBIO           : Pointer;
    AExportPrivKey : Boolean;
    AExportCert    : Boolean;
begin
  pBIO := BIO_new_file(PAnsiChar(AOutputFile), 'wb');

  if not Assigned(pBIO) then
    raise EOpenSSLLibraryException.Create('Could not create output file.');
  try
    AExportPrivKey := False;
    AExportCert    := False;

    case AExportKind of
      ekPrivateKey  : AExportPrivKey := True;
      ekCertificate : AExportCert    := True;

      ekBoth : begin
        AExportPrivKey := True;
        AExportCert    := True;
      end;
    end;

    if AExportPrivKey and Assigned(ACert.pPrivKey) then
      PEM_write_bio_PrivateKey(pBIO, ACert.pPrivKey, nil, nil, 0, 0, nil);

    if AExportCert and Assigned(ACert.pX509) then
      PEM_write_bio_X509(pBIO, ACert.pX509);
  finally
    BIO_free(pBIO);
  end;
end;


{ _.OpenSSL_GenerateSelfSignedCertificate }

procedure OpenSSL_GenerateSelfSignedCertificate(const AOutputFile : AnsiString);
var ACert : TX509Certificate;
begin
  ACert.pPrivKey := OpenSSL_GeneratePrivateKey();
  try
    ACert.pX509 := OpenSSL_GenerateX509(ACert.pPrivKey);
    try
      OpenSSL_ExportToFile(ACert, AOutputFile, ekBoth);
    finally
      X509_free(ACert.pX509);
    end;
  finally
    EVP_PKEY_free(ACert.pPrivKey);
  end;
end;

{ _.OpenSSL_LoadCertificate }

procedure OpenSSL_LoadCertificate(const AInputFile : AnsiString; var ACert : TX509Certificate);
var pBIO : Pointer;
begin
  ZeroMemory(@ACert, SizeOf(TX509Certificate));
  ///

  if not FileExists(AInputFile) then
    raise Exception.Create(Format('File "%s" does not exists.', [AInputFile]));

  pBIO := BIO_new_file(PAnsiChar(AInputFile), 'rb');
  try
    // First we read the private key
    ACert.pPrivKey := PEM_read_bio_PrivateKey(pBIO, nil, nil, nil);
    if not Assigned(ACert.pPrivKey) then
      raise EOpenSSLLibraryException.Create('Could not read Private Key from file.');

    // Then we read the X509 structure
    ACert.pX509 := PEM_read_bio_X509(pBIO, nil, nil, nil);
    if not Assigned(ACert.pX509) then
      raise EOpenSSLLibraryException.Create('Could not read X509 structure from file.');

    // Retrieve Information about the certificate
    OpenSSL_LoadCertificateInformation(ACert);
  finally
    BIO_free(pBIO);
  end;
end;

{ _.OpenSSL_ReadCertificateInformation }

procedure OpenSSL_LoadCertificateInformation(var ACert : TX509Certificate);
begin
  if not Assigned(ACert.pX509) or
     not Assigned(ACert.pPrivKey) then
    raise Exception.Create('X509 structure and Private Key must be present in certificate record.');
  ///

  ACert.Fingerprint := X509_SHA512FingerPrint(ACert.pX509);
end;

{ _.OpenSSL_CheckKeyPair }

procedure OpenSSL_CheckKeyPair(const ACertFile : String);
var AContext : TOpenSSLContext;
begin
  AContext := TOpenSSLContext.Create(cmClient, ACertFile);
  try
    ///
  finally
    if Assigned(AContext) then
      FreeAndNil(AContext);
  end;
end;

end.
