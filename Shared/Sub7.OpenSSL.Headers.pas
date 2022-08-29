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

unit Sub7.OpenSSL.Headers;

interface

uses Winapi.Windows;

var hLibCrypto : THandle = 0;
    hLibSSL    : THandle = 0;

const LIB_CRYPTO_DLL = {$IFDEF WIN64}'libcrypto-1_1-x64.dll'{$ELSE}'libcrypto-1_1.dll'{$IFEND};
      LIB_SSL_DLL    = {$IFDEF WIN64}'libssl-1_1-x64.dll'{$ELSE}'libssl-1_1.dll'{$IFEND};

type
  cuint = longword;

  {$IFDEF WIN64}
    culong = Int64;
    clong  = Int64;
  {$ELSE}
    culong = Cardinal;
    clong  = Longint;
  {$IFEND}

  TSSL_SHA512 = record
    Length : cuint;
    Sha512 : array [0..64-1] of AnsiChar;
  end;

  TX509Validity = record
    not_before : Pointer;
    not_after  : Pointer;
  end;
  PX509Validity = ^TX509Validity;

  ASN1_STRING = record
    length : Integer;
    _type  : Integer;
    data   : PAnsiChar;
    flags  : ulong;
  end;

   X509_ALGOR = record
    algorithm : Pointer;
    parameter : Pointer;
  end;

  TX509CertInfo = record
    version       : Pointer;
    serialNumber  : ASN1_STRING;
    signature     : X509_ALGOR;
    issuer        : Pointer;
    validity      : TX509Validity;

    // ... //
  end;
  PX509CertInfo = ^TX509CertInfo;

  TX509 = record
    cert_info : TX509CertInfo;
  end;
  PX509 = ^TX509;

const SSL_FILETYPE_PEM                = 1;
      SSL_ERROR_WANT_READ             = 2;
      SSL_ERROR_WANT_WRITE            = 3;
      SSL_ERROR_SSL                   = 1;
      SSL_ERROR_NONE                  = 0;
      SSL_VERIFY_PEER                 = $01;
      SSL_VERIFY_FAIL_IF_NO_PEER_CERT = $02;
      RSA_F4                          = $10001;
      MBSTRING_ASC                    = $1000 or 1;

    {
      libssl.dll
    }
    SSL_CTX_new                   : function(pMethod: Pointer): Pointer cdecl = nil;
    SSL_CTX_use_certificate_file  : function(pContext: Pointer; const ACertificateFile: PAnsiChar; ACertificateType: Integer): Integer cdecl = nil;
    SSL_CTX_use_PrivateKey_file   : function(pContext: Pointer; const APrivateKeyFile: PAnsiChar; ACertificateType: Integer): Integer cdecl = nil;
    SSL_CTX_check_private_key     : function(pContext: Pointer): Integer cdecl = nil;
    SSL_new                       : function(pContext: Pointer): Pointer cdecl = nil;
    SSL_CTX_free                  : procedure(pContext: Pointer) cdecl = nil;
    SSL_set_fd                    : function(pSSL: Pointer; ASocketFd: Integer): Integer cdecl = nil;
    SSL_accept                    : function(pSSL: Pointer): Integer cdecl = nil;
    SSL_read                      : function(pSSL: Pointer; pBuffer: Pointer; ABufferLength: Integer): Integer cdecl = nil;
    SSL_peek                      : function(pSSL: Pointer; pBuffer: Pointer; ABufferLength: Integer): Integer cdecl = nil;
    SSL_write                     : function(pSSL: Pointer; const pBuffer: Pointer; ABufferLength: Integer): Integer cdecl = nil;
    SSL_free                      : procedure(pSSL: Pointer); cdecl = nil;
    SSL_CTX_set_ciphersuites      : function(_para1: Pointer; const AString: PAnsiChar): Integer cdecl = nil;
    SSL_connect                   : function(pSSL: Pointer): Integer cdecl = nil;
    SSL_get_error                 : function(pSSL: Pointer; AReturnCode: Integer): Integer cdecl = nil;
    TLS_server_method             : function(): Pointer cdecl = nil;
    TLS_client_method             : function(): Pointer cdecl = nil;
    SSL_has_pending               : function(pSSL : Pointer): Integer cdecl = nil;
    SSL_get_peer_certificate      : function(pSSL: Pointer): Pointer cdecl = nil;
    SSL_CTX_set_verify            : procedure(pContext: Pointer; AMode: Integer; pCallback: Pointer) cdecl = nil;

    {
      libcrypto.dll
    }
    ERR_error_string_n            : function(e: ULONG; buf: PAnsiChar; len : size_t): PAnsiChar cdecl = nil;
    ERR_get_error                 : function(): ULONG cdecl = nil;
    X509_NAME_oneline             : function(a: Pointer; buf: PAnsiChar; size: Integer): PAnsiChar cdecl = nil;
    EVP_add_cipher                : function(const cipher: Pointer): Integer; cdecl = nil;
    EVP_aes_256_gcm               : function(): Pointer; cdecl = nil;
    EVP_add_digest                : function(const digest: Pointer): Integer; cdecl = nil;
    EVP_sha512                    : function(): Pointer cdecl = nil;
    EVP_sha384                    : function(): Pointer; cdecl = nil;
    EVP_PKEY_new                  : function(): Pointer cdecl = nil;
    RSA_generate_key              : function(bits: Integer; e: culong; callback: Pointer; cb_arg: Pointer): Pointer cdecl = nil;
    EVP_PKEY_free                 : procedure(pkey: Pointer) cdecl = nil;
    EVP_PKEY_assign               : function(pkey: Pointer; _type: Integer; key: PAnsiChar): Integer cdecl = nil;
    X509_new                      : function(): Pointer cdecl = nil;
    ASN1_INTEGER_set              : function(a: Pointer; v: clong): Integer cdecl = nil;
    X509_get_serialNumber         : function(x: Pointer): Pointer cdecl = nil;
    X509_gmtime_adj               : function(s: Pointer; adj: clong): Pointer cdecl = nil;
    X509_set_pubkey               : function(x: Pointer; pkey: Pointer): Integer cdecl = nil;
    X509_get_subject_name         : function(a: Pointer): Pointer cdecl = nil;
    X509_NAME_add_entry_by_txt    : function(name: Pointer; const field: PAnsiChar; _type: Integer; const bytes: PAnsiChar; len, loc, _set: Integer): Integer cdecl = nil;
    X509_set_issuer_name          : function(x: Pointer; name: Pointer): Integer cdecl = nil;
    X509_sign                     : function(x: Pointer; pkey: Pointer; const md: Pointer): Integer cdecl = nil;
    X509_free                     : procedure(x: Pointer) cdecl = nil;
    BIO_new_file                  : function(const filename: PAnsiChar; const mode: PAnsiChar): Pointer cdecl = nil;
    BIO_free                      : function(pBIO: Pointer): Integer cdecl = nil;
    PEM_write_bio_X509            : function(pBIO: Pointer; pX509: Pointer): Integer cdecl = nil;
    PEM_write_bio_PrivateKey      : function(pBIO : Pointer; pKey : Pointer; const enc : Pointer; kstr :PAnsiChar; klen : Integer; cb : Pointer; u : Pointer) : Integer cdecl = nil;
    PEM_read_bio_X509             : function(pBIO: Pointer; pX509: Pointer; cb: Pointer; u: Pointer): Pointer cdecl = nil;
    PEM_read_bio_PrivateKey       : function(pBIO : Pointer; pKey : Pointer; cb : Pointer; u : Pointer) : Pointer cdecl = nil;
    X509_digest                   : function(const data: PX509; const _type: Pointer; md: PByte; var len: cuint): Integer cdecl = nil;

    {
      Export
    }
    procedure LoadOpenSSL();
    procedure CheckOpenSSL();
    procedure FreeOpenSSL();

implementation

uses System.SysUtils;

{ _.LoadOpenSSL }

procedure LoadOpenSSL();
begin
  hLibCrypto := LoadLibrary(LIB_CRYPTO_DLL);
  hLibSSL    := LoadLibrary(LIB_SSL_DLL);

  if (hLibCrypto = 0) then
    raise Exception.Create(Format('Could not load "%s" library.', [LIB_CRYPTO_DLL]));

  if (hLibSSL = 0) then
    raise Exception.Create(Format('Could not load "%s" library.', [LIB_SSL_DLL]));

  { libssl.dll }

  @SSL_CTX_new                   := GetProcAddress(hLibSSL, 'SSL_CTX_new');
  @SSL_CTX_use_certificate_file  := GetProcAddress(hLibSSL, 'SSL_CTX_use_certificate_file');
  @SSL_CTX_use_PrivateKey_file   := GetProcAddress(hLibSSL, 'SSL_CTX_use_PrivateKey_file');
  @SSL_CTX_check_private_key     := GetProcAddress(hLibSSL, 'SSL_CTX_check_private_key');
  @SSL_new                       := GetProcAddress(hLibSSL, 'SSL_new');
  @SSL_CTX_free                  := GetProcAddress(hLibSSL, 'SSL_CTX_free');
  @SSL_set_fd                    := GetProcAddress(hLibSSL, 'SSL_set_fd');
  @SSL_accept                    := GetProcAddress(hLibSSL, 'SSL_accept');
  @SSL_read                      := GetProcAddress(hLibSSL, 'SSL_read');
  @SSL_write                     := GetProcAddress(hLibSSL, 'SSL_write');
  @SSL_CTX_set_ciphersuites      := GetProcAddress(hLibSSL, 'SSL_CTX_set_ciphersuites');
  @SSL_free                      := GetProcAddress(hLibSSL, 'SSL_free');
  @SSL_connect                   := GetProcAddress(hLibSSL, 'SSL_connect');
  @SSL_get_error                 := GetProcAddress(hLibSSL, 'SSL_get_error');
  @TLS_server_method             := GetProcAddress(hLibSSL, 'TLS_server_method');
  @TLS_client_method             := GetProcAddress(hLibSSL, 'TLS_client_method');
  @SSL_has_pending               := GetProcAddress(hLibSSL, 'SSL_has_pending');
  @SSL_peek                      := GetProcAddress(hLibSSL, 'SSL_peek');
  @SSL_get_peer_certificate      := GetProcAddress(hLibSSL, 'SSL_get_peer_certificate');
  @SSL_CTX_set_verify            := GetProcAddress(hLibSSL, 'SSL_CTX_set_verify');

  { libcrypto.dll }

  @ERR_error_string_n         := GetProcAddress(hLibCrypto, 'ERR_error_string_n');
  @ERR_get_error              := GetProcAddress(hLibCrypto, 'ERR_get_error');
  @X509_NAME_oneline          := GetProcAddress(hLibCrypto, 'X509_NAME_oneline');
  @EVP_add_cipher             := GetProcAddress(hLibCrypto, 'EVP_add_cipher');
  @EVP_aes_256_gcm            := GetProcAddress(hLibCrypto, 'EVP_aes_256_gcm');
  @EVP_add_digest             := GetProcAddress(hLibCrypto, 'EVP_add_digest');
  @EVP_sha384                 := GetProcAddress(hLibCrypto, 'EVP_sha384');
  @EVP_sha512                 := GetProcAddress(hLibCrypto, 'EVP_sha512');
  @X509_digest                := GetProcAddress(hLibCrypto, 'X509_digest');
  @EVP_PKEY_new               := GetProcAddress(hLibCrypto, 'EVP_PKEY_new');
  @RSA_generate_key           := GetProcAddress(hLibCrypto, 'RSA_generate_key');
  @EVP_PKEY_free              := GetProcAddress(hLibCrypto, 'EVP_PKEY_free');
  @EVP_PKEY_assign            := GetProcAddress(hLibCrypto, 'EVP_PKEY_assign');
  @X509_new                   := GetProcAddress(hLibCrypto, 'X509_new');
  @ASN1_INTEGER_set           := GetProcAddress(hLibCrypto, 'ASN1_INTEGER_set');
  @X509_get_serialNumber      := GetProcAddress(hLibCrypto, 'X509_get_serialNumber');
  @X509_gmtime_adj            := GetProcAddress(hLibCrypto, 'X509_gmtime_adj');
  @X509_set_pubkey            := GetProcAddress(hLibCrypto, 'X509_set_pubkey');
  @X509_get_subject_name      := GetProcAddress(hLibCrypto, 'X509_get_subject_name');
  @X509_NAME_add_entry_by_txt := GetProcAddress(hLibCrypto, 'X509_NAME_add_entry_by_txt');
  @X509_set_issuer_name       := GetProcAddress(hLibCrypto, 'X509_set_issuer_name');
  @X509_sign                  := GetProcAddress(hLibCrypto, 'X509_sign');
  @X509_free                  := GetProcAddress(hLibCrypto, 'X509_free');
  @BIO_new_file               := GetProcAddress(hLibCrypto, 'BIO_new_file');
  @BIO_free                   := GetProcAddress(hLibCrypto, 'BIO_free');
  @PEM_write_bio_X509         := GetProcAddress(hLibCrypto, 'PEM_write_bio_X509');
  @PEM_write_bio_PrivateKey   := GetProcAddress(hLibCrypto, 'PEM_write_bio_PrivateKey');
  @PEM_read_bio_X509          := GetProcAddress(hLibCrypto, 'PEM_read_bio_X509');
  @PEM_read_bio_PrivateKey    := GetProcAddress(hLibCrypto, 'PEM_read_bio_PrivateKey');

  ///
  CheckOpenSSL();
end;

{ _.CheckOpenSSL }

procedure CheckOpenSSL();
var AResult : Boolean;
begin
  AResult := Assigned(SSL_CTX_new)                       and
             Assigned(SSL_CTX_use_certificate_file)      and
             Assigned(SSL_CTX_use_PrivateKey_file)       and
             Assigned(SSL_CTX_check_private_key)         and
             Assigned(SSL_new)                           and
             Assigned(SSL_CTX_free)                      and
             Assigned(SSL_set_fd)                        and
             Assigned(SSL_accept)                        and
             Assigned(SSL_read)                          and
             Assigned(SSL_CTX_set_ciphersuites)          and
             Assigned(SSL_free)                          and
             Assigned(SSL_write)                         and
             Assigned(ERR_error_string_n)                and
             Assigned(ERR_get_error)                     and
             Assigned(X509_NAME_oneline)                 and
             Assigned(SSL_connect)                       and
             Assigned(SSL_get_error)                     and
             Assigned(EVP_add_cipher)                    and
             Assigned(EVP_aes_256_gcm)                   and
             Assigned(EVP_add_digest)                    and
             Assigned(EVP_sha384)                        and
             Assigned(TLS_server_method)                 and
             Assigned(TLS_client_method)                 and
             Assigned(SSL_has_pending)                   and
             Assigned(SSL_peek)                          and
             Assigned(SSL_get_peer_certificate)          and
             Assigned(EVP_sha512)                        and
             Assigned(X509_digest)                       and
             Assigned(SSL_CTX_set_verify)                and
             Assigned(EVP_PKEY_new)                      and
             Assigned(RSA_generate_key)                  and
             Assigned(EVP_PKEY_free)                     and
             Assigned(EVP_PKEY_assign)                   and
             Assigned(X509_new)                          and
             Assigned(ASN1_INTEGER_set)                  and
             Assigned(X509_get_serialNumber)             and
             Assigned(X509_gmtime_adj)                   and
             Assigned(X509_set_pubkey)                   and
             Assigned(X509_get_subject_name)             and
             Assigned(X509_NAME_add_entry_by_txt)        and
             Assigned(X509_set_issuer_name)              and
             Assigned(X509_sign)                         and
             Assigned(X509_free)                         and
             Assigned(BIO_new_file)                      and
             Assigned(BIO_free)                          and
             Assigned(PEM_write_bio_X509)                and
             Assigned(PEM_write_bio_PrivateKey)          and
             Assigned(PEM_read_bio_X509)                 and
             Assigned(PEM_read_bio_PrivateKey)
;

  if not AResult then
    raise Exception.Create('Some required OpenSSL API''s are missing from current loaded libraries.' +
                           'Please use the correct libraries versions or reinstall the application.');
end;

{ _.FreeOpenSSL }

procedure FreeOpenSSL();
begin
  if hLibSSL > 0 then
    FreeLibrary(hLibSSL);

  if hLibCrypto > 0 then
    FreeLibrary(hLibCrypto);

  ///
  hLibSSL    := 0;
  hLibCrypto := 0;
end;

initialization
  {}

finalization
  FreeOpenSSL();

end.
