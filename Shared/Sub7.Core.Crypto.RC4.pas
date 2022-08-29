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

unit Sub7.Core.Crypto.RC4;

interface

uses
  Classes, SysUtils, Sub7.Core.Crypto.CRC32, WinAPI.Windows;

type
  TReason = (
              rSuccess,
              rCipherError,
              rWrongPassphrase
  );

  T256    = array[0..256-1] of byte;
  TKey    = array of byte;
  PKey    = ^TKey;

  TRC4 = class
  private
    FPassphrase : String;
    FKeyPtr     : PKey;
    FArrS       : T256;
    FKeySize    : Integer;
    FPRGA_i     : Integer;
    FPRGA_j     : Integer;

    {@M}
    procedure KSA();
    function PRGA() : Byte;
    procedure InitializeRC4();

    procedure InitializeKey();
    procedure FreeKey();

    function GetKeySize() : Byte;
  public
    {@C}
    constructor Create(APassphrase : String);
    destructor Destroy(); override;

    {@M}
    function RC4(pBuffer : Pointer; ABufferSize : Cardinal) : Boolean;

    // #Memory
    function Encrypt(pBuffer : Pointer; ABufferSize : Cardinal) : Boolean; overload;
    function Encrypt(pBuffer : Pointer; ABufferSize : Cardinal; var ASignature : Cardinal) : Boolean; overload;
    function Decrypt(pBuffer : Pointer; ABufferSize : Cardinal; ASignature : Cardinal = 0) : TReason; overload;

    // #Str
    function Encrypt(AString : String; ASignOutput : Boolean = False) : String; overload;
    function Decrypt(AHexString : String) : String; overload;

    procedure UpdatePassphrase(APassphrase : String);

    {@G}
    property KeySize    : Byte   read GetKeySize;
    property Passphrase : String read FPassphrase;
  end;

  {@Exports}
  function MemoryToHexString(pBuffer : Pointer; ABufferSize : Cardinal) : String;
  function HexStringToMemory(const AString : String; var outBuffer : Pointer; var outBufferSize : Cardinal) : Boolean;
  function TryStrToCardinal(const AValue : String; out AResult : Cardinal): boolean;

implementation

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


  Local Functions


+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

function MemoryToHexString(pBuffer : Pointer; ABufferSize : Cardinal) : String;
var I              : Cardinal;
    AStringBuilder : TStringBuilder;
begin
  result := '';
  ///

  AStringBuilder := TStringBuilder.Create((ABufferSize * 2)); // x2 because of string hex representation
  try
    for I := 0 to ABufferSize -1 do begin
      AStringBuilder.Append(IntToHex(PByte(NativeUInt(pBuffer) + I)^));
    end;
  finally
    result := AStringBuilder.ToString();
    ///

    if Assigned(AStringBuilder) then
      FreeAndNil(AStringBuilder);
  end;
end;

function HexStringToMemory(const AString : String; var outBuffer : Pointer; var outBufferSize : Cardinal) : Boolean;
var I      : Cardinal;
    AChunk : String;
begin
  result := False;
  ///

  outBufferSize := (Length(AString) div 2); // We are working with Ansi Strings

  GetMem(outBuffer, outBufferSize);
  try
    ZeroMemory(outBuffer, outBufferSize);
    ///

    for I := 0 to outBufferSize -1 do begin
      AChunk := Copy(AString, (I * 2) +1, 2);

      PByte(NativeUInt(outBuffer) + I)^ := StrToInt('$' + AChunk);
    end;

    result := (outBufferSize > 0);
  except
    FreeMem(outBuffer, outBufferSize);

    outBufferSize := 0;
  end;
end;

function TryStrToCardinal(const AValue : String; out AResult : Cardinal): boolean;
var AInteger64: Int64;
begin
  Result := false;
  ///

  if not TryStrToInt64(AValue, AInteger64) then
    Exit;

  if (AInteger64 < Low(AResult)) or (AInteger64 > High(AResult)) then
    Exit;

  Result := true;

  AResult := AInteger64;
end;

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


  TRC4


+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

{-------------------------------------------------------------------------------
  ___constructor
-------------------------------------------------------------------------------}
constructor TRC4.Create(APassphrase : String);
begin
  if (Length(Trim(APassphrase)) = 0) then
     Exit();
  ///

  if Length(APassphrase) > 256 then
    FPassphrase := Copy(APassphrase, 0, 255) // We are limited to 2048 bits
  else
    FPassphrase := APassphrase;

  ///
  InitializeKey();
end;

{-------------------------------------------------------------------------------
  ___destructor
-------------------------------------------------------------------------------}
destructor TRC4.Destroy();
begin
  FreeKey();

  FillChar(FArrS, Length(FArrS), #0);

  ///
  inherited Destroy();
end;

{-------------------------------------------------------------------------------
 Finalize / Free Key
-------------------------------------------------------------------------------}
procedure TRC4.FreeKey();
begin
  if (Assigned(FKeyPtr)) and (FKeySize > 0) then begin
    FillChar(FKeyPtr, FKeySize, #0); // Securely erase key from memory.

    FreeMem(FKeyPtr, FKeySize); // Memory region is available.

    ///
    FKeyPtr  := nil;
    FKeySize := 0;
  end;
end;

{-------------------------------------------------------------------------------
 Initialize Key
-------------------------------------------------------------------------------}
procedure TRC4.InitializeKey();
var i : Integer;
begin
  self.FreeKey();
  ///

  FKeySize := (Length(FPassphrase) * SizeOf(Byte));

  FKeyPtr := GetMemory(FKeySize);

  for i := 0 to FKeySize -1 do begin
      TKey(FKeyPtr)[i] := ord(FPassphrase[i + 1]);
  end;

  ///
  FillChar(FPassphrase[1], (Length(FPassphrase) * SizeOf(Char)), 0); // Secure
end;

{-------------------------------------------------------------------------------
 Requires to key to be initialized
-------------------------------------------------------------------------------}
procedure TRC4.UpdatePassphrase(APassphrase : String);
begin
  FPassphrase := APassphrase;

  self.InitializeKey();
end;

{-------------------------------------------------------------------------------
 Initialize RC4 Array (S)
-------------------------------------------------------------------------------}
procedure TRC4.InitializeRC4();
var i : Integer;
begin
  FillChar(FArrS, Length(FArrS), #0);
  ///

  for i := 0 to length(FArrS) -1 do begin
     FArrS[i] := i;
  end;
end;

{-------------------------------------------------------------------------------
  Initialize Key (KSA)
-------------------------------------------------------------------------------}
procedure TRC4.KSA();
var i, j  : Integer;
    AByte : Byte;
begin
  j := 0;
  for i := 0 to length(FArrS) -1 do begin
     j := (j + FArrS[i] + TKey(FKeyPtr)[i mod FKeySize]) mod 256;

     AByte    := FArrS[i];
     FArrS[i] := FArrS[j];
     FArrS[j] := AByte;
  end;
end;

{-------------------------------------------------------------------------------
  Pseudo Random Generation Algotithm (PRGA)
-------------------------------------------------------------------------------}
function TRC4.PRGA() : Byte;
var AByte : Byte;
begin
  FPRGA_i := (FPRGA_i + 1) mod 256;
  FPRGA_j := (FPRGA_j + FArrS[FPRGA_i]) mod 256;

  AByte          := FArrS[FPRGA_i];
  FArrS[FPRGA_i] := FArrS[FPRGA_j];
  FArrS[FPRGA_j] := AByte;

  ///
  result := FArrS[(FArrS[FPRGA_i] + FArrS[FPRGA_j]) mod 256];
end;

{-------------------------------------------------------------------------------
  Encrypt's / Decrypt memory region.
-------------------------------------------------------------------------------}
function TRC4.RC4(pBuffer : Pointer; ABufferSize : Cardinal) : Boolean;
var APRGA : Byte;
    i     : NativeUInt;
begin
  result := False;
  ///

  if NOT Assigned(pBuffer) then
    Exit();

  if (ABufferSize = 0) then
    Exit();

  if (NOT Assigned(FKeyPtr)) or (FKeySize = 0) then
    Exit();
  ///

  FPRGA_i := 0;
  FPRGA_j := 0;

  {
    Prepare RC4 Cipher
  }
  InitializeRC4();

  self.KSA();

  {
    Encrypt / Decrypt Routine
  }
  for i := 0 to ABufferSize -1 do begin
      APRGA := self.PRGA();
      ///

      PByte(NativeUInt(pBuffer) + i)^ := APRGA xor PByte(NativeUInt(pBuffer) + i)^;
  end;

  ///
  result := True;
end;

{-------------------------------------------------------------------------------
  Encrypt / Decrypt (Support Signature - CRC32)
-------------------------------------------------------------------------------}

function TRC4.Encrypt(pBuffer : Pointer; ABufferSize : Cardinal) : Boolean;
begin
  result := self.RC4(pBuffer, ABufferSize);
end;

function TRC4.Encrypt(pBuffer : Pointer; ABufferSize : Cardinal; var ASignature : Cardinal) : Boolean;
begin
  ASignature := CRC32(pBuffer, ABufferSize);

  result := self.Encrypt(pBuffer, ABufferSize);
end;

function TRC4.Decrypt(pBuffer : Pointer; ABufferSize : Cardinal; ASignature : Cardinal = 0) : TReason;
var b : Boolean;
begin
  b := self.RC4(pBuffer, ABufferSize);

  if b then begin
    if (ASignature > 0) then begin
      if (CRC32(pBuffer, ABufferSize) = ASignature) then
        result := rSuccess
      else
        result := rWrongPassphrase;
    end else
      result := rSuccess; // Bypass Signature Check
  end else
    result := rCipherError;
end;

{-------------------------------------------------------------------------------

  Encrypt / Decrypt Plaint Text

  Hints:
  -------------
    If Signature is used, Encrypted Output / Input will look as follows:
      [Encrypted Hex String]:[Signature_Cardinal]

-------------------------------------------------------------------------------}

function TRC4.Encrypt(AString : String; ASignOutput : Boolean = False) : String;
var pCopy      : PWideChar;
    ASize      : Cardinal;
    ASignature : Cardinal;
    AEncoded   : String;
    b          : Boolean;
begin
  result := '';
  ///

  ASize := Length(AString) * SizeOf(WideChar);
  ///

  GetMem(pCopy, ASize);
  try
    MoveMemory(pCopy, PWideChar(AString), ASize);
    ///

    ASignature := 0;

    if ASignOutput then
      b := self.Encrypt(pCopy, ASize, ASignature)
    else
      b := self.Encrypt(pCopy, ASize);

    if b then begin
      AEncoded := MemoryToHexString(pCopy, ASize);

      if (Length(AEncoded) > 0) then begin
        if ASignOutput then
          result := Format('%s:%s', [AEncoded, IntToStr(ASignature)])
        else
          result := AEncoded;
      end;
    end;
  finally
    FreeMem(pCopy, ASize);
  end;
end;

function TRC4.Decrypt(AHexString : String) : String;
var ASignature  : Cardinal;
    AMessage    : String;
    APos        : Cardinal;
    pBuffer     : Pointer;
    ABufferSize : Cardinal;
begin
  result := '';
  ///

  APos := Pos(':', AHexString);

  ASignature := 0;

  if (APos > 0) then begin
    {
      Check Signatures
    }
    AMessage := Copy(AHexString, 1, APos-1);
    Delete(AHexString, 1, APos);

    if NOT TryStrToCardinal(AHexString, ASignature) then
      Exit();

    if (ASignature = 0) then
      Exit();
  end else
    AMessage := AHexString;
  ///

  if NOT HexStringToMemory(AMessage, pBuffer, ABufferSize) then
    Exit();
  try
    if (self.Decrypt(pBuffer, ABufferSize, ASignature) = rSuccess) then begin
      SetString(result, PWideChar(pBuffer), (ABufferSize div 2));
    end;
  finally
    FreeMem(pBuffer, ABufferSize);
  end;
end;

{-------------------------------------------------------------------------------
  Getters / Setters
-------------------------------------------------------------------------------}

function TRC4.GetKeySize() : Byte;
begin
  result := (FKeySize * 8);
end;

end.

