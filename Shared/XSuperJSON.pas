 (*
  *                       XSuperObject - Simple JSON Framework
  *
  * The MIT License (MIT)
  * Copyright (c) 2015 Onur YILDIZ
  *
  *
  * Permission is hereby granted, free of charge, to any person
  * obtaining a copy of this software and associated documentation
  * files (the "Software"), to deal in the Software without restriction,
  * including without limitation the rights to use, copy, modify,
  * merge, publish, distribute, sublicense, and/or sell copies of the Software,
  * and to permit persons to whom the Software is furnished to do so,
  * subject to the following conditions:
  *
  * The above copyright notice and this permission notice shall
  * be included in all copies or substantial portions of the Software.
  *
  * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
  * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
  * OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
  * IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
  * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
  * TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH
  * THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
  *
  *)

unit XSuperJSON;

interface

uses
  System.SysUtils, System.Classes, Generics.Collections, Generics.Defaults,
  System.Math, System.DateUtils, System.RegularExpressions, System.RTTI;


const
  CNull = 'null';
  MaxCHR = #127;

  Err_UnexpectedEndOfInput = 'Unexpected end of input';
  Err_Expected = 'Expected %s';
  Err_ExpectedButFound = '"%s" expected but "%s" found';
  Err_UnexpectedTokenILLEGAL = 'Unexpected token ILLEGAL';

type

  // ## Forward Declarations
  // -----------------------

  TJSONNull = class;
  TLexGenerator = class;
  TRoute = class;
  PPosition = ^TPosition;
  TPosition = record
    Col: Integer;
    Line: Integer;
  end;

  TDataType = (dtNil, dtNull, dtObject, dtArray, dtString, dtInteger, dtFloat, dtBoolean, dtDateTime, dtDate, dtTime);
  TJSONComparison<T> = reference to function(Left, Right: T): Integer;
  // ## Exception

  TJSONSyntaxError = class(Exception)
  public
    constructor Create(const Msg: String; Pos: PPosition);
    constructor CreateFmt(const Msg: String; const Args: array of TVarRec; Pos: PPosition);
  end;

  // ## JSONWriter
  TJSONWriter = class
  public const
    IDENT_SIZE = 2;
  private
    FData: TStringBuilder;
    FIdent: Boolean;
    FIdentOffset: Integer;
    FUniversalTime: Boolean;
  public
    constructor Create(const useIdent, useUniversalTime: Boolean);
    destructor Destroy; override;
    procedure Inc;
    procedure Dec;

    function Append(const Value: string; const CRLF: Boolean = False): TJSONWriter; overload;
    function Append(const Value: int64; const CRLF: Boolean = False): TJSONWriter; overload;
    function AppendVal(const Value: string; const CRLF: Boolean = False): TJSONWriter; overload;
    function AppendVal(const Value: int64; const CRLF: Boolean = False): TJSONWriter; overload;
    function ToString: string; override;

    property Ident: Boolean read FIdent;
    property UniversalTime: Boolean read FUniversalTime;
  end;


  // ## JSON Symbols
  // ---------------

  IJSONAncestor = interface
  ['{FFB71762-50A1-4D27-9F59-56F6208421C7}']
    function GetAsVariant: Variant;
    procedure SetAsVariant(const Value: Variant);
    function GetDataType: TDataType;
    function GetIsNull: Boolean;
    procedure AsJSONString(Str: TJSONWriter);
    property IsNull: Boolean read GetIsNull;
    property DataType: TDataType read GetDataType;
    property AsVariant: Variant read GetAsVariant write SetAsVariant;
  end;

  TJSONAncestor = class abstract(TInterfacedObject, IJSONAncestor)
  private
    function GetAsVariant: Variant;
    procedure SetAsVariant(const Value: Variant);
  protected
    function GetDataType: TDataType; virtual;
    function GetIsNull: Boolean; virtual;
  public
    procedure AsJSONString(Str: TJSONWriter); virtual;
    property IsNull: Boolean read GetIsNull;
    property DataType: TDataType read GetDataType;
    property AsVariant: Variant read GetAsVariant write SetAsVariant;
  end;

  IJSONValue<T> = interface(IJSONAncestor)
  ['{0B1ED53C-EF62-4BFA-9E78-9DD9088D96C5}']
    function GetData: T;
    procedure SetData(const Value: T);
    procedure SetNull;
    property Value: T read GetData write SetData;
  end;

  TJSONValue<T> = class abstract(TJSONAncestor, IJSONValue<T>)
  public
    FNull: Boolean;
    FData: T;
  protected
    function GetData: T; virtual;
    procedure SetData(const Value: T); virtual;
    function GetIsNull: Boolean; override;
    property Value: T read GetData write SetData;
  public
    constructor Create(const Value: T);
    constructor CreateNull;
    procedure SetNull;
  end;

  IJSONNull = interface(IJSONValue<Boolean>)['{C19F5715-B832-46D8-8668-1A9DC31393D7}']end;
  TJSONNull = class(TJSONValue<Boolean>, IJSONNull)
  public
    procedure AsJSONString(Str: TJSONWriter); override;
  protected
    function GetIsNull: Boolean; override;
  end;

  IJSONBoolean = interface(IJSONValue<Boolean>)['{CCC8D8C5-081D-4DCF-93DB-CC0696458A12}']end;
  TJSONBoolean = class(TJSONValue<Boolean>, IJSONBoolean)
  public
    procedure AsJSONString(Str: TJSONWriter); override;
    property Value;
  end;

  IJSONString = interface(IJSONValue<String>)['{C507BB41-3674-4F47-8D6B-5605258F6A2F}']end;
  TJSONString = class(TJSONValue<String>, IJSONString)
  public
    procedure AsJSONString(Str: TJSONWriter); override;
    property Value;
  end;

  IJSONRaw = interface(IJSONString)['{EF5EF422-1A81-49EA-A3E0-9E7D5B5CC1E2}']end;
  TJSONRaw = class(TJSONString, IJSONRaw)
  public
    procedure AsJSONString(Str: TJSONWriter); override;
    property Value;
  end;

  IJSONInteger = interface(IJSONValue<Int64>)['{E9D84348-9634-40F5-8A1F-FF006F45FC6D}']end;
  TJSONInteger = class(TJSONValue<Int64>, IJSONInteger)
  public
    procedure AsJSONString(Str: TJSONWriter); override;
    property Value;
  end;

  IJSONFloat = interface(IJSONValue<Double>)['{29D840FB-191B-4304-9518-C2937B3AE6B0}']end;
  TJSONFloat = class(TJSONValue<Double>, IJSONFloat)
  public
    procedure AsJSONString(Str: TJSONWriter); override;
    property Value;
  end;

  IJSONBaseDate<T> = interface(IJSONValue<T>)
  ['{7ACB3D47-A9A6-49C1-AFF3-F451368EAE48}']
    function GetAsString: String;
    property AsString: String read GetAsString;
  end;

  TJSONBaseDate<T> = class(TJSONValue<T>, IJSONBaseDate<T>)
  protected
    FFormat: String;
  public
    function GetAsString: String;
    procedure AsJSONString(Str: TJSONWriter); override;
  end;

  IJSONDateTime = interface(IJSONBaseDate<TDateTime>)['{9441CA2E-B822-4C13-ABF0-15F8026CCE50}']end;
  TJSONDateTime = class(TJSONBaseDate<TDateTime>, IJSONDateTime)
  public
    constructor Create(const Value: TDateTime; const Format: String = 'yyyy-mm-dd"T"hh":"mm":"ss.zzz');
    property Value;
  end;

  IJSONDate = interface(IJSONBaseDate<TDate>)['{A862D6A5-2C4A-41CD-B2C0-F7B58FA14066}']end;
  TJSONDate = class(TJSONBaseDate<TDate>, IJSONDate)
  public
    constructor Create(const Value: TDate; const Format: String = 'yyyy-mm-dd');
    property Value;
  end;

  IJSONTime = interface(IJSONBaseDate<TTime>)['{EEBCD145-B837-4129-A21D-378DF7DA53B2}']end;
  TJSONTime = class(TJSONBaseDate<TTime>, IJSONTime)
  public
    constructor Create(const Value: TTime; const Format: String = 'hh":"mm":"ss.zzz');
    property Value;
  end;


  TJSONDateTimeCheckCallBack = reference to function(Str: String; var Value: TDateTime; var Typ: TDataType): Boolean;
  TJSONDateManager = class
  private
    class var FFormats: TList<TJSONDateTimeCheckCallBack>;
    class function GetFormats: TList<TJSONDateTimeCheckCallBack>; static; inline;
  public
    class constructor Create;
    class destructor Destroy;
    class function Check(const Data: String; var AValue: TDateTime; var Typ: TDataType): Boolean;
    class property Formats: TList<TJSONDateTimeCheckCallBack> read GetFormats;
  end;

  IJSONPair = interface
  ['{D328943F-5ED1-4B35-8332-573156565C96}']
    function GetName: String;
    function GetValue: IJSONAncestor;
    procedure SetName(const Value: String);
    procedure SetValue(const Value: IJSONAncestor);
    property Name: String read GetName write SetName;
    property JSONValue: IJSONAncestor read GetValue write SetValue;
  end;

  TJSONPair = class(TInterfacedObject, IJSONPair)
  private
    FName: String;
    FValue: IJSONAncestor;
    function GetName: String;
    function GetValue: IJSONAncestor;
    procedure SetName(const Value: String);
    procedure SetValue(const Value: IJSONAncestor);
  public
    constructor Create(const aName: String; aValue: IJSONAncestor);
    destructor Destroy; override;
    property Name: String read GetName write SetName;
    property JSONValue: IJSONAncestor read GetValue write SetValue;
  end;

  TJSONEnumerator<T> = record
    Index : Integer;
    List : TList<T>;
    function MoveNext : Boolean;
    function GetCurrent : T;
    property Current : T read GetCurrent;
  end;

  IJSONObject = Interface(IJSONValue<IJSONPair>)
  ['{2A9244EC-F202-4CC1-9F89-7DA12437F7ED}']
    function Count: Integer;
    function Get(const Name: String): IJSONPair; overload;
    function Get(const Index: Integer): IJSONPair; overload;
    procedure AddPair(P: IJSONPair); overload;
    procedure AddPair(Name: String; Value: IJSONAncestor); overload;
    procedure Remove(P: IJSONPair); overload;
    procedure Remove(const Name: String); overload;
    procedure Remove(const Index: Integer); overload;
    function GetEnumerator: TJSONEnumerator<IJSONPair>;
    procedure Sort(Comparison: TJSONComparison<IJSONPair>);
  end;


  TJSONObject = class(TJSONValue<IJSONPair>, IJSONObject)
  private
    FPairList: TList<IJSONPair>;
    FNull: Boolean;
  protected
    function GetIsNull: Boolean; override;
  public
    constructor Create;
    destructor Destroy; override;
    function Count: Integer;
    function Get(const Name: String): IJSONPair; overload;
    function Get(const Index: Integer): IJSONPair; overload;
    procedure AsJSONString(Str: TJSONWriter); override;
    procedure AddPair(P: IJSONPair); overload;
    procedure AddPair(Name: String; Value: IJSONAncestor); overload; inline;
    procedure Remove(P: IJSONPair); overload; inline;
    procedure Remove(const Name: String); overload;
    procedure Remove(const Index: Integer); overload;
    function GetEnumerator: TJSONEnumerator<IJSONPair>;
    procedure Sort(Comparison: TJSONComparison<IJSONPair>);
    class function ParseJSONValue(const Str: String; const CheckDate: Boolean): IJSONAncestor;
  end;

  IJSONArray = interface(IJSONValue<IJSONAncestor>)
  ['{C63B4323-6D7E-4151-BA1B-4C55CDE28FDB}']
    procedure Add(Val: IJSONAncestor);
    procedure Remove(Val: IJSONAncestor); overload;
    procedure Remove(Index: Integer); overload;
    procedure Clear;
    function Count: Integer;
    function Get(const I: Integer): IJSONAncestor;
    procedure SetIndex(const Int: Integer; const Value: IJSONAncestor);
    function GetEnumerator: TJSONEnumerator<IJSONAncestor>;
    procedure Sort(Comparison: TJSONComparison<IJSONAncestor>);
    property Index[const Int: Integer]: IJSONAncestor read Get write SetIndex; default;
  end;

  TJSONArray = class(TJSONValue<IJSONAncestor>, IJSONArray)
  private
    FList: TList<IJSONAncestor>;
    FNull: Boolean;
    procedure SetIndex(const Int: Integer; const Value: IJSONAncestor);
  protected
    function GetIsNull: Boolean; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AsJSONString(Str: TJSONWriter); override;
    procedure Add(Val: IJSONAncestor);
    procedure Remove(Val: IJSONAncestor); overload;
    procedure Remove(Index: Integer); overload;
    procedure Clear;
    function Count: Integer;
    function Get(const I: Integer): IJSONAncestor;
    function GetEnumerator: TJSONEnumerator<IJSONAncestor>;
    procedure Sort(Comparison: TJSONComparison<IJSONAncestor>);
    property Index[const Int: Integer]: IJSONAncestor read Get write SetIndex; default;
  end;


  TJSONBuilder = class
  private
    LGen: TLexGenerator;
    FCheckDates: Boolean;
  public
    constructor Create(const JSON: String; const CheckDates: Boolean);
    destructor Destroy; override;
    function  ReadValue: IJSONAncestor;
    procedure ReadString(var Val: IJSONAncestor);
    procedure ReadInteger(var Val: IJSONAncestor);
    procedure ReadFloat(var Val: IJSONAncestor);
    procedure ReadObject(var Val: IJSONAncestor);
    procedure ReadTrue(var Val: IJSONAncestor);
    procedure ReadFalse(var Val: IJSONAncestor);
    procedure ReadNull(var Val: IJSONAncestor);
    procedure ReadArray(var Val: IJSONAncestor);
  end;

  TJSONInterpreter = class
  private
    LGen: TLexGenerator;
    FJSON: IJSONAncestor;
    FExceptionBlock: Boolean;
    function ReadName(Base: IJSONAncestor): IJSONAncestor;
    function ReadArrayIndex(Base: IJSONArray): IJSONAncestor;
    function ReadObject(Base: IJSONAncestor): IJSONObject;
    function ReadArray(Base: IJSONAncestor): IJSONArray;
    function ReadValue(Base: IJSONAncestor): IJSONAncestor;
    procedure CreateExcept(const S: String; Args: array of TVarRec); overload;
    procedure CreateExcept(const S: String); overload; inline;
  public
    constructor Create(const Expression: String; JSON: IJSONAncestor; BlockException: Boolean = False);
    destructor Destroy; override;
    function ReadExpression: IJSONAncestor;
  end;


  // ## Parse
  // --------

  TLexemType = ( ltNil,
                 ltSValue, ltIValue, ltDValue, ltNull, ltCLeft, ltCRight,
                 ltBLeft, ltBRight, ltBSlash, ltColon, ltDot, ltVirgule,
                 ltName,
                 ltTrue,
                 ltFalse );

  TLexemTypes = set of TLexemType;

  TLexBuff = class
  public
    Capacity: Integer;
    Length : Integer;
    Buff: PWideChar;
    constructor Create;
    destructor Destroy; override;
    function AsString: String; inline;
    function AsInt64: Int64;
    function AsDouble: Double;
    function AsType: TLexemType;
    function AsHInt: Int64;
    procedure Add(Ch: WideChar); inline;
    procedure Grow;
    procedure Clear; inline;
  end;

  ILexeme = ^TLexeme;
  TLexeme = record
    Pos: TPosition;
    Int: Int64;
    Str: String;
    Dbl: Double;
    LType: TLexemType;
  end;

  TParseProc = (ppNil, ppInteger, ppDouble, ppString, ppName, ppEscape, ppEscapeUChar);

  TTriggerProcs = set of (ttBuffer, ttEnd, ttBack);

  TTrigger = class
  public
    TriggerProcs: TTriggerProcs;
    ParseProcs: TParseProc;
    NextRoute: TRoute;
    BF: Boolean;
    ED: Boolean;
    BK: Boolean;
    constructor Create(NextRoute: TRoute; TriggerProcs: TTriggerProcs; ParseProcs: TParseProc);
  end;

  TErrorTrigger = class(TTrigger)
  private
    FMessage: String;
    function GetMeessage: String;
    procedure SetMessage(const Value: String);
  public
    constructor Create(const Message: String);
    property Message: String read GetMeessage write SetMessage;
  end;


  TNoRouteTrigger = class(TTrigger)
  end;

  TUseRouteTrigger = class(TTrigger)
  end;

  TJumpTrigger = class(TTrigger)
  end;

  {$WARNINGS OFF}
  TRouteChars = set of Char;
  {$WARNINGS ON}

  TRoute = class
  private
    FName: String;
    FTriggers: array[#0..MaxCHR] of TTrigger;
    FTriggerList: TObjectList<TTrigger>;
    function GetIndex(Ch: WideChar): TTrigger; inline;
    function GetName: String;
  public
    constructor Create(const Name: String);
    destructor Destroy; override;
    property Name: String read GetName;
    procedure Add(const Chars: TRouteChars; Trigger: TTrigger);
    procedure NoRoute(Trigger: TTrigger);
    function TryGetRoute(Ch: WideChar; var Trg: TTrigger): Boolean; inline;
    property Index[Ch: WideChar]: TTrigger read GetIndex; default;
  end;

  TLexGrammar = class
  private
     FRoutes: TList<TRoute>;
  protected
     function FirstRoute: TRoute; virtual; abstract;
     function CreateRoute(const Name: String): TRoute;
     function EscapeSupport: Boolean; virtual;
     function EscapeRoute: TRoute; virtual;
  public
     constructor Create; virtual;
     destructor Destroy; override;
  end;

  TJSONGrammar = class(TLexGrammar)
  private
     rFirst,
     rName,
     rString,
     rString2,
     rInt,
     rDouble,
     rExp, rExpE,
     rExpPM,

     rEscape,
     rEscapeRoute,
     rEscapeUChar: TRoute;

  protected
     function FirstRoute: TRoute; override;
     function EscapeSupport: Boolean; override;
     function EscapeRoute: TRoute; override;
  public
     constructor Create; override;
     destructor Destroy; override;
  end;

  TLexGenerator = class
  private
    FFirstRoute: TRoute;
    FBuffer: TLexBuff;
    FEscapeBuff: TLexBuff;
    FCurr: PWideChar;
    FCurrPos: PPosition;
    FLexem: ILexeme;
    FLexG: TLexGrammar;
    FEscapeSupport: Boolean;
    FEscapeRoute: TRoute;
    FExceptBlock: Boolean;
    procedure CreateLexeme;
    procedure NextLex;
    procedure KillLex; inline;
  public
    constructor Create(LexG: TLexGrammar = nil; ExceptBlock: Boolean = False);
    destructor Destroy; override;
    procedure Load(const Source: String);
    function  Check(LTyp: TLexemType): Boolean; overload;
    function  Check(LTyp: TLexemTypes): TLexemType; overload;
    function  CheckName(var S: String): Boolean;
    function  CheckKill(LTyp: TLexemType): Boolean; overload;
    function  CheckKill(LTyp: TLexemTypes): TLexemType; overload;
    function  Current: ILexeme; inline;
    property CurrPos: PPosition read FCurrPos;
  end;

  TSuperParser = class
  public
    class function ParseJSON(const S: String; const CheckDateTime: Boolean): IJSONAncestor;
  end;

  TISO8601 = record
  private
    FData: TMatch;
    FSuccess: Boolean;
    FOffset: Integer;
    FUseTime: Boolean;
    FUseDate: Boolean;
    FValue: TDateTime;
    FValueType: TDataType;
    function NextOffset: Integer;
    function GetIntData(const Index: Integer): Integer; overload; inline;
    function GetIntData(const Index: Integer; const P: Boolean): Integer; overload;
    function GetStrData(const Index: Integer): String; inline;
    procedure ReadStructure;
    procedure ReadZulu;
    function ReadDate: Boolean;
    function ReadTime: Boolean;
    procedure ReadMS;
    procedure ReadTZ(const P: Boolean);
  public
    constructor Create(const Value: String);
    property Value: TDateTime read FValue;
    property ValueType: TDataType read FValueType;
    property Success: Boolean read FSuccess;
  end;

  function LimitedStrToUTF16(const Str: String): String;

implementation

uses
  XSuperObject;

const
  FloatFormat : TFormatSettings = ( DecimalSeparator : '.' );
  STokenTypes : array [TLexemType] of string = ('Nil',
                'String', 'Integer', 'Float', 'Null', '[', ']',
                '(', ')', '\', ':', '.', ',',
                '',
                'TRUE',
                'FALSE' );


  optAll = [#0..#255];
  optWhiteSpace = [' ', #0, #9, #10, #13];

  optAlpha = ['a'..'z', 'A'..'Z', '$', '_', #127];
  optSym = ['[', ']', '{', '}', ':', ',', '"', '''', '.'];
  optNumeric = ['0'..'9'];
  optEscape = ['b', 'f', 'n', 'r', 't', 'v', '''', '"', '\'];
  optEscapeUnicode = ['u'];
  optHex = ['A'..'F', 'a'..'f'] + optNumeric;
  optStop = optWhiteSpace + optSym;

  HexMap : array [0..15] of WideChar = ('0', '1', '2', '3', '4', '5', '6',
           '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f');

var
  JSONLexGrammar: TJSONGrammar;
  function iff(const Bool: Boolean; _true, _false: Variant): Variant; inline;
  begin
    if Bool then
       Result := _true
    else
       Result := _false;
  end;

  function ChrToUTF16(const ChrCode: Integer): String; inline;
  begin
     Result := '\u' +
               HexMap[ChrCode shr 12] +
               HexMap[(ChrCode shr 8) and 15] +
               HexMap[(ChrCode shr 4) and 15] +
               HexMap[ChrCode and 15];
  end;

  function StrToUTF16(const Str: String): String;
  var
    Tmp: PWideChar;
  begin
    if Str = #0 then Exit(ChrToUtf16(0));
    Result := '';
    if Str = '' then
       Exit
    else
       Tmp := PWideChar(Pointer(Str));
    while Tmp^ <> #0 do
    begin
      case Tmp^ of
        #1..#31: case Tmp^ of
                  #8 : Result := Result + '\b';
                  #9 : Result := Result + '\t';
                  #10: Result := Result + '\n';
                  //#11: Result := Result + '\v';
                  #12: Result := Result + '\f';
                  #13: Result := Result + '\r';
               else
                  Result := Result + ChrtoUTF16(Ord(Tmp^))
               end;
        #34{"}: Result := Result + '\"';
        #92{\}: Result := Result + '\\';
        //#127..#65535: Result := Result + ChrtoUTF16(Ord(Tmp^));
      else
        Result := Result + Tmp^;
      end;
      Inc(Tmp);
    end;
  end;

  function LimitedStrToUTF16(const Str: String): String;
  var
    Tmp: PWideChar;
  begin
    if Str = #0 then Exit(ChrToUtf16(0));
    Result := '';
    if Str = '' then
       Exit
    else
       Tmp := PWideChar(Pointer(Str));
    while Tmp^ <> #0 do
    begin
      case Tmp^ of
        #1..#31: case Tmp^ of
                  #8 : Result := Result + '\b';
                  #9 : Result := Result + '\t';
                  #10: Result := Result + '\n';
                  //#11: Result := Result + '\v';
                  #12: Result := Result + '\f';
                  #13: Result := Result + '\r';
               else
                  Result := Result + ChrtoUTF16(Ord(Tmp^))
               end;
      else
        Result := Result + Tmp^;
      end;
      Inc(Tmp);
    end;
  end;

{ TJSONAncestor }

procedure TJSONAncestor.AsJSONString(Str: TJSONWriter);
begin
  Str.Append('');
end;

function TJSONAncestor.GetDataType: TDataType;
begin
  with TCast.Create(Self) do
  begin
     Result := DataType;
     Free;
  end;
end;

function TJSONAncestor.GetIsNull: Boolean;
begin
  Result := Self is TJSONNull;
end;

function TJSONAncestor.GetAsVariant: Variant;
begin
  with TCast.Create(Self) do
  begin
     Result := AsVariant;
     Free;
  end;
end;

procedure TJSONAncestor.SetAsVariant(const Value: Variant);
begin
  with TCast.Create(Self) do
  begin
     AsVariant := Value;
     Free;
  end;
end;

{ TLexBuff }

procedure TLexBuff.Add(Ch: WideChar);
begin
  if Capacity = 0 then Exit;
  if (Length >=  Capacity - Length) then Grow;
  Buff[Length] := Ch;
  Inc(Length);
  Buff[Length] := #0;
end;

function TLexBuff.AsDouble: Double;
var
  Res: Extended;
begin
  Add(#0);
  {$WARNINGS OFF}
  if not TextToFloat(PWideChar(@Buff[0]), Res, fvExtended, FloatFormat)  then
  {$WARNINGS ON}
     raise EConvertError.Create('')
  else
     Result := Res;
end;

function TLexBuff.AsHInt: Int64;
var
  I, J: Integer;
begin
  I := 0;
  Result := 0;
  while I < Length do
  begin
    J := Ord(Buff[I]);
    Inc(I);
    case J of
       Ord('a')..Ord('f') :
          J := J - (Ord('a') - 10);
       Ord('A')..Ord('F') :
          J := J - (Ord('A') - 10);
       Ord('0')..Ord('9') :
          J := J - Ord('0');
    else
       Continue;
    end;
    Result := (Result shl 4) or J;
  end;
end;

function TLexBuff.AsInt64: Int64;
begin
  Result := StrToInt64(AsString);
end;

function TLexBuff.AsString: String;
begin
  SetString(Result, Buff, Length);
end;

function TLexBuff.AsType: TLexemType;
begin
  Result := ltName;
   if Length = 0 then
      Exit;

   case Buff[0] of
      '[': Result := ltCLeft;
      ']': Result := ltCRight;
      ':': Result := ltColon;
      ',': Result := ltVirgule;
      '{': Result := ltBLeft;
      '}': Result := ltBRight;
      '.': Result := ltDot;
   else
      if CompareText(STokenTypes[ltTrue], AsString) = 0 then
         Result := ltTrue
      else
      if CompareText(STokenTypes[ltFalse], AsString) = 0 then
         Result := ltFalse
      else
      if CompareText(STokenTypes[ltNull], AsString) = 0 then
         Result := ltNull
   end;
end;

procedure TLexBuff.Clear;
begin
  Length := 0;
  Buff[0] := #0;
end;

constructor TLexBuff.Create;
begin
  inherited;
  Length := 0;
  Capacity := 32;
  GetMem(Buff, Capacity * SizeOf(PWideChar));
end;

destructor TLexBuff.Destroy;
begin
  if Assigned(Buff) then
     FreeMem(Buff);
  inherited;
end;

procedure TLexBuff.Grow;
begin
   Capacity := System.Math.Max(Capacity * 2, Length + 8);
   ReallocMem(Buff, Capacity * SizeOf(WideChar));
end;

{ TSuperParser }

class function TSuperParser.ParseJSON(const S: String; const CheckDateTime: Boolean): IJSONAncestor;
var
  JSON: TJSONBuilder;
begin
  JSON := TJSONBuilder.Create(S, CheckDateTime);
  try
    Result := JSON.ReadValue;
  finally
    if Assigned(JSON) then
       JSON.Free;
  end;
end;

{ TTrigger }

{ TTrigger }

constructor TTrigger.Create(NextRoute: TRoute; TriggerProcs: TTriggerProcs;
  ParseProcs: TParseProc);
begin
  Self.NextRoute := NextRoute;
  Self.ParseProcs := ParseProcs;
  Self.TriggerProcs := TriggerProcs;
  BF := ttBuffer in TriggerProcs;
  ED := ttEnd in TriggerProcs;
  BK := ttBack in TriggerProcs;
end;

{ TRoute }

procedure TRoute.Add(const Chars: TRouteChars; Trigger: TTrigger);
var
  Ch: WideChar;
begin
  Ch := #0;

  if not FTriggerList.Contains(Trigger) then
     FTriggerList.Add(Trigger);

  while Ch <= MaxCHR do
  begin
     {$WARNINGS OFF}
       if Ch in Chars then {$WARNINGS ON}
         if not Assigned(FTriggers[Ch]) then
            FTriggers[Ch] := Trigger;
     Inc(Ch);
  end;
end;

constructor TRoute.Create(const Name: String);
begin
  FName := Name;
  FTriggerList := TObjectList<TTrigger>.Create;
end;


destructor TRoute.Destroy;
begin
  FTriggerList.Free;
  inherited;
end;

function TRoute.GetIndex(Ch: WideChar): TTrigger;
begin
  if Ch > MaxCHR then Ch := MaxCHR;
  Result := FTriggers[Ch];
end;

function TRoute.GetName: String;
begin
  Result := FName;
end;

procedure TRoute.NoRoute(Trigger: TTrigger);
var
  Ch: WideChar;
begin
  Ch := #0;

  if not FTriggerList.Contains(Trigger) then
     FTriggerList.Add(Trigger);

  while Ch <= MaxCHR do
  begin
     if not Assigned(FTriggers[Ch]) then
        FTriggers[Ch] := Trigger;
     Inc(Ch);
  end;
end;

function TRoute.TryGetRoute(Ch: WideChar; var Trg: TTrigger): Boolean;
begin
  if Ch > MaxCHR then Ch := MaxCHR;
  if FTriggers[Ch] <> nil then
  begin
     Result := True;
     Trg := FTriggers[Ch];
  end
  else
     Result := False;
end;

{ TLexGrammar }

constructor TLexGrammar.Create;
begin
  FRoutes := TList<TRoute>.Create;
end;

destructor TLexGrammar.Destroy;
begin
  FRoutes.Free;
  inherited;
end;

function TLexGrammar.EscapeRoute: TRoute;
begin
  Result := Nil;
end;

function TLexGrammar.EscapeSupport: Boolean;
begin
  Result := False;
end;

function TLexGrammar.CreateRoute(const Name: String): TRoute;
begin
  Result := TRoute.Create(Name);
  FRoutes.Add(Result);
end;

{ TJSONGrammar }

constructor TJSONGrammar.Create;
begin
  inherited;

  rFirst := CreateRoute('First');
  rName := CreateRoute('Name');
  rString := CreateRoute('String');
  rString2 := CreateRoute('String2');
  rInt := CreateRoute('Int');
  rDouble := CreateRoute('Double');

  rExp := CreateRoute('Exp');
  rExpE := CreateRoute('ExpE');
  rExpPM := CreateRoute('ExpPM');

  rEscape := CreateRoute('Escape');
  rEscapeRoute := CreateRoute('EscapeRoute');
  rEscapeUChar := CreateRoute('EscapeUChar');


  rEscape.Add( ['\'], TJumpTrigger.Create(rEscapeRoute, [], ppNil ));

  rEscapeRoute.Add(['u'], TJumpTrigger.Create(rEscapeUChar, [ttBuffer], ppNil));
  rEscapeRoute.NoRoute(TUseRouteTrigger.Create(rEscape, [ttBuffer, ttEnd], ppEscape));

  rEscapeUChar.Add(optHex, TUseRouteTrigger.Create(rEscapeUChar, [], ppEscapeUChar));
  rEscapeUChar.NoRoute(TErrorTrigger.Create(ERR_UnexpectedTokenILLEGAL));

  rFirst.Add(optSym - ['"', ''''], TUseRouteTrigger.Create(rFirst, [ttBuffer, ttEnd], ppName));
  rFirst.Add(optAlpha, TUseRouteTrigger.Create(rName, [ttBuffer], ppNil));
  rFirst.Add(['"'], TJumpTrigger.Create(rString, [ttBuffer], ppNil));
  rFirst.Add([''''], TJumpTrigger.Create(rString2, [ttBuffer], ppNil));
  rFirst.Add(optNumeric, TUseRouteTrigger.Create(rInt, [ttBuffer], ppNil));
  rFirst.Add(optWhiteSpace - [#0], TJumpTrigger.Create(rFirst, [], ppNil));
  rFirst.Add(['-'], TUseRouteTrigger.Create(rInt, [ttBuffer], ppNil));
  rFirst.NoRoute(TErrorTrigger.Create(ERR_UnexpectedTokenILLEGAL));

  rName.Add(optAll - optWhiteSpace - optSym, TUseRouteTrigger.Create(rName, [], ppNil));
  rName.Add(optWhiteSpace, TJumpTrigger.Create(rFirst, [ttEnd, ttBack], ppName));
  rName.NoRoute(TJumpTrigger.Create(rFirst, [ttEnd, ttBack], ppName));

  rString.Add(optAll - ['"', #0, #10, #13], TUseRouteTrigger.Create(rString, [], ppNil));
  rString.Add(['"'], TJumpTrigger.Create(rFirst, [ttEnd], ppString));
  rString.Add([#0, #10, #13], TErrorTrigger.Create(ERR_UnexpectedTokenILLEGAL));

  rString2.Add(optAll - ['''', #0, #10, #13], TUseRouteTrigger.Create(rString2, [], ppNil));
  rString2.Add([''''], TJumpTrigger.Create(rFirst, [ttEnd], ppString));
  rString2.Add([#0, #10, #13], TErrorTrigger.Create(ERR_UnexpectedTokenILLEGAL));

  rInt.Add(optNumeric, TUseRouteTrigger.Create(rInt, [], ppNil));
  rInt.Add(['.'], TUseRouteTrigger.Create(rDouble, [], ppNil));
  rInt.Add(['e', 'E'], TUseRouteTrigger.Create(rExp, [], ppNil));
  rInt.Add(optStop, TJumpTrigger.Create(rFirst, [ttEnd, ttBack], ppInteger));
  rInt.NoRoute(TErrorTrigger.Create(ERR_UnexpectedTokenILLEGAL));

  rDouble.Add(optNumeric, TUseRouteTrigger.Create(rDouble, [], ppNil));
  rDouble.Add(['e', 'E'], TUseRouteTrigger.Create(rExp, [], ppNil));
  rDouble.Add(optStop, TJumpTrigger.Create(rFirst, [ttEnd, ttBack], ppDouble));
  rDouble.NoRoute(TErrorTrigger.Create(ERR_UnexpectedTokenILLEGAL));

  rExp.Add(['+', '-'], TUseRouteTrigger.Create(rExpPM, [], ppNil));
  rExp.Add(optNumeric, TUseRouteTrigger.Create(rExpE, [], ppNil));
  rExp.NoRoute(TErrorTrigger.Create(Err_UnexpectedTokenILLEGAL));

  rExpPM.Add(optNumeric, TUseRouteTrigger.Create(rExpE, [], ppNil));
  rExpPM.NoRoute(TErrorTrigger.Create(Err_UnexpectedTokenILLEGAL));

  rExpE.Add(optNumeric, TUseRouteTrigger.Create(rExpE, [], ppNil));
  rExpE.Add(optStop, TJumpTrigger.Create(rFirst, [ttEnd, ttBack], ppDouble));
  rExpE.NoRoute(TErrorTrigger.Create(Err_UnexpectedTokenILLEGAL));

end;

destructor TJSONGrammar.Destroy;
begin
  rFirst.Free;
  rName.Free;
  rString.Free;
  rString2.Free;
  rInt.Free;
  rDouble.Free;
  rExp.Free;
  rExpE.Free;
  rExpPM.Free;
  rEscape.Free;
  rEscapeRoute.Free;
  rEscapeUChar.Free;
  inherited;
end;

function TJSONGrammar.EscapeRoute: TRoute;
begin
  Result := rEscape;
end;

function TJSONGrammar.EscapeSupport: Boolean;
begin
  Result := True;
end;

function TJSONGrammar.FirstRoute: TRoute;
begin
  Result := rFirst;
end;

{ TErrorTrigger }

constructor TErrorTrigger.Create(const Message: String);
begin
  inherited Create(Nil, [], ppNil);
  FMessage := Message;
end;

function TErrorTrigger.GetMeessage: String;
begin
  Result := FMessage;
end;

procedure TErrorTrigger.SetMessage(const Value: String);
begin
  FMessage := Value;
end;

{ TLexGenerator }

function TLexGenerator.Check(LTyp: TLexemTypes): TLexemType;
begin
  if not Assigned(FLexem) then
  begin
     NextLex;
     if not Assigned(FLexem) then
        Exit(ltNil);
  end;
  Result := iff(FLexem.LType in LTyp, FLexem.LType, ltNil);
end;

function TLexGenerator.Check(LTyp: TLexemType): Boolean;
begin
  if not Assigned(FLexem) then
  begin
     NextLex;
     if not Assigned(FLexem) then
        Exit(False);
  end;
  Result := FLexem.LType = LTyp;
end;

function TLexGenerator.CheckKill(LTyp: TLexemType): Boolean;
begin
  if not Assigned(FLexem) then
  begin
     NextLex;
     if not Assigned(FLexem) then
        Exit(False);
  end;
  if FLexem.LType = LTyp then
  begin
     KillLex;
     Result := True;
  end
  else
     Result := False;
end;

function TLexGenerator.CheckKill(LTyp: TLexemTypes): TLexemType;
begin
  if not Assigned(FLexem) then
  begin
     NextLex;
     if not Assigned(FLexem) then
        Exit(ltNil);
  end;
  if FLexem.LType in LTyp then
  begin
     Result := FLexem.LType;
     KillLex;
  end
  else
     Result := ltNil;
end;

function TLexGenerator.CheckName(var S: String): Boolean;
var
  lt: TLexemType;
begin
  lt := Check([ltSValue, ltName, ltDValue, ltIValue, ltTrue, ltFalse]);
  if lt in [ltSValue, ltName, ltTrue, ltFalse] then
  begin
     if (Pos(#$D, FLexem.Str) > 0) or (Pos(#$A, FLexem.Str)>0) then
         Exit(False);
     Result := True;
     S := FLexem.Str;
  end
  else
     Result := False;
end;

constructor TLexGenerator.Create(LexG: TLexGrammar; ExceptBlock: Boolean);
begin
  FExceptBlock := ExceptBlock;
  if not Assigned(LexG) then
     FLexG := JSONLexGrammar
  else
     FLexG := LexG;
  FFirstRoute := LexG.FirstRoute;
  FBuffer := TLexBuff.Create;
  FEscapeSupport := LexG.EscapeSupport;
  if FEscapeSupport then
  begin
     FEscapeRoute := LexG.EscapeRoute;
     FEscapeBuff := TLexBuff.Create;
  end;
  New(FCurrPos);
  FCurrPos.Line := 1;
  FCurrPos.Col := 0;
end;

procedure TLexGenerator.CreateLexeme;
begin
  KillLex;
  New(FLexem);
  FillChar(FLexem.Pos, SizeOf(TPosition), 0);
  FLexem.LType := ltNull;
end;

function TLexGenerator.Current: ILexeme;
begin
  Result := FLexem;
end;

destructor TLexGenerator.Destroy;
begin
  KillLex;
  FBuffer.Free;
  if FEscapeSupport then
     FEscapeBuff.Free;
  Dispose(FCurrPos);
  inherited;
end;

procedure TLexGenerator.KillLex;
begin
  if Assigned(FLexem) then
  begin
     Dispose(FLexem);
     FLexem := Nil;
  end;
end;

procedure TLexGenerator.Load(const Source: String);
begin
  FCurr := PWideChar(Source);
end;

{$HINTS OFF}
procedure TLexGenerator.NextLex;
var
  Route: TRoute;
  Trigger: TTrigger;
  CTyp: TClass;
  UseEscape, UseEscapeEnd: Boolean;
begin
  CreateLexeme;
   UseEscape := False;
  UseEscapeEnd := False;
  FBuffer.Clear;
  if FEscapeSupport then
  begin
    FEscapeRoute := FLexG.EscapeRoute;
    FEscapeBuff.Clear;
  end;

  Route := FFirstRoute;
  while Assigned(Route) do
  begin

     if FEscapeSupport then
     begin
        Trigger := FEscapeRoute[FCurr^];
        if Trigger = Nil then
        begin
           Trigger := Route[FCurr^];
           UseEscape := False;
        end
        else
           UseEscape := True;
     end
     else
     begin
       Trigger := Route[FCurr^];
       UseEscape := False;
     end;

     if Trigger = Nil then Exit;

     CTyp := Trigger.ClassType;

     if CTyp = TErrorTrigger then
        if FCurr^ = #0 then
           Break
        else
           if FExceptBlock then
              Abort
           else
              raise TJSONSyntaxError.Create( TErrorTrigger(Trigger).Message, FCurrPos);

     if CTyp = TUseRouteTrigger then
        if UseEscape then
           FEscapeBuff.Add(FCurr^)
        else
           FBuffer.Add(FCurr^);

     if Trigger.BF and (FLexem.Pos.Col = 0) then
        FLexem.Pos := FCurrPos^;

     if not Trigger.BK then
     begin
        Inc(FCurr);
        if FCurr^ = #10 then
        begin
           Inc(FCurrPos.Line);
           FCurrPos.Col := 1;
        end
        else
           Inc(FCurrPos.Col);
     end;

     if Trigger.ParseProcs <> ppNil then
     begin
        case Trigger.ParseProcs of
            ppInteger: begin
               FLexem.Int := FBuffer.AsInt64;
               FLexem.LType := ltIValue;
            end;
            ppDouble:begin
               FLexem.Dbl := FBuffer.AsDouble;
               FLexem.LType := ltDValue;
            end;
            ppString:begin
               FLexem.LType := ltSValue;
            end;

            ppName: begin
               FLexem.Str := FBuffer.AsString;
               FLexem.LType := FBuffer.AsType;
            end;

            ppEscapeUChar: begin
               if FEscapeBuff.Length = 4 then
               begin
                  FBuffer.Add(Chr(FEscapeBuff.AsHInt));
                  FEscapeBuff.Clear;
                  UseEscapeEnd := True;
               end;
            end;

            ppEscape: begin
               case FEscapeBuff.Buff[0] of
                 'b' : FBuffer.Add(#8);
                 't' : FBuffer.Add(#9);
                 'n' : FBuffer.Add(#10);
                 'v' : FBuffer.Add(#11);
                 'f' : FBuffer.Add(#12);
                 'r' : FBuffer.Add(#13);
                 '\' : FBuffer.Add('\');
                 '"' : FBuffer.Add('"');
                 '''': FBuffer.Add('''');
               else
                 FBuffer.Add(FEscapeBuff.Buff[0]);
               end;
               FEscapeBuff.Clear;
            end;
          end;
     end;

     if Trigger.ED or UseEscapeEnd then
     begin
       if not UseEscape then
       begin
         FFirstRoute := Trigger.NextRoute;
         FLexem.Str := FBuffer.AsString;
         Exit;
       end;
       UseEscape := False;
       UseEscapeEnd := False;
       FEscapeRoute := FLexG.EscapeRoute;
     end
     else
     if UseEscape then
        FEscapeRoute := Trigger.NextRoute
     else
        Route := Trigger.NextRoute;
  end;
  KillLex;
end;
{$HINTS ON}

{ TJSONBuilder }

constructor TJSONBuilder.Create(const JSON: String; const CheckDates: Boolean);
begin
  LGen := TLexGenerator.Create(JSONLexGrammar);
  LGen.Load(JSON);
  FCheckDates := CheckDates;
end;

destructor TJSONBuilder.Destroy;
begin
  LGen.Free;
  inherited;
end;

procedure TJSONBuilder.ReadArray(var Val: IJSONAncestor);
var
  Item: IJSONAncestor;
begin
  LGen.KillLex;
  Val := TJSONArray.Create;

  repeat
    Item := ReadValue;
    if Assigned(Item) then
       TJSONArray(Val).Add(Item);
  until not LGen.CheckKill(ltVirgule);

  if not LGen.CheckKill(ltCRight) then
     raise TJSONSyntaxError.Create(Err_UnexpectedEndOfInput, LGen.CurrPos);
end;

procedure TJSONBuilder.ReadFalse(var Val: IJSONAncestor);
begin
  Val := TJSONBoolean.Create(False);
  LGen.KillLex;
end;

procedure TJSONBuilder.ReadFloat(var Val: IJSONAncestor);
begin
  Val := TJSONFloat.Create(LGen.Current.Dbl);
  LGen.KillLex;
end;

procedure TJSONBuilder.ReadInteger(var Val: IJSONAncestor);
begin
  Val := TJSONInteger.Create(LGen.Current.Int);
  LGen.KillLex;
end;

procedure TJSONBuilder.ReadNull(var Val: IJSONAncestor);
begin
  Val := TJSONNull.Create(True);
  LGen.KillLex;
end;

procedure TJSONBuilder.ReadObject(var Val: IJSONAncestor);
var
  Name: String;
begin
  LGen.KillLex;
  Val := TJSONObject.Create;
  repeat
    if LGen.CheckName(Name) then
    begin
       LGen.KillLex;
       if not LGen.CheckKill(ltColon) then
          raise TJSONSyntaxError.CreateFmt(Err_Expected, [':'], LGen.CurrPos);
       TJSONObject(Val).AddPair(TJSONPair.Create(Name, ReadValue));
    end
  until not LGen.CheckKill(ltVirgule);

  if not LGen.CheckKill(ltBRight) then
     raise TJSONSyntaxError.Create(Err_UnexpectedEndOfInput, LGen.CurrPos);
end;

procedure TJSONBuilder.ReadString(var Val: IJSONAncestor);
var
  dT: TDateTime;
  DVal: TDataType;
label
  JMP;
begin
  if (not FCheckDates) or (Length(LGen.Current.Str) > 25 {2015-10-20T12:22:24+00:00}) or (Length(LGen.Current.Str) < 5 {22:22}) then
     JMP:Val := TJSONString.Create( LGen.Current.Str )
  else
     if TJSONDateManager.Check(LGen.Current.Str, dT, DVal ) then
        case DVal of
           dtDateTime: Val := TJSONDateTime.Create(dT);
           dtDate    : Val := TJSONDate.Create(TDate(dT));
           dtTime    : Val := TJSONTime.Create(TTime(dT));
           else
             goto JMP;
        end
     else
       goto JMP;
  LGen.KillLex;
end;

procedure TJSONBuilder.ReadTrue(var Val: IJSONAncestor);
begin
  Val := TJSONBoolean.Create(True);
  LGen.KillLex;
end;

function TJSONBuilder.ReadValue: IJSONAncestor;
begin
  case LGen.Check([ ltSValue, ltIValue, ltDValue, ltBLeft, ltCLeft,
                    ltTrue, ltFalse, ltNull ]) of
    ltSValue: ReadString(Result);
    ltIValue: ReadInteger(Result);
    ltDValue: ReadFloat(Result);
    ltBLeft : ReadObject(Result);
    ltTrue  : ReadTrue(Result);
    ltFalse : ReadFalse(Result);
    ltCLeft : ReadArray(Result);
    ltNull  : ReadNull(Result);
  else
    Result := Nil;
  end;
end;

{ TJSONString }

procedure TJSONString.AsJSONString(Str: TJSONWriter);
begin
  if IsNull then
     Str.AppendVal( cNull )
  else
     Str.AppendVal( '"' +  StrToUTF16(Value) + '"' );
end;

{ TJSONInteger }

procedure TJSONInteger.AsJSONString(Str: TJSONWriter);
begin
  if FNull then
     Str.AppendVal( cNull )
  else
     Str.AppendVal( Value );
end;


{ TJSONFloat }

procedure TJSONFloat.AsJSONString(Str: TJSONWriter);
begin
  if FNull then
     Str.AppendVal( cNull )
  else
     Str.AppendVal( FloatToStr(Value, FloatFormat) );
end;

{ TJSONBoolean }

procedure TJSONBoolean.AsJSONString(Str: TJSONWriter);
begin
   Str.AppendVal( String(iff( IsNull, cNull, iff( Value, 'true', 'false') )) );
end;

{ TJSONNull }

procedure TJSONNull.AsJSONString(Str: TJSONWriter);
begin
  Str.AppendVal( cNull );
end;

function TJSONNull.GetIsNull: Boolean;
begin
  Result := True;
end;

{ TJSONObject }

procedure TJSONObject.AddPair(P: IJSONPair);
var
  N: IJSONPair;
begin
  N := Get(P.Name);
  if Assigned(N) then
  begin
     FPairList.Remove(N);
     N := Nil;
  end;
  FPairList.Add(P);
end;

procedure TJSONObject.AddPair(Name: String; Value: IJSONAncestor);
begin
  AddPair( TJSONPair.Create(Name, Value) );
end;

procedure TJSONObject.AsJSONString(Str: TJSONWriter);
var
  P: IJSONPair;
  I,L: Integer;
begin
  if FNull then
     Str.AppendVal( cNull )
  else
  begin
    Str.Append('{', True);
    Str.Inc;
    L := Count-1;
    for I := 0 to L do
    begin
       P := FPairList[I];
       Str.Append('"' + StrToUTF16(P.Name) + '":');
       if Str.Ident and (P.JSONValue.DataType in [dtObject, dtArray]) then
          Str.Append('', True);
       P.JSONValue.AsJSONString(Str);
       if I < L then
          Str.AppendVal(',', Str.Ident);
    end;
    Str.Dec;
    if Str.Ident then
       Str.Append(#$D#$A);
    Str.Append('}');
  end;
end;

function TJSONObject.Count: Integer;
begin
  Result := FPairList.Count;
end;

constructor TJSONObject.Create;
begin
  FPairList := TList<IJSONPair>.Create;
end;

destructor TJSONObject.Destroy;
begin
  FPairList.Free;
  inherited;
end;

function TJSONObject.Get(const Name: String): IJSONPair;
var
  P: IJSONPair;
begin
  for P in FPairList do
      if CompareText(Name, P.Name) = 0 then
         Exit(P);
  Result := Nil;
end;

function TJSONObject.GetIsNull: Boolean;
begin
  Result := FNull;
end;

function TJSONObject.Get(const Index: Integer): IJSONPair;
begin
  if (FPairList.Count = 0) or (FPairList.Count <= Index) then
     Result := Nil
  else
     Result := FPairList[Index];
end;

function TJSONObject.GetEnumerator: TJSONEnumerator<IJSONPair>;
begin
  Result.Index := -1;
  Result.List := FPairList;
end;

class function TJSONObject.ParseJSONValue(const Str: String; const CheckDate: Boolean): IJSONAncestor;
begin
  Result := TSuperParser.ParseJSON(Str, CheckDate);
end;

procedure TJSONObject.Remove(P: IJSONPair);
begin
  Remove(P.Name);
end;

procedure TJSONObject.Remove(const Index: Integer);
begin
  if Count > Index then
     FPairList.Delete(Index);
end;

procedure TJSONObject.Sort(Comparison: TJSONComparison<IJSONPair>);
begin
  FPairList.Sort( TComparer<IJSONPair>.Construct(
    TComparison<IJSONPair>(Comparison)
  ));
end;

procedure TJSONObject.Remove(const Name: String);
var
  R: IJSONPair;
begin
  R := Get(Name);
  if Assigned(R) then
  begin
     FPairList.Remove(R);
     R := Nil;
  end;
end;


{ TJSONPair }

constructor TJSONPair.Create(const aName: String; aValue: IJSONAncestor);
begin
  FName := aName;
  FValue := aValue;
end;

destructor TJSONPair.Destroy;
begin
  FValue := Nil;
  inherited;
end;


function TJSONPair.GetName: String;
begin
  Result := FName;
end;

function TJSONPair.GetValue: IJSONAncestor;
begin
  Result := FValue;
end;

procedure TJSONPair.SetName(const Value: String);
begin
  FName := Value;
end;

procedure TJSONPair.SetValue(const Value: IJSONAncestor);
begin
  FValue := Value;
end;

{ TJSONSyntaxError }

constructor TJSONSyntaxError.Create(const Msg: String; Pos: PPosition);
begin
  inherited CreateFmt(Msg + '. (Line: %d Col: %d)', [Pos.Line, Pos.Col]);
end;

constructor TJSONSyntaxError.CreateFmt(const Msg: String; const Args: array of TVarRec;
  Pos: PPosition);
begin
  Create( Format(Msg, Args), Pos );
end;


{ TJSONArray }

procedure TJSONArray.Add(Val: IJSONAncestor);
begin
  FList.Add(Val);
end;

procedure TJSONArray.AsJSONString(Str: TJSONWriter);
var
  I,L: Integer;
begin
  if FNull then
     Str.AppendVal( cNull )
  else
  begin
    Str.Append('[', True);
    Str.Inc;
    L := Count - 1;
    for I := 0 to L do
    begin
       if FList = Nil then Continue;
       FList[I].AsJSONString(Str);
       if I < L then
          Str.AppendVal(',', Str.Ident);
    end;
    Str.Dec;
    if Str.Ident then
       Str.Append(#$D#$A);
    Str.Append(']');
  end;
end;

procedure TJSONArray.Clear;
begin
  FList.Clear;
end;

function TJSONArray.Count: Integer;
begin
  Result := FList.Count;
end;

constructor TJSONArray.Create;
begin
  FList := TList<IJSONAncestor>.Create;
end;

destructor TJSONArray.Destroy;
begin
  FList.Free;
  inherited;
end;

function TJSONArray.Get(const I: Integer): IJSONAncestor;
begin
  if (FList.Count = 0) or (Flist.Count <= I) then
     Result := Nil
  else
     Result := FList.Items[I]
end;

function TJSONArray.GetEnumerator: TJSONEnumerator<IJSONAncestor>;
begin
   Result.Index := -1;
   Result.List := FList;
end;

function TJSONArray.GetIsNull: Boolean;
begin
  Result := FNull;
end;

procedure TJSONArray.Remove(Val: IJSONAncestor);
begin
  FList.Remove(Val);
end;

procedure TJSONArray.Remove(Index: Integer);
begin
  FList.Delete(Index);
end;

procedure TJSONArray.SetIndex(const Int: Integer; const Value: IJSONAncestor);
begin
  if (FList.Count = 0) or (Flist.Count <= Int) then
      Exit;
  FList[Int] := Value;
end;

procedure TJSONArray.Sort(Comparison: TJSONComparison<IJSONAncestor>);
begin
  FList.Sort( TComparer<IJSONAncestor>.Construct(
    TComparison<IJSONAncestor>(Comparison)
  ));
end;

{ TJSONValue<T> }

constructor TJSONValue<T>.Create(const Value: T);
begin
  FData := Value;
  FNull := False;
end;

constructor TJSONValue<T>.CreateNull;
begin
  FNull := True;
end;

function TJSONValue<T>.GetData: T;
begin
  Result := FData;
end;

function TJSONValue<T>.GetIsNull: Boolean;
begin
  Result := FNull;
end;

procedure TJSONValue<T>.SetData(const Value: T);
begin
  FData := Value;
end;

procedure TJSONValue<T>.SetNull;
begin
  FNull := True;
end;

{ TJSONInterpreter }

constructor TJSONInterpreter.Create(const Expression: String;
  JSON: IJSONAncestor; BlockException: Boolean = False);
begin
  LGen := TLexGenerator.Create(JSONLexGrammar, BlockException);
  LGen.Load(Expression);
  FJSON := JSON;
  FExceptionBlock := BlockException;
end;

procedure TJSONInterpreter.CreateExcept(const S: String;
  Args: array of TVarRec);
begin
  if FExceptionBlock then
     Abort
  else
     raise TJSONSyntaxError.CreateFmt(S, Args, LGen.CurrPos);
end;

procedure TJSONInterpreter.CreateExcept(const S: String);
begin
  if FExceptionBlock then
     Abort
  else
     raise TJSONSyntaxError.Create(S, LGen.CurrPos);
end;

destructor TJSONInterpreter.Destroy;
begin
  LGen.Free;
  inherited;
end;

function TJSONInterpreter.ReadArray(Base: IJSONAncestor): IJSONArray;
var
  Item: IJSONAncestor;
begin
  LGen.KillLex;
  Result := TJSONArray.Create;
  repeat
    Item := ReadValue(Base);
    if Assigned(Item) then
       TJSONArray(Result).Add(Item);
  until not LGen.CheckKill(ltVirgule);

  if not LGen.CheckKill(ltCRight) then
     CreateExcept(Err_UnexpectedEndOfInput);
end;

function TJSONInterpreter.ReadArrayIndex(Base: IJSONArray): IJSONAncestor;
var
  RName: IJSONAncestor;
  Index: Integer;
begin
  Index := 0;
  case LGen.Check([ltIValue, ltName]) of
    ltIValue:
      begin
         Index := StrToInt(LGen.Current.Str);
         LGen.KillLex;
      end;
    ltName:
      begin
         RName := ReadName(FJSON);
         if not (RName is TJSONInteger) then
            CreateExcept(Err_ExpectedButFound, [STokenTypes[ltIValue], STokenTypes[LGen.Current.LType]])
         else
            Index := TJSONInteger(RName).Value;
      end
    else
       CreateExcept(Err_ExpectedButFound, [STokenTypes[ltIValue], STokenTypes[LGen.Current.LType]])
  end;
  Result := Base.Index[Index];
  if not LGen.CheckKill(ltCRight) then
     CreateExcept(Err_Expected, [STokenTypes[ltCRight]]);
  if LGen.CheckKill(ltDot) then
  begin
     RName := ReadName(Result);
     if Assigned(RName) then
        Result := RName;
  end;
end;

function TJSONInterpreter.ReadExpression: IJSONAncestor;
begin
  try
    case LGen.Check([ltBLeft, ltCLeft]) of
       ltBLeft : Result := ReadObject(FJSON);
       ltCLeft : Result := ReadArray(FJSON);
    else
       Result := ReadName(FJSON);
    end;
  except
    on E: Exception do
    begin
      if FExceptionBlock then
         Result := Nil
      else
         raise;
    end;
  end;
end;

function TJSONInterpreter.ReadName(Base: IJSONAncestor): IJSONAncestor;
var
  Name: String;
  Pair: IJSONPair;
begin
  if not LGen.CheckName(Name) then
     Exit(Nil);

  if Base is TJSONArray then
  begin
    if LGen.Current.LType <> ltIValue then
       CreateExcept(Err_ExpectedButFound, [STokenTypes[ltIValue], STokenTypes[LGen.Current.LType]])
    else
       Result := TJSONArray(Base).Index[StrToInt(Name)];
  end
  else
  if Base is TJSONObject then
  begin
     Pair := TJSONObject(Base).Get(Name);
     if Pair = Nil then
        Exit(Nil)
     else
        Result := Pair.JSONValue;
     LGen.KillLex;
     if Assigned(Result) then
         case LGen.CheckKill([ltDot, ltCLeft]) of
           ltDot:
              Result := ReadName(Result);
           ltCLeft:
              begin
                 if Result is TJSONArray then
                    Result := ReadArrayIndex(TJSONArray(Result))
                 else
                    CreateExcept(Err_Expected, ['Array']);
              end;
         end;
  end
  else
    Result := Nil;
end;

function TJSONInterpreter.ReadObject(Base: IJSONAncestor): IJSONObject;
var
  Name: String;
begin
  LGen.KillLex;
  Result := TJSONObject.Create;
  repeat
    if LGen.CheckName(Name) then
    begin
       LGen.KillLex;
       if not LGen.CheckKill(ltColon) then
          CreateExcept(Err_Expected, [':']);
       TJSONObject(Result).AddPair(TJSONPair.Create(Name, ReadValue(Base)));
    end
  until not LGen.CheckKill(ltVirgule);

  if not LGen.CheckKill(ltBRight) then
     CreateExcept(Err_UnexpectedEndOfInput);
end;

function TJSONInterpreter.ReadValue(Base: IJSONAncestor): IJSONAncestor;
begin
  case LGen.Check([ ltSValue, ltIValue, ltDValue, ltBLeft, ltCLeft,
                    ltTrue, ltFalse, ltName, ltNull ]) of
    ltSValue: Result := TJSONString.Create(LGen.Current.Str);
    ltIValue: Result := TJSONInteger.Create(LGen.Current.Int);
    ltDValue: Result := TJSONFloat.Create(LGen.Current.Dbl);
    ltBLeft : Result := ReadObject(Base);
    ltTrue  : Result := TJSONBoolean.Create(True);
    ltFalse : Result := TJSONBoolean.Create(False);
    ltCLeft : Result := ReadArray(Base);
    ltNull  : Result := TJSONNull.Create(True);
    ltName  : begin
       Result := ReadName(Base);
       Exit;
    end
  else
    Result := Nil;
    Exit;
  end;
  LGen.KillLex;
end;

{ TSuperEnumerator<T> }

function TJSONEnumerator<T>.GetCurrent: T;
begin
  Result := List[Index]
end;

function TJSONEnumerator<T>.MoveNext: Boolean;
begin
  Result := Index < List.Count - 1;
  if Result then
    Inc(Index);
end;

{ TJSONWriter }

function TJSONWriter.Append(const Value: string; const CRLF: Boolean = False): TJSONWriter;
begin
  if FIdent then
  begin
     FData.Append(' ', FIdentOffset);
     if CRLF then
        FData.AppendLine(Value)
     else
        FData.Append(Value)
  end
  else
     FData.Append(Value);
  Result := Self;
end;

function TJSONWriter.Append(const Value: int64; const CRLF: Boolean): TJSONWriter;
begin
  Result := Append(IntToStr(Value), CRLF);
end;

function TJSONWriter.AppendVal(const Value: string; const CRLF: Boolean): TJSONWriter;
begin
  if CRLF then
     FData.AppendLine(Value)
  else
     FData.Append(Value);
  Result := Self;
end;

function TJSONWriter.AppendVal(const Value: int64; const CRLF: Boolean): TJSONWriter;
begin
  Result := Append(IntToStr(Value), CRLF);
end;

constructor TJSONWriter.Create(const useIdent, useUniversalTime: Boolean);
begin
  inherited Create;
  FData := TStringBuilder.Create;
  FIdent := useIdent;
  FUniversalTime := useUniversalTime;
  FIdentOffset := 0;
end;

procedure TJSONWriter.Dec;
begin
  System.Dec(FIdentOffset, IDENT_SIZE);
end;

destructor TJSONWriter.Destroy;
begin
  FData.Free;
  inherited;
end;

procedure TJSONWriter.Inc;
begin
  System.Inc(FIdentOffset, IDENT_SIZE);
end;

function TJSONWriter.ToString: string;
begin
  Result := FData.ToString;
end;

{ TJSONDateTime }

constructor TJSONDateTime.Create(const Value: TDateTime; const Format: String);
begin
  inherited Create(Value);
  FFormat := Format;
end;

{ TJSONDateManager }

class function TJSONDateManager.Check(const Data: String; var AValue: TDateTime;
  var Typ: TDataType): Boolean;
var
  CallBck: TJSONDateTimeCheckCallBack;
begin
  for CallBck in FFormats do
      if CallBck(Data, AValue, Typ) then
         Exit(True);
  Result := False;
end;

class constructor TJSONDateManager.Create;
begin
  FFormats := TList<TJSONDateTimeCheckCallBack>.Create;
end;

class destructor TJSONDateManager.Destroy;
{$IF CompilerVersion < 29}
var
  I: Integer;
{$ENDIF}
begin
  if Assigned(FFormats) then begin
    {$IF CompilerVersion < 29}
    for I := 0 to FFormats.Count - 1 do
        FFormats.List[I]._Release;
    {$ENDIF}
    FFormats.Free;
  end;
end;

class function TJSONDateManager.GetFormats: TList<TJSONDateTimeCheckCallBack>;
begin
  Result := FFormats;
end;

{ TISO8601 }

constructor TISO8601.Create(const Value: String);
var
  Matches: TMatchCollection;
begin
  FillChar(Self, SizeOf(TISO8601), #0);
  Matches := TRegEx.Matches(Value,  '(?=\d{4})((\d{4})-(\d{2})-(\d{2}))?(T(\d{2})\:(\d{2})\:('+
                                    '\d{2})(Z)?(\.(\d{1,3})(Z)?)?([+-](\d{2})\:(\d{2}))?)?|(\d{2})\:('+
                                    '\d{2})\:(\d{2})(Z)?(\.(\d{1,3}))?([+-](\d{2})\:(\d{2}))?');
  if Matches.Count <> 1 then Exit;
  FData := Matches.Item[0];
  FSuccess := Trim(FData.Value) = Trim(Value);
  if not FSuccess then Exit;
  ReadStructure;
end;

function TISO8601.GetIntData(const Index: Integer): Integer;
begin
  Result := StrToInt(GetStrData(Index));
end;

function TISO8601.GetIntData(const Index: Integer; const P: Boolean): Integer;
begin
  Result := GetIntData(Index);
  if not P then
     Result := -1 * Result;
end;

function TISO8601.GetStrData(const Index: Integer): String;
begin
  Result := FData.Groups.Item[Index].Value;
end;

function TISO8601.NextOffset: Integer;
begin
  Inc(FOffset);
  Result := FOffset;
end;

procedure TISO8601.ReadStructure;
var
  Len, VLen: Integer;
  Grp: TGroup;
begin
  FOffset := 1;
  Len := FData.Groups.Count - 1;
  while FOffset <= Len do
  begin
    Grp := FData.Groups.Item[FOffset];
    with Grp do
       if Value > '' then begin
          VLen := System.Length(Value);
          if (Value[CharIndex] <> '.') and (VLen = 4) then begin
             Dec(FOffset);
             ReadDate
          end else
          case Value[CharIndex] of
            '0'..'9': if VLen = 2 then begin
               FUseTime := True;
               Dec(FOffset);
               if not ReadTime then
               begin
                  FSuccess := False;
                  Exit;
               end;
            end;

            'T', 't': begin
               FUseTime := True;
               if not ReadTime then
               begin
                  FSuccess := False;
                  Exit;
               end;
            end;
            'Z': if FUseTime then ReadZulu;
            '.': if FUseTime then ReadMS;
            '+': if FUseTime then ReadTZ(True);
            '-': if FUseTime then ReadTZ(False);
          end;
       end;
    Inc(FOffset);
  end;
  if FUseDate and FUseTime then
     FValueType := dtDateTime
  else if FUseDate then
     FValueType := dtDate
  else if FUseTime then
     FValueType := dtTime;
end;

function TISO8601.ReadDate: Boolean;
begin
 Result := True;
 if not TryEncodeDate( GetIntData(NextOffset), GetIntData(NextOffset), GetIntData(NextOffset), FValue ) then
 begin
    FValue := 0;
    Result := False;
 end
 else
    FUseDate := True
end;

procedure TISO8601.ReadMS;
var
  Temp: TDateTime;
begin
  if TryEncodeTime(0, 0, 0, GetIntData(NextOffset), Temp) then
     FValue := FValue + TTime(Temp);
end;

function TISO8601.ReadTime: Boolean;
var
  Temp: TDateTime;
begin
  if TryEncodeTime(GetIntData(NextOffset), GetIntData(NextOffset), GetIntData(NextOffset), 0, Temp ) then
  begin
     FValue := FValue + TTime(Temp);
     FUseTime := True;
     Result := True;
  end
  else
     Result := False;
end;

procedure TISO8601.ReadTZ(const P: Boolean);
begin
  FValue := IncHour(FValue, -1 * GetIntData(NextOffset, P));
  FValue := IncMinute(FValue, -1 * GetIntData(NextOffset, P));
  FValue := TTimeZone.Local.ToLocalTime(FValue)
end;


procedure TISO8601.ReadZulu;
begin
  FValue := TTimeZone.Local.ToLocalTime(FValue);
end;

{ TJSONDate }

constructor TJSONDate.Create(const Value: TDate; const Format: String);
begin
  inherited Create(Value);
  FFormat := Format;
end;

{ TJSONTime }

constructor TJSONTime.Create(const Value: TTime; const Format: String);
begin
  inherited Create(Value);
  FFormat := Format;
end;

{ TJSONBaseDate<T> }

procedure TJSONBaseDate<T>.AsJSONString(Str: TJSONWriter);
begin
  if FNull then
     Str.AppendVal( cNull )
  else
  begin
     if Str.UniversalTime then
        Str.AppendVal( '"' +  FormatDateTime(FFormat, TTimeZone.Local.ToUniversalTime(PDateTime(@FData)^)) + 'Z"' )
     else
        Str.AppendVal( '"' +  FormatDateTime(FFormat, PDateTime(@FData)^) + '"' );
  end;
end;

function TJSONBaseDate<T>.GetAsString: String;
begin
   Result := FormatDateTime(FFormat, PDateTime(@FData)^);
end;

{ TJSONRaw }

procedure TJSONRaw.AsJSONString(Str: TJSONWriter);
begin
  Str.AppendVal( Value );
end;

initialization

  JSONLexGrammar := TJSONGrammar.Create;

  TJSONDateManager.Formats.Add( (* ISO-8601 | [Date] + [ Time + [MS] + [UTC] + [Z] ] *)
     function(Str: String; var AValue: TDateTime; var Typ: TDataType): Boolean
     begin
         with TISO8601.Create(Str) do
         begin
            Result := Success;
            if Result then
            begin
               AValue := Value;
               Typ := ValueType;
            end;
         end;
     end);

finalization

  JSONLexGrammar.Free;

end.
