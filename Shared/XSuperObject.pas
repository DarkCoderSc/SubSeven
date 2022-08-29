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

  // Updated by Jean-Pierre LESUEUR to avoid having shitty Indy components embedded

unit XSuperObject;

interface

uses
  System.Classes,
  System.Variants,
  System.SysUtils,
  System.Character,
  XSuperJSON,
  System.RTTI,
  System.TypInfo,
  Generics.Collections;

  const CharIndex = Low(String);
type

  SOException = class(Exception) end;
  SOInvalidDate = class(SOException) end;
  ESerializeError = class(Exception) end;

  ISuperObject = interface;
  ISuperArray = interface;
  ICast = Interface;
  IMember = ICast;
  TSuperObject = class;
  TSuperArray = class;

  TMemberStatus = (jUnAssigned, jNull, jAssigned);
  TJSONType = (jtObject, jtArray);

  Alias = class(TCustomAttribute)
  private
    FName: String;
  public
    constructor Create(const AName: String);
    property Name: String read FName write FName;
  end;

  TRevalOption = (roNone, roEmptyArrayToNull);

  REVAL = class(TCustomAttribute)
  private
    FOption: TRevalOption;
    FEqual: Variant;
    FValue: Variant;
  public
    constructor Create(EQVal: String; NewVal: String); overload;
    constructor Create(EQVal: Integer; NewVal: Integer); overload;
    constructor Create(EQVal: Boolean; NewVal: Boolean); overload;
    constructor Create(EQVal: Double; NewVal: Double); overload;
    constructor Create(EQVal: String); overload;
    constructor Create(EQVal: Integer); overload;
    constructor Create(EQVal: Double); overload;
    constructor Create(EQVal: Boolean); overload;
    constructor Create(Option: TRevalOption); overload;
    function CheckEQ(Val: TValue): Boolean;
    property Equal: Variant read FEqual;
    property Value: Variant read FValue;
    property Option: TRevalOption read FOption;
  end;

  DISABLE = class(TCustomAttribute)
  end;

  DISABLEREAD = class(TCustomAttribute)
  end;

  DISABLEWRITE = class(TCustomAttribute)
  end;

  IBase = interface
  ['{872FA14E-9276-4F86-A8D8-832CF39DACE6}']
    function AsObject: ISuperObject;
    function AsArray: ISuperArray;
  end;

  TBase = class(TInterfacedObject, IBase)
    function AsObject: ISuperObject; virtual;
    function AsArray: ISuperArray; virtual;
  end;

  IBaseJSON<T, Typ> = interface(IBase)
  ['{EBD49266-BEF2-4B79-9BAF-329F725E0568}']
    function GetBoolean(V: Typ): Boolean;
    function GetInteger(V: Typ): Int64;
    function GetString(V: Typ): String;
    procedure SetBoolean(V: Typ; const Value: Boolean);
    procedure SetInteger(V: Typ; const Value: Int64);
    procedure SetString(V: Typ; const Value: String);
    function GetObject(V: Typ): ISuperObject;
    procedure SetObject(V: Typ; const Value: ISuperObject);
    function GetArray(V: Typ): ISuperArray;
    procedure SetArray(V: Typ; const Value: ISuperArray);
    function GetDouble(V: Typ): Double;
    procedure SetDouble(V: Typ; const Value: Double);
    function GetVariant(V: Typ): Variant;
    procedure SetVariant(V: Typ; const Value: Variant);
    function GetDateTime(V: Typ): TDateTime;
    procedure SetDateTime(V: Typ; const Value: TDateTime);
    function GetDate(V: Typ): TDate;
    procedure SetDate(V: Typ; const Value: TDate);
    function GetTime(V: Typ): TTime;
    procedure SetTime(V: Typ; const Value: TTime);
    function GetSelf: T;
    function GetAncestor(V: Typ): IJSONAncestor;
    function GetNull(V: Typ): TMemberStatus;
    procedure SetNull(V: Typ; const Value: TMemberStatus);
    function GetDataType: TDataType;

    property Null[V: Typ]: TMemberStatus read GetNull write SetNull;
    property S[V: Typ]: String read GetString write SetString;
    property I[V: Typ]: Int64 read GetInteger write SetInteger;
    property B[V: Typ]: Boolean read GetBoolean write SetBoolean;
    property F[V: Typ]: Double read GetDouble write SetDouble;
    property O[V: Typ]: ISuperObject read GetObject write SetObject;
    property A[V: Typ]: ISuperArray read GetArray write SetArray;
    property V[V: Typ]: Variant read GetVariant write SetVariant;
    property D[V: Typ]: TDateTime read GetDateTime write SetDateTime;
    property Date[V: Typ]: TDate read GetDate write SetDate;
    property Time[V: Typ]: TTime read GetTime write SetTime;
    property Ancestor[V: Typ]: IJSONAncestor read GetAncestor;
    function Contains(Key: Typ): Boolean;
    function GetType(Key: Typ): TVarType;
    procedure Sort(Comparison: TJSONComparison<IMember>);
    procedure SaveTo(Stream: TStream; const Ident: Boolean = false; const UniversalTime : Boolean = false); overload;
    procedure SaveTo(AFile: String; const Ident: Boolean = false; const UniversalTime : Boolean = false); overload;
    function AsJSON(const Ident: Boolean = False; const UniversalTime: Boolean = False): String;
    property Self: T read GetSelf;
    property DataType: TDataType read GetDataType;
  end;

  TJSONValueHelper = class helper for TJSONAncestor
  public
    function ValueEx<T>: Variant;
  end;

  TCondCallBack<T> = reference to function(Arg: T): Boolean;

  TBaseJSON<T, Typ> = class(TBase, IBaseJSON<T, Typ>)
  protected
    FJSONObj: T;
    FCasted: IJSONAncestor;
    FInterface: IInterface;
    FCheckDate: Boolean;
    function ContainsEx(Key: Typ; out Value: IJSONAncestor): Boolean;
    function  DefaultValueClass<TT: Class>(const Value): TT;
    procedure Member<MT: Class; TValue>(const Name: Typ; const Value: TValue); overload;
    function  Member(const Name: Typ): Boolean; overload;
    function  GetValue<C: Class>(const Name: Typ): C;
    function  GetSelf: T;
    function  GetData(Key: Typ): IJSONAncestor;
    function  GetVariant(V: Typ): Variant;
    procedure SetVariant(V: Typ; const Value: Variant);
    function  GetDataType: TDataType;
  protected
    function GetObject(V: Typ): ISuperObject; virtual;
    function GetArray(V: Typ): ISuperArray; virtual;
    function GetBoolean(V: Typ): Boolean; virtual;
    function GetInteger(V: Typ): Int64; virtual;
    function GetString(V: Typ): String; virtual;
    function GetDouble(V: Typ): Double; virtual;
    function GetAncestor(V: Typ): IJSONAncestor; inline;
    function GetNull(V: Typ): TMemberStatus; virtual;
    function GetDateTime(V: Typ): TDateTime; virtual;
    function GetDate(V: Typ): TDate; virtual;
    function GetTime(V: Typ): TTime; virtual;
    procedure SetDate(V: Typ; const Value: TDate); virtual;
    procedure SetTime(V: Typ; const Value: TTime); virtual;
    procedure SetDateTime(V: Typ; const Value: TDateTime); virtual;
    procedure SetObject(V: Typ; const Value: ISuperObject); virtual;
    procedure SetArray(V: Typ; const Value: ISuperArray); virtual;
    procedure SetBoolean(V: Typ; const Value: Boolean); virtual;
    procedure SetInteger(V: Typ; const Value: Int64); virtual;
    procedure SetString(V: Typ; const Value: String); virtual;
    procedure SetDouble(V: Typ; const Value: Double); virtual;
    procedure SetNull(V: Typ; const Value: TMemberStatus); virtual;
  public
    constructor Create(JSON: String = '{}'; const CheckDate: Boolean = True); overload;
    constructor Create(JSON: T; const CheckDate: Boolean = True); overload;
    constructor CreateCasted(Value: IJSONAncestor; const CheckDate: Boolean = True);
    constructor CreateWithEscape(JSON: String = '{}'; const CheckDate: Boolean = True);
    destructor Destroy; override;
    property Null[V: Typ]: TMemberStatus read GetNull write SetNull;
    property S[V: Typ]: String read GetString write SetString;
    property I[V: Typ]: Int64 read GetInteger write SetInteger;
    property B[V: Typ]: Boolean read GetBoolean write SetBoolean;
    property F[V: Typ]: Double read GetDouble write SetDouble;
    property O[V: Typ]: ISuperObject read GetObject write SetObject;
    property A[V: Typ]: ISuperArray read GetArray write SetArray;
    property V[V: Typ]: Variant read GetVariant write SetVariant;
    property D[V: Typ]: TDateTime read GetDateTime write SetDateTime;
    property Date[V: Typ]: TDate read GetDate write SetDate;
    property Time[V: Typ]: TTime read GetTime write SetTime;
    property Ancestor[V: Typ]: IJSONAncestor read GetAncestor;
    function Contains(Key: Typ): Boolean;
    function GetType(Key: Typ): TVarType;
    procedure Sort(Comparison: TJSONComparison<IMember>); virtual; abstract;
    procedure SaveTo(Stream: TStream; const Ident: Boolean = false; const UniversalTime : Boolean = false); overload; virtual; abstract;
    procedure SaveTo(AFile: String; const Ident: Boolean = false; const UniversalTime : Boolean = false); overload; virtual; abstract;
    function AsJSON(const Ident: Boolean = False; const UniversalTime: Boolean = False): String; inline;
    property Self: T read GetSelf;
    property DataType: TDataType read GetDataType;
  end;


  ICast = interface
  ['{0F5387AB-C1C9-4229-921D-226960332271}']
    function GetArray: ISuperArray;
    function GetBoolean: Boolean;
    function GetDataType: TDataType;
    function GetFloat: Double;
    function GetInteger: Int64;
    function GetObject: ISuperObject;
    function GetString: String;
    function GetName: String;
    function GetVariant: Variant;
    function GetDate: TDate;
    function GetDateTime: TDateTime;
    function GetTime: TTime;
    procedure SetDate(const Value: TDate);
    procedure SetDateTime(const Value: TDateTime);
    procedure SetTime(const Value: TTime);
    procedure SetBoolean(const Value: Boolean);
    procedure SetFloat(const Value: Double);
    procedure SetInteger(const Value: Int64);
    procedure SetString(const Value: String);
    procedure SetVariant(const Value: Variant);

    property AsObject: ISuperObject read GetObject;
    property AsArray: ISuperArray read GetArray;
    property AsString: String read GetString write SetString;
    property AsInteger: Int64 read GetInteger write SetInteger;
    property AsFloat: Double read GetFloat write SetFloat;
    property AsBoolean: Boolean read GetBoolean write SetBoolean;
    property AsVariant: Variant read GetVariant write SetVariant;
    property AsDateTime: TDateTime read GetDateTime write SetDateTime;
    property AsDate: TDate read GetDate write SetDate;
    property AsTime: TTime read GetTime write SetTime;
    property DataType: TDataType read GetDataType;
    property Name: String read GetName;
    function ToString(const Ident: Boolean = False; const UniversalTime: Boolean = False): String;
  end;

  TCast = class(TInterfacedObject, ICast)
  private
    FJSON: IJSONAncestor;
    FName: String;
    function GetArray: ISuperArray;
    function GetBoolean: Boolean;
    function GetDataType: TDataType;
    function GetFloat: Double;
    function GetInteger: Int64;
    function GetObject: ISuperObject;
    function GetString: String;
    procedure SetBoolean(const Value: Boolean);
    procedure SetFloat(const Value: Double);
    procedure SetInteger(const Value: Int64);
    procedure SetString(const Value: String);
    function GetName: String;
    function GetVariant: Variant;
    procedure SetVariant(const Value: Variant);
    function GetDate: TDate;
    function GetDateTime: TDateTime;
    function GetTime: TTime;
    procedure SetDate(const Value: TDate);
    procedure SetDateTime(const Value: TDateTime);
    procedure SetTime(const Value: TTime);
  public
    constructor Create(Base: IJSONAncestor); overload;
    constructor Create(Base: IJSONPair); overload;
    class function CreateFrom<T>(Base: T): ICast;
    destructor Destroy; override;
    property AsObject: ISuperObject read GetObject;
    property AsArray: ISuperArray read GetArray;
    property AsString: String read GetString write SetString;
    property AsInteger: Int64 read GetInteger write SetInteger;
    property AsFloat: Double read GetFloat write SetFloat;
    property AsBoolean: Boolean read GetBoolean write SetBoolean;
    property AsVariant: Variant read GetVariant write SetVariant;
    property AsDateTime: TDateTime read GetDateTime write SetDateTime;
    property AsDate: TDate read GetDate write SetDate;
    property AsTime: TTime read GetTime write SetTime;
    property DataType: TDataType read GetDataType;
    property Name: String read GetName;
    function ToString(const Ident: Boolean = False; const UniversalTime: Boolean = False): String; reintroduce;
  end;



  ISuperExpression = interface(ICast)
  ['{58366F15-0D83-4BC5-85D5-238E78E73247}']
  end;

  TSuperExpression = class(TCast, ISuperExpression)
  private
    FInterpreter: TJSONInterpreter;
  public
    constructor Create(Base: IJSONAncestor; const Expr: String; const BlockException: Boolean = False);
    destructor Destroy; override;
  end;

  TSuperEnumerator<T> = record
    Index : Integer;
    List : TJSONEnumerator<T>;
    function MoveNext : Boolean;
    function GetCurrent : ICast;
    property Current : ICast read GetCurrent;
  end;

  ISuperObject = interface(IBaseJSON<IJSONObject, String>)
  ['{B7E271F3-205B-4172-8532-BE03F2A6EDE7}']
    procedure First;
    procedure Next;
    function GetEoF: Boolean;
    function GetCount: Integer;
    function GetCurrentKey: String;
    function GetCurrentValue: IJSONAncestor;
    function GetOffset: Integer;
    function GetExpr(const Code: String): ISuperExpression;
    function GetRaw(V: String): String;
    procedure SetRaw(V: String; Value: String);


    procedure Add(const Key: String; const Data: IJSONAncestor);
    procedure SetData(V: String; Data: Variant); overload;
    procedure SetData(V: String; Data: Variant; AFormatSettings: TFormatSettings); overload;
    procedure Remove(Key: String);
    function  Check(const Expr: String): Boolean;

    property Expression[const Code: String]: ISuperExpression read GetExpr; default;
    property Count: Integer read GetCount;
    property EoF: Boolean read GetEoF;
    property CurrentKey: String read GetCurrentKey;
    property CurrentValue: IJSONAncestor read GetCurrentValue;
    property Offset: Integer read GetOffset;
    function Clone: ISuperObject;
    function GetEnumerator: TSuperEnumerator<IJSONPair>;
    function T: TSuperObject;
    function Where(const Cond: TCondCallBack<IMember>): ISuperObject;
    function Delete(const Cond: TCondCallBack<IMember>): ISuperObject;
    function Cast: ICast;
    property Raw[V: String]: String read GetRaw write SetRaw;
  end;

  TSuperObject = class(TBaseJSON<IJSONObject, String>, ISuperObject)
  private
    FOffset: Integer;
    function GetEoF: Boolean;
    function GetCount: Integer;
    function GetCurrentKey: String;
    function GetCurrentValue: IJSONAncestor;
    function GetOffset: Integer;
    function GetExpr(const Code: String): ISuperExpression;
    function GetRaw(V: String): String;
    procedure SetRaw(V: String; Value: String);
  protected
    function GetString(V: String): String; override;
    procedure SetNull(V: String; const Value: TMemberStatus); override;
  public
    procedure First;
    procedure Next;

    procedure Add(const Key: String; const Data: IJSONAncestor);
    procedure SetData(V: String; Data: Variant); overload; inline;
    procedure SetData(V: String; Data: Variant; AFormatSettings: TFormatSettings); overload;

    class function ParseStream(Stream: TStream; CheckDate: Boolean = True): TSuperObject;
    class function ParseFile(FileName: String; CheckDate: Boolean = True): TSuperObject;

    procedure SaveTo(Stream: TStream; const Ident: Boolean = false; const UniversalTime : Boolean = false); overload; override;
    procedure SaveTo(AFile: String; const Ident: Boolean = false; const UniversalTime : Boolean = false); overload; override;
    procedure Remove(Key: String);
    function  Check(const Expr: String): Boolean;

    property Expression[const Code: String]: ISuperExpression read GetExpr; default;
    property Count: Integer read GetCount;
    property Offset: Integer read GetOffset;
    property EoF: Boolean read GetEoF;
    property CurrentKey: String read GetCurrentKey;
    property CurrentValue: IJSONAncestor read GetCurrentValue;
    function GetEnumerator: TSuperEnumerator<IJSONPair>;
    function AsType<T>: T;
    function T: TSuperObject; inline;
    function Clone: ISuperObject;
    function AsObject: ISuperObject; override;
    function AsArray: ISuperArray; override;
    procedure Sort(Comparison: TJSONComparison<IMember>); override;
    function Where(const Cond: TCondCallBack<IMember>): ISuperObject;
    function Delete(const Cond: TCondCallBack<IMember>): ISuperObject;
    function Cast: ICast;
  end;

  ISuperArray = interface(IBaseJSON<IJSONArray, Integer>)
  ['{41A2D578-CFAB-4924-8F15-0D0227F35412}']
    function GetLength: Integer;
    property Length: Integer read GetLength;
    procedure Add(Value: IJSONAncestor); overload;
    procedure Add(Value: ISuperArray); overload;
    procedure Add(Value: ISuperObject); overload;
    procedure Add(Value: Variant; DateFormat: TFormatSettings); overload;
    procedure Add(Value: Variant); overload;
    procedure Delete(Index: Integer); overload;
    procedure Clear;
    function Clone: ISuperArray;
    function GetEnumerator: TSuperEnumerator<IJSONAncestor>;
    function T: TSuperArray;
    function Where(const Cond: TCondCallBack<IMember>): ISuperArray;
    function Delete(const Cond: TCondCallBack<IMember>): ISuperArray; overload;
  end;

  TSuperArray = class(TBaseJSON<IJSONArray, Integer>, ISuperArray)
  private
    function GetLength: Integer;
  protected
    procedure SetNull(V: Integer; const aValue: TMemberStatus); override;
  public
    procedure Add(Value: IJSONAncestor); overload;
    procedure Add(Value: ISuperObject); overload;
    procedure Add(Value: ISuperArray); overload;
    procedure Add(Value: Variant; DateFormat: TFormatSettings); overload;
    procedure Add(Value: Variant); overload;
    procedure Delete(Index: Integer); overload;
    function Delete(const Cond: TCondCallBack<IMember>): ISuperArray; overload;
    procedure Clear;
    property Length: Integer read GetLength;
    function GetEnumerator: TSuperEnumerator<IJSONAncestor>;
    procedure SaveTo(Stream: TStream; const Ident: Boolean = false; const UniversalTime : Boolean = false); overload; override;
    procedure SaveTo(AFile: String; const Ident: Boolean = false; const UniversalTime : Boolean = false); overload; override;
    procedure Sort(Comparison: TJSONComparison<IMember>); override;
    function Clone: ISuperArray;
    function AsArray: ISuperArray; override;
    function AsObject: ISuperObject; override;
    function Where(const Cond: TCondCallBack<IMember>): ISuperArray;
    function T: TSuperArray; inline;
    function AsType<T>: T;
  end;

  TSuperProperty = class(TRttiProperty)
  public
    ArrayRawData: Pointer;
  end;

  TSuperField = class(TRttiField)
  public
    ArrayRawData: Pointer;
  end;

  TSuperDynArr = class(TRttiDynamicArrayType)
  public
    ArrayRawData: Pointer;
  end;

  TSuperArr = class(TRttiArrayType)
  public
    ArrayRawData: Pointer;
  end;

  TGenericsType = (gtNil, gtList, gtObjectList);

  TGenericsInfo = class
  private
    FContext: TRttiContext;
    FType: TRttiType;
    FAddMethod: TRttiMethod;
    FCountProperty: TRttiProperty;
    FGetItemMethod: TRttiIndexedProperty;
  public
    IsGeneric: Boolean;
    Typ: TRttiType;
    CreateArgs: TArray<TValue>;
    procedure AddVal(Instance: TObject; Val: TValue);
    function Count(Instance: TObject): Integer;
    function Item(Instance: TObject; const Index: Integer): TObject;
    constructor Create(GenericClass: TClass; const AIsGeneric: Boolean; AType: TRttiType);
    destructor Destroy; override;
  end;

  TAttributeClass = class of TCustomAttribute;
  TPropertyGetterType = (pgtField, pgtMethod);

  TSerializeParse = class
  private
    class var FGenericsCache: TObjectDictionary<TClass, TGenericsInfo>;
    class function GetAttribute(AttributeType: TAttributeClass; Attributes: TArray<TCustomAttribute>): TCustomAttribute;
    class procedure GetAliasName(const Attributes: TArray<TCustomAttribute>; var Result: String);
    class function  GetREVAL(const Attributues: TArray<TCustomAttribute>): REVAL;
    class function  IsDisabled(const Attributes: TArray<TCustomAttribute>): Boolean; inline;
    class function  IsDisabledRead(const Attributes: TArray<TCustomAttribute>): Boolean; inline;
    class function  IsDisabledWrite(const Attributes: TArray<TCustomAttribute>): Boolean; inline;
    class function  PropGetterType(Prop: TRttiProperty): TPropertyGetterType;
  public
    class constructor Create;
    class destructor Destroy;
    class function IsGenerics(Cls: TRttiType): Boolean; overload;
    class function IsGenerics(Cls: TClass): Boolean; overload;
    class function IsCollection(Cls: TRttiType): Boolean; overload;
    class function IsCollection(Cls: TClass): Boolean; overload; inline;
    class function GetGenericType(Cls: TClass): TGenericsType;
    class function GetGenericsCreateArgs(Cls: TRttiType): TArray<TValue>;

    // ** Read
    class procedure ReadGeneric(AObject: TObject; IResult: ISuperArray);
    class procedure ReadCollection(ACollection: TCollection; IResult: ISuperArray);
    class procedure ReadObject(AObject: TObject; IResult: ISuperObject);
    class procedure ReadRecord(Info: PTypeInfo; ARecord: Pointer; IResult: ISuperObject);
    class function  ReadRecordEx<T>(Rec: T): ISuperObject;
    class procedure ReadMembers(Data: Pointer; aType: TRttiType; IJsonData: ISuperObject);
    class procedure ReadMember<T, Typ>(Member: Typ; RType: PTypeInfo; MemberValue: TValue; IJsonData: IBaseJSON<T, Typ>);

    class procedure ReadSet(Val: TValue; IJsonData: ISuperArray);
    class procedure ReadVariantOfArray(Val: Variant; IJsonData: ISuperArray);
    class procedure ReadTValueOfArray(Val: TValue; IJsonData: ISuperArray);
    class procedure ReadVariantOfObject(Val: Variant; const Name: String; IJsonData: ISuperObject);

    // ** Write
    class procedure WriteGeneric(AObject: TObject; IData: ISuperArray);
    class procedure WriteCollection(ACollection: TCollection; IData: ISuperArray);
    class procedure WriteObject(AObject: TObject; IData: ISuperObject);
    class procedure WriteRecord(Info: PTypeInfo; ARecord: Pointer; IData: ISuperObject);
    class procedure WriteRecordEx<T>(Rec: T; IData: ISuperObject);
    class procedure WriteMembers(Data: Pointer; aType: TRttiType; IJsonData: ISuperObject);
    class procedure WriteMember<T, Typ>(Data: Pointer; Member: Typ; RType: PTypeInfo; MemberValue: TRttiObject; IJsonData: IBaseJSON<T, Typ>);
    class procedure WriteSet(Data: Pointer; Member: TRttiObject; IJSONData: ISuperArray);
    class procedure SetValue<Typ>(var Data: Pointer; Member: TRttiObject; MIdx: Typ; Val: TValue);
    class function  GetValue<Typ>(Data: Pointer; Member: TRttiObject; MIdx: Typ): TValue;
    class function  GetMemberTypeInfo(Member: TRttiObject; const GetArray: Boolean = true): PTypeInfo; inline;
    class function  GetMemberType(Member: TRttiObject; const GetArray: Boolean = true): TRttiType; //inline;
    class function  GetArrayRawData(Member: TRttiObject): Pointer;
    class procedure SetArrayRawData(Member: TRttiObject; RawData: Pointer);
    class procedure ClearArrayRawData(Member: TRttiObject);

    class function  ObjectConstructorParamCount(Instance: TClass): Integer;
    class function  ObjectConstructor(Instance: TClass): TObject;
    class function  CheckObject<Typ>(Data: Pointer; Member: TRttiObject; MIdx: Typ; var Obj: TObject): Boolean;

    class property  GenericsCache: TObjectDictionary<TClass, TGenericsInfo> read FGenericsCache;
  end;

  TMemberVisibilities = set of TMemberVisibility;
  TSerializeParseOptions = class
  private
    class var FVisibilities: TMemberVisibilities;
  public
    class constructor Create;
    class property Visibilities: TMemberVisibilities read FVisibilities write FVisibilities;
  end;

  TSuperObjectHelper = class helper for TObject
  public
    function AsJSON(const Ident: Boolean = False; const UniversalTime: Boolean = False): String;
    function AsJSONObject: ISuperObject;
    procedure AssignFromJSON(const JSON: String); overload;
    procedure AssignFromJSON(JSON: ISuperObject); overload;
    constructor FromJSON(const JSON: String); overload;
    constructor FromJSON(JSON: ISuperObject); overload;
    constructor FromJSON(const JSON: String; CreateArgs: Array of TValue; const ConstructMethod: String = 'Create'); overload;
    constructor FromJSON(const JSON: ISuperObject; CreateArgs: Array of TValue; const ConstructMethod: String = 'Create'); overload;
  end;

  TBaseSuperRecord<T> = class
  public
    class function AsJSON(Rec: T): String;
    class function AsJSONObject(Rec: T): ISuperObject;
    class function FromJSON(JSON: String): T; overload;
    class function FromJSON(JSON: ISuperObject): T; overload;
  end;
  TSuperRecord<T: Record> = class(TBaseSuperRecord<T>);

  TJSON = class
  public
    class function Parse<T>(const Value: String): T; overload;
    class function Parse<T>(JSON: ISuperObject): T; overload;
    class function Parse<T>(JSON: ISuperArray): T; overload;
    class function SuperObject<T>(Value: T): ISuperObject; overload;
    class function SuperObject(Value: TValue): ISuperObject; overload;
    class function Stringify<T>(Value: T; Indent: Boolean = False; UniversalTime: Boolean = True): String; overload;
    class function Stringify(Value: TValue; Indent: Boolean = False; UniversalTime: Boolean = True): String; overload;
  end;

  function SO(JSON: String = '{}'): ISuperObject; overload;
  function SO(const Args: array of const): ISuperObject; overload;
  function SA(JSON: String = '[]'): ISuperArray; overload;
  function SA(const Args: array of const): ISuperArray; overload;

implementation

var GenericsUnit : String;

function SO(JSON: String): ISuperObject;
begin
  if JSON = '' then JSON := '{}';
  Result := TSuperObject.Create(JSON);
end;

function SO(const Args: array of const): ISuperObject;
var
  I: Integer;
  Members: ISuperArray;
begin
  Result := TSuperObject.Create;
  Members := SA(Args);
  if Odd(Members.Length) then
     Assert(False);
  for I := 0 to (Members.Length div 2) - 1 do
      Result.Add(Members.S[I*2], Members.Ancestor[(I*2)+1]);
end;

function SA(JSON: String): ISuperArray;
begin
  Result := TSuperArray.Create(JSON);
end;

function SA(const Args: array of const): ISuperArray;
var
  I: Integer;
  SArray: ISuperArray;
  SObject: ISuperObject;
begin
  Result := TSuperArray.Create;
  for I := 0 to High(Args) do
      case PVarRec(@Args[I]).VType of
          vtInteger : Result.Add(TJSONInteger.Create(PVarRec(@Args[I]).VInteger));
          vtInt64   : Result.Add(TJSONInteger.Create(PVarRec(@Args[I]).VInt64^));
          vtBoolean : Result.Add(TJSONBoolean.Create(PVarRec(@Args[I]).VBoolean));
          {$IFNDEF NEXTGEN}
          vtChar    : Result.Add(TJSONString.Create(PVarRec(@Args[I]).VWideChar));
          vtString  : Result.Add(TJSONString.Create(String(PVarRec(@Args[I]).VString^)));
          vtPChar   : Result.Add(TJSONString.Create(Char(PVarRec(@Args[I]).VPChar^)));
          vtAnsiString: Result.Add(TJSONString.Create(String(PVarRec(@Args[I]).VAnsiString)));
          {$ENDIF}
          vtWideChar: Result.Add(TJSONString.Create(PVarRec(@Args[I]).VWideChar));
          vtExtended: Result.Add(TJSONFloat.Create(PVarRec(@Args[I]).VExtended^));
          vtCurrency: Result.Add(TJSONFloat.Create(PVarRec(@Args[I]).VCurrency^));
          vtWideString: Result.Add(TJSONString.Create(PWideChar(PVarRec(@Args[I]).VWideString)));
          vtUnicodeString: Result.Add(TJSONString.Create(String(PVarRec(@Args[I]).VUnicodeString)));
          vtInterface:
            if PVarRec(@Args[I]).VInterface = nil then
               Result.Add(TJSONNull.Create(False))
            else if IInterface(PVarRec(@Args[I]).VInterface).QueryInterface(ISuperObject, SObject) = 0 then
                Result.Add(SObject)
            else if IInterface(PVarRec(@Args[I]).VInterface).QueryInterface(ISuperArray, SArray) = 0 then
                Result.Add(SArray)
            else
                Assert(False);
          vtPointer :
            if PVarRec(@Args[I]).VPointer = nil then
               Result.Add(TJSONNull.Create(False))
            else
               Result.Add(TJSONInteger.Create(NativeInt(PVarRec(@Args[I]).VPointer)));
          vtVariant:
            Result.Add(PVarRec(@Args[I]).VVariant^);
          vtObject:
            if PVarRec(@Args[I]).VPointer = nil then
               Result.Add(TJSONNull.Create(False))
            else
               Result.Add(TJSONInteger.Create(NativeInt(PVarRec(@Args[I]).VPointer)));
          vtClass:
            if PVarRec(@Args[I]).VPointer = nil then
               Result.Add(TJSONNull.Create(False))
            else
               Result.Add(TJSONInteger.Create(NativeInt(PVarRec(@Args[I]).VPointer)));
          else
            Assert(false);
      end;
end;

{ TSuperObject }



constructor TBaseJSON<T, Typ>.Create(JSON: String; const CheckDate: Boolean);
type PInterface = ^IInterface;
var
  JVal: IJSONAncestor;
  PIntf: PInterface;
begin
  FCheckDate := CheckDate;
  if (Self.InheritsFrom(TSuperArray)) and (Trim(JSON) = '{}') then JSON := '[]';
  JVal := TJSONObject.ParseJSONValue(JSON, FCheckDate);
  if JVal.QueryInterface(GetTypeData(TypeInfo(T)).Guid, FJSONObj) = S_OK then
     FInterface := TValue.From<T>(FJSONObj).AsInterface
  else
     FCasted := JVal
end;

function TBaseJSON<T, Typ>.GetValue<C>(const Name: Typ): C;
begin
  if Self.InheritsFrom(TSuperObject) then
    with TJSONObject(FInterface).Get(PString(@Name)^) do
       if JsonValue is TJSONNull then
          Result := Nil
       else
          Result := JSonValue as C
  else
  if Self.InheritsFrom(TSuperArray) then
    Result := TJSONArray(FInterface).Get(PInteger(@Name)^) as C
  else
    Result := Nil;
end;

function TBaseJSON<T, Typ>.GetVariant(V: Typ): Variant;
begin
  case GetType(V) of
    varString: Result := S[V];
    varInt64: Result := I[V];
    varDouble: Result := F[V];
    varBoolean: Result := B[V];
    varDate: Result := D[V];
  else
    Result := System.Variants.Null;
  end;
end;

function TBaseJSON<T, Typ>.Member(const Name: Typ): Boolean;
begin
  if Self.InheritsFrom(TSuperObject) then
    Result := Assigned(TJSONObject(FInterface).Get(PString(@Name)^))
  else
    Result := Assigned(TJSONArray(FInterface).Get(PInteger(@Name)^))
end;

procedure TBaseJSON<T, Typ>.Member<MT, TValue>(const Name: Typ; const Value: TValue);
var
  Pair: IJSONPair;
begin
  if Self.InheritsFrom(TSuperObject) then
  begin
    Pair := TJSONObject(FInterface).Get(PString(@Name)^);
    if not Assigned(Pair) then
    begin
      TJSONObject(FInterface).AddPair(PString(@Name)^, DefaultValueClass<MT>(Value) as TJSONAncestor );
      Exit;
    end;
    if Assigned(Pair.JsonValue) then
      Pair.JsonValue := Nil;
    Pair.JsonValue := DefaultValueClass<MT>(Value) as TJSONAncestor;
  end
  else
  begin
    if TJSONArray(FInterface).Count - 1 < PInteger(@Name)^ then
      while TJSONArray(FInterface).Count - 1 < PInteger(@Name)^ do
        TJSONArray(FInterface).Add(DefaultValueClass<MT>(Value) as TJSONAncestor)
    else
      TJSONArray(FInterface).Index[PInteger(@Name)^] := DefaultValueClass<MT>(Value) as TJSONAncestor
  end;
end;

function TBaseJSON<T, Typ>.AsJSON(const Ident, UniversalTime: Boolean): String;
var
  SBuild: TJSONWriter;
begin
  try
    SBuild := TJSONWriter.Create(Ident, UniversalTime);
    if Assigned(FCasted) then
       FCasted.AsJSONString(SBuild)
    else
       TJSONAncestor(FInterface).AsJSONString(SBuild);
    Result := SBuild.ToString;
  finally
    SBuild.Free;
  end;

end;

function TBaseJSON<T, Typ>.Contains(Key: Typ): Boolean;
begin
  Result := GetData(Key) <> Nil;
end;

function TBaseJSON<T, Typ>.ContainsEx(Key: Typ; out Value: IJSONAncestor): Boolean;
begin
  Value := GetData(Key);
  Result := Value <> Nil;
end;

constructor TBaseJSON<T, Typ>.Create(JSON: T; const CheckDate: Boolean = True);
begin
  FJSONObj := JSON;
  FCasted := nil;
  FCheckDate := CheckDate;
  FInterface := TValue.From<T>(JSON).AsInterface;
end;

constructor TBaseJSON<T, Typ>.CreateCasted(Value: IJSONAncestor; const CheckDate: Boolean);
begin
//  FJSONObj := Nil;
  FInterface := Nil;
  FCasted := Value;
  FCheckDate := CheckDate;
end;

constructor TBaseJSON<T, Typ>.CreateWithEscape(JSON: String; const CheckDate: Boolean);
begin
  Create(LimitedStrToUTF16(JSON), CheckDate);
end;

function TBaseJSON<T, Typ>.DefaultValueClass<TT>(const Value): TT;
var
  r: TRttiContext;
  ty: TRttiType;
begin
  if TJSONString.InheritsFrom(TT) then
    Result := TJSONString.Create(String(Value)) as TT
  else if TJSONInteger.InheritsFrom(TT) then
    Result := TJSONInteger.Create(Int64(Value)) as TT
  else if TJSONFloat.InheritsFrom(TT) then
    Result := TJSONFloat.Create(Double(Value)) as TT
  else if TJSONBoolean.InheritsFrom(TT) then
    Result := TJSONBoolean.Create(Boolean(Value)) as TT
  else if TJSONNull.InheritsFrom(TT) then
    Result := TJSONNull.Create(Boolean(Value)) as TT
  else if TJSONDateTime.InheritsFrom(TT) then
    Result := TJSONDateTime.Create(TDateTime(Value)) as TT
  else if TJSONDate.InheritsFrom(TT) then
    Result := TJSONDate.Create(TDate(Value)) as TT
  else if TJSONTime.InheritsFrom(TT) then
    Result := TJSONTime.Create(TTime(Value)) as TT
  else if TJSONRaw.InheritsFrom(TT) then
    Result := TJSONRaw.Create(String(Value)) as TT
  else if TJSONArray.InheritsFrom(TT) then
  begin
    if Pointer(Value) <> Nil then
       Exit(TJSONArray(ISuperArray(Value)) as TT);
    Result := TJSONArray.Create as TT;
  end
  else if TJSONObject.InheritsFrom(TT) then
  begin
    if Pointer(Value) <> Nil then
       Exit(TJSONObject(ISuperObject(Value)) as TT);
    Result := TJSONObject.Create as TT;
  end
  else
  begin
    r := TRttiContext.Create;
    ty := r.GetType(TClass(TT));
    if ty = nil then
      exit(Nil);
    try
      Result := TT(ty.GetMethod('Create').Invoke(ty.AsInstance.MetaclassType, []).AsObject);
    except
      if Assigned(ty) then
        ty.Free;
      raise;
    end;
    r.Free;
  end;
end;

destructor TBaseJSON<T, Typ>.Destroy;
begin
  inherited;
end;

function TBaseJSON<T, Typ>.GetBoolean(V: Typ): Boolean;
begin
  Result := False;
  if Member(V) then
    Result := GetValue<TJSONAncestor>(V).ValueEx<Boolean>;
end;

function TBaseJSON<T, Typ>.GetData(Key: Typ): IJSONAncestor;
var
  P: IJsonPair;
begin
  if Self.InheritsFrom(TSuperObject) then
  begin
     P := TJSONObject(FInterface).Get(PString(@Key)^);
     if Assigned(P) then
        Result := P.JsonValue
     else
        Result := Nil
  end
  else
  if Self.InheritsFrom(TSuperArray) then
     Result := TJSONArray(FInterface).Get(PInteger(@Key)^);
end;

function TBaseJSON<T, Typ>.GetDataType: TDataType;
var
  Cast: ICast;
begin
  if TValue.From<T>(FJSONObj).AsInterface <> nil then
     Cast := TCast.CreateFrom<T>(FJSONObj)
  else
  if Assigned(FCasted)  then
     Cast := TCast.Create(FCasted)
  else
     Exit(dtNil);
  Result := Cast.DataType
end;

function TBaseJSON<T, Typ>.GetDate(V: Typ): TDate;
begin
  Result := 0;
  if Member(V) then
     Result := GetValue<TJSONDate>(V).Value;
end;

function TBaseJSON<T, Typ>.GetDateTime(V: Typ): TDateTime;
begin
  Result := 0;
  if Member(V) then
     Result := GetValue<TJSONDateTime>(V).Value;
end;

function TBaseJSON<T, Typ>.GetDouble(V: Typ): Double;
begin
  Result := 0;
  if Member(V) then
     if GetType(V) = varInt64 then
        Result := GetValue<TJSONInteger>(V).ValueEx<Int64>
     else
        Result := GetValue<TJSONFloat>(V).ValueEx<Double>;
end;

function TBaseJSON<T, Typ>.GetInteger(V: Typ): Int64;
begin
  Result := 0;
  if Member(V) then
    Result := GetValue<TJSONInteger>(V).ValueEx<Int64>;
end;

function TBaseJSON<T, Typ>.GetNull(V: Typ): TMemberStatus;
var
  Val: IJSONAncestor;
begin
  if ContainsEx(V, Val) then begin
     if Val is TJSONNull then
        Result := jNull
     else
        Result := jAssigned
  end else
        Result := jUnAssigned;
end;

function TBaseJSON<T, Typ>.GetArray(V: Typ): ISuperArray;
var
  J: IJSONArray;
begin
  Result := Nil;
  if not Member(V) then
     Member<TJSONArray, Pointer>(V, nil);
  J := GetValue<TJSONArray>(V);
  Result := TSuperArray.Create(J);
end;

function TBaseJSON<T, Typ>.GetObject(V: Typ): ISuperObject;
begin
  Result := Nil;
  if not Member(V) then
    Member<TJSONObject, Pointer>(V, Nil);

  Result := TSuperObject.Create(GetValue<TJSONObject>(V));
end;

function TBaseJSON<T, Typ>.GetAncestor(V: Typ): IJSONAncestor;
begin
  Result := GetData(V);
end;

function TBaseJSON<T, Typ>.GetString(V: Typ): String;
label
  JMP;
begin
  Result := '';
  if Member(V) then
     if FCheckDate then
        case Ancestor[V].DataType of
          dtDateTime : Result := GetValue<TJSONDateTime>(V).GetAsString;
          dtDate     : Result := GetValue<TJSONDate>(V).GetAsString;
          dtTime     : Result := GetValue<TJSONTime>(V).GetAsString;
          else
            goto JMP;
        end
     else
        JMP: Result := GetValue<TJSONString>(V).ValueEx<String>;
end;

function TBaseJSON<T, Typ>.GetTime(V: Typ): TTime;
begin
  Result := 0;
  if Member(V) then
     Result := GetValue<TJSONTime>(V).Value;
end;

function TBaseJSON<T, Typ>.GetType(Key: Typ): TVarType;
var
  Temp: IJSONAncestor;
begin
  Temp := GetData(Key);
  if Temp = Nil then
     Result := varUnknown
  else if Temp is TJSONString then
     Result := varString
  else if Temp is TJSONFloat then
     Result := varDouble
  else if Temp is TJSONInteger then
      Result := varInt64
  else if Temp is TJSONNull then
     Result := varNull
  else if Temp is TJSONObject then
     Result := varObject
  else if Temp is TJSONArray then
     Result := varArray
  else if Temp is TJSONBoolean then
     Result := varBoolean
  else if (Temp is TJSONDateTime) or (Temp is TJSONDate) or (Temp is TJSONTime) then
     Result := varDate
end;

procedure TBaseJSON<T, Typ>.SetArray(V: Typ; const Value: ISuperArray);
begin
  Member<TJSONArray, IJSONArray>(V, Value.Self )
end;

procedure TBaseJSON<T, Typ>.SetBoolean(V: Typ; const Value: Boolean);
begin
  Member<TJSONBoolean, Boolean>(V, Value)
end;

procedure TBaseJSON<T, Typ>.SetDate(V: Typ; const Value: TDate);
begin
  Member<TJSONDate, TDate>(V, Value);
end;

procedure TBaseJSON<T, Typ>.SetDateTime(V: Typ; const Value: TDateTime);
begin
  Member<TJSONDateTime, TDateTime>(V, Value);
end;

procedure TBaseJSON<T, Typ>.SetDouble(V: Typ; const Value: Double);
begin
  Member<TJSONFloat, Double>(V, Value);
end;

procedure TBaseJSON<T, Typ>.SetInteger(V: Typ; const Value: Int64);
begin
  Member<TJSONInteger, Int64>(V, Value);
end;

procedure TBaseJSON<T, Typ>.SetNull(V: Typ; const Value: TMemberStatus);
begin
end;

procedure TBaseJSON<T, Typ>.SetObject(V: Typ; const Value: ISuperObject);
begin
  Member<TJSONObject, IJSONObject>(V, Value.Self )
end;

procedure TBaseJSON<T, Typ>.SetString(V: Typ; const Value: String);
var
  Anc: IJSONAncestor;
  dT: TDateTime;
  ValType: TDataType;
label
  JMP, JERR;
begin
  if FCheckDate then
  begin
     Anc := Ancestor[V];
     if Assigned(Anc) and (Anc.DataType in [dtDateTime..dtTime]) then
     begin
        if not TJSONDateManager.Check(Value, dT, ValType ) then
           JERR: raise SOInvalidDate.Create('Invalid date format.')
        else
           case ValType of
             dtDateTime: Member<TJSONDateTime, TDateTime>(V, dT);
             dtDate: Member<TJSONDate, TDate>(V, TDate(dT));
             dtTime: Member<TJSONTime, TTime>(V, TTime(dT));
             else
               goto JERR;
           end;
     end
     else
       goto JMP;
  end
  else
     JMP: Member<TJSONString, String>(V, Value);
end;

procedure TBaseJSON<T, Typ>.SetTime(V: Typ; const Value: TTime);
begin
  Member<TJSONTime, TTime>(V, Value);
end;

procedure TBaseJSON<T, Typ>.SetVariant(V: Typ; const Value: Variant);
var
  VTyp: TVarType;
begin
  if VarIsNull(Value) then
     Null[V] := jNull
  else
  begin
     VTyp := GetType(V);
     if VTyp = varUnknown then
        VTyp := VarType(Value);
     case VTyp of
       varString, varUString:
          S[V] := Value;
       varInt64, varInteger, varByte:
          I[V] := Value;
       varDouble, varCurrency:
          F[V] := Value;
       varBoolean:
          B[V] := Value;
       varDate:
          D[V] := Value;
       varNull:
          Null[V] := jNull;
     end;
  end;
end;

function TBaseJSON<T, Typ>.GetSelf: T;
begin
  Result := FJSONObj;
end;


{ TSuperObject }

function TSuperObject.AsArray: ISuperArray;
begin
  if not Assigned(FCasted) or not (FCasted is TJSONArray) then
     Exit(Nil);
  Result := TSuperArray.Create(IJSONArray(FCasted));
end;

function TSuperObject.AsObject: ISuperObject;
begin
  Result := Self;
end;

function TSuperObject.Check(const Expr: String): Boolean;
var
  IExpr: ISuperExpression;
begin
  IExpr :=  TSuperExpression.Create(FJSONObj, Expr, True);
  Result := IExpr.DataType <> dtNil;
end;

function TSuperObject.AsType<T>: T;
begin
  Result := TJSON.Parse<T>(Self);
end;

function TSuperObject.Cast: ICast;
begin
  if Assigned(FCasted) then
     Result := TCast.Create(FCasted)
  else
     Result := TCast.Create(FJSONObj);
end;

function TSuperObject.Clone: ISuperObject;
begin
  Result := SO(AsJSON);
end;

function TSuperObject.Delete(const Cond: TCondCallBack<IMember>): ISuperObject;
var
  Member: IJSONPair;
begin
  Result := Self;
  if not Assigned(Cond) then
     Exit;
  for Member in FJSONObj do
      if Cond(TCast.Create(Member)) then
         Result.Self.Remove(Member);
end;

procedure TSuperObject.First;
begin
  FOffset := 0;
end;

function TSuperObject.GetCount: Integer;
begin
  Result := FJSONObj.Count;
end;

function TSuperObject.GetCurrentKey: String;
begin
  Result := FJSONObj.Get(FOffset).Name;
end;

function TSuperObject.GetCurrentValue: IJSONAncestor;
begin
  Result := FJSONObj.Get(FOffset).JsonValue;
end;

function TSuperObject.GetEnumerator: TSuperEnumerator<IJSONPair>;
begin
  Result.Index := -1;
  Result.List := TJSONObject(FJSONObj).GetEnumerator
end;

function TSuperObject.GetEoF: Boolean;
begin
  Result := FOffset > Count - 1;
end;

function TSuperObject.GetExpr(const Code: String): ISuperExpression;
begin
  Result := TSuperExpression.Create(FJSONObj, Code);
end;


function TSuperObject.GetOffset: Integer;
begin
  Result := FOffset;
end;

function TSuperObject.GetRaw(V: String): String;
begin
  Result := GetValue<TJSONRaw>(V).ValueEx<String>;
end;

function TSuperObject.GetString(V: String): String;
begin
  Result := inherited GetString(V);
end;

procedure TSuperObject.Next;
begin
  Inc(FOffset);
end;

class function TSuperObject.ParseFile(FileName: String; CheckDate: Boolean = True): TSuperObject;
var
  Strm: TFileStream;
begin
  Strm := TFileStream.Create(FileName, fmOpenRead, fmShareDenyWrite);
  try
    Result := ParseStream(Strm, CheckDate);
  finally
    Strm.Free;
  end;
end;

class function TSuperObject.ParseStream(Stream: TStream; CheckDate: Boolean): TSuperObject;
var
  Strm: TStringStream;
  preamble, tmp: TBytes;
  preambleLength: integer;
  enc: TEncoding;
begin
  Strm := TStringStream.Create;
  try
    Strm.LoadFromStream(Stream);
    SetLength(tmp,10);
    strm.read(tmp, 10);
    enc := nil;
    preambleLength := TEncoding.GetBufferEncoding(tmp, enc);
    if preambleLength <> 0 then
      Result := TSuperObject.Create(enc.GetString(strm.Bytes, preambleLength, stream.Size - preambleLength), CheckDate)
    else
      Result := TSuperObject.Create(Strm.Datastring, CheckDate);
  finally
    Strm.Free;
  end;
end;

procedure TSuperObject.Remove(Key: String);
begin
  FJSONObj.Remove(Key);
end;

procedure TSuperObject.SaveTo(Stream: TStream; const Ident, UniversalTime: Boolean);
var
  S: TStringStream;
begin
  S := TStringStream.Create( AsJSON(Ident, UniversalTime) );
  try
     S.SaveToStream(Stream);
  finally
     S.Free;
  end;
end;

procedure TSuperObject.SaveTo(AFile: String; const Ident, UniversalTime: Boolean);
var
  S: TStringStream;
begin
  S := TStringStream.Create( AsJSON(Ident, UniversalTime) );
  try
     S.SaveToFile(AFile);
  finally
     S.Free;
  end;
end;

procedure TSuperObject.Add(const Key: String; const Data: IJSONAncestor);
begin
  FJSONObj.AddPair(Key, Data);
end;

procedure TSuperObject.SetData(V: String; Data: Variant; AFormatSettings: TFormatSettings);
begin
  case VarType(Data) of
    varNull:
        FJSONObj.AddPair(V, TJSONNull.Create(True));

    varDate:
        FJSONObj.AddPair(V, TJSONString.Create(DateTimeToStr(TDateTime(Data), AFormatSettings)));

    varInteger:
        FJSONObj.AddPair(V, TJSONInteger.Create(Integer(Data)));

    varBoolean:
        FJSONObj.AddPair(V, TJSONBoolean.Create(Data));

    varString, varUString:
        FJSONObj.AddPair(V, TJSONString.Create(String(Data)));

    varDouble:
        FJSONObj.AddPair(V, TJSONFloat.Create(Double(Data)));

    vtCurrency:
        FJSONObj.AddPair(V, TJSONFloat.Create(Currency(Data)));

    varInt64: FJSONObj.AddPair(V, TJSONInteger.Create(Int64(Data)));
  end;
end;

procedure TSuperObject.SetNull(V: String; const Value: TMemberStatus);
var
  Val: IJSONAncestor;
begin
  if Value = jAssigned then
     Exit;
  with TJSONObject(FJSONObj) do begin
       if ContainsEx(V, Val) then
       begin
          case Value of
            jUnAssigned:
              Remove(V);
            jNull: begin
              Remove(V);
              AddPair(V, TJSONNull.Create(True));
            end;
          end;
       end
       else
          AddPair(V, TJSONNull.Create(True));
  end;
end;

procedure TSuperObject.SetRaw(V, Value: String);
begin
  Member<TJSONRaw, String>(V, Value);
end;

procedure TSuperObject.Sort(Comparison: TJSONComparison<IMember>);
begin
  if not Assigned(Comparison) then Exit;
  FJSONObj.Sort(function(Left, Right: IJSONPair): Integer
  begin
    Result := Comparison(TCast.Create(Left), TCast.Create(Right));
  end);
end;

function TSuperObject.T: TSuperObject;
begin
  Result := Self;
end;

function TSuperObject.Where(const Cond: TCondCallBack<IMember>): ISuperObject;
var
  Member: IJSONPair;
begin
  Result := TSuperObject.Create('{}', FCheckDate);
  if not Assigned(Cond) then
     Exit;
  for Member in FJSONObj do
      if Cond(TCast.Create(Member)) then
         Result.Self.AddPair(Member);
end;

procedure TSuperObject.SetData(V: String; Data: Variant);
begin
  SetData(V, Data, FormatSettings);
end;

{ TSuperArray }

procedure TSuperArray.Add(Value: Variant; DateFormat: TFormatSettings);
begin
  if VarIsNull(Value) then
  begin
    TJSONArray(FJSONObj).Add(TJSONNull.Create(True));
    Exit;
  end;

  case VarType(Value) of
    varDate :
       TJSONArray(FJSONObj).Add(TJSONString.Create(DateTimeToStr(TDateTime(Value), DateFormat)));

    varBoolean:
       TJSONArray(FJSONObj).Add(TJSONBoolean.Create(Value));

    else
      with TValue.FromVariant(Value) do
          case Kind of
             tkInteger, tkInt64:
                TJSONArray(FJSONObj).Add(TJSONInteger.Create(Int64(Value)));

             tkFloat:
                TJSONArray(FJSONObj).Add(TJSONFloat.Create(Double(Value)));

             tkString, tkWChar, tkLString, tkWString, tkUString, tkChar:
                TJSONArray(FJSONObj).Add(TJSONString.Create(Value));
          end;
  end;

end;

procedure TSuperArray.Add(Value: Variant);
begin
  Add(Value, FormatSettings);
end;

function TSuperArray.AsArray: ISuperArray;
begin
  Result := Self;
end;

function TSuperArray.AsObject: ISuperObject;
begin
  Result := TSuperObject.CreateCasted(FJSONObj);
end;

function TSuperArray.AsType<T>: T;
var
  Ctx: TRttiContext;
  Typ: TRttiType;
begin
  Ctx := TRttiContext.Create;
  try
    Typ := Ctx.GetType(TypeInfo(T));
    if not Assigned(Typ) then
       Exit;
    if Typ.IsInstance then begin
       if TSerializeParse.IsGenerics(Typ) then begin
          Result := TValue.From<TObject>(TSerializeParse.ObjectConstructor(Typ.AsInstance.MetaclassType)).AsType<T>;
          TSerializeParse.WriteGeneric(TValue.From<T>(Result).AsObject, Self);
          Exit;
       end else if TSerializeParse.IsCollection(Typ) then begin
          Result := TValue.From<TObject>(TSerializeParse.ObjectConstructor(Typ.AsInstance.MetaclassType)).AsType<T>;
          TSerializeParse.WriteCollection(TValue.From<T>(Result).AsObject as TCollection, Self);
          Exit;
       end;
    end;
    raise SOException.Create('Unsupported type.');
  except
    Ctx.Free;
    raise;
  end;
end;

procedure TSuperArray.Add(Value: IJSONAncestor);
begin
  TJSONArray(FJSONObj).Add(Value);
end;

procedure TSuperArray.Clear;
begin
  FJSONObj.Clear;
end;

function TSuperArray.Clone: ISuperArray;
begin
  Result := SA(AsJSON);
end;

function TSuperArray.Delete(const Cond: TCondCallBack<IMember>): ISuperArray;
var
  Member: IJSONAncestor;
begin
  Result := Self;
  if not Assigned(Cond) then
     Exit;
  for Member in FJSONObj do
      if Cond(TCast.Create(Member)) then
         Result.Self.Remove(Member);
end;

procedure TSuperArray.Delete(Index: Integer);
begin
  TJsonArray(FJSONObj).Remove(Index);
end;

function TSuperArray.GetEnumerator: TSuperEnumerator<IJSONAncestor>;
begin
  Result.Index := -1;
  Result.List := TJSONArray(FJSONObj).GetEnumerator
end;

function TSuperArray.GetLength: Integer;
begin
  Result := TJSONArray(FJSONObj).Count;
end;

procedure TSuperArray.SaveTo(Stream: TStream; const Ident, UniversalTime: Boolean);
var
  S: TStringStream;
begin
  S := TStringStream.Create( AsJSON(Ident, UniversalTime) );
  try
     S.SaveToStream(Stream);
  finally
     S.Free;
  end;
end;

procedure TSuperArray.SaveTo(AFile: String; const Ident, UniversalTime: Boolean);
var
  S: TStringStream;
begin
  S := TStringStream.Create( AsJSON(Ident, UniversalTime) );
  try
     S.SaveToFile(AFile);
  finally
     S.Free;
  end;
end;

procedure TSuperArray.SetNull(V: Integer; const aValue: TMemberStatus);
var
  Val: IJSONAncestor;
begin
  if aValue = jAssigned then
     Exit;
  with FJSONObj do begin
       if ContainsEx(V, Val) then
       begin
          case aValue of
            jUnAssigned:
              Remove(V);
            jNull: begin
              Index[V] := TJSONNull.Create(True);
            end;
          end;
       end
       else
          Member<TJSONNull, Boolean>(V, True);
  end;
end;

procedure TSuperArray.Sort(Comparison: TJSONComparison<IMember>);
begin
  if not Assigned(Comparison) then Exit;
  FJSONObj.Sort(function(Left, Right: IJSONAncestor): Integer
  begin
     Result := Comparison(TCast.Create(Left), TCast.Create(Right));
  end);
end;

function TSuperArray.T: TSuperArray;
begin
  Result := Self;
end;

function TSuperArray.Where(const Cond: TCondCallBack<IMember>): ISuperArray;
var
  Member: IJSONAncestor;
begin
  Result := TSuperArray.Create('[]', FCheckDate);
  if not Assigned(Cond) then
     Exit;
  for Member in FJSONObj do
      if Cond(TCast.Create(Member)) then
         Result.Self.Add(Member);
end;

procedure TSuperArray.Add(Value: ISuperObject);
begin
  Add(Value.Self);
end;

procedure TSuperArray.Add(Value: ISuperArray);
begin
  Add(Value.Self);
end;

{ TSuperObjectHelper }

function TSuperObjectHelper.AsJSON(const Ident: Boolean = False; const UniversalTime: Boolean = False): String;
begin
  Result := AsJSONObject.AsJSON(Ident, UniversalTime);
end;

constructor TSuperObjectHelper.FromJSON(const JSON: String);
begin
  FromJSON(JSON, []);
end;

function TSuperObjectHelper.AsJSONObject: ISuperObject;
var
  IResult: ISuperObject;
begin
  try
    IResult := TSuperObject.Create;
    TSerializeParse.ReadObject(Self, IResult);
  finally
    Result := IResult;
  end;
end;

procedure TSuperObjectHelper.AssignFromJSON(const JSON: String);
begin
  TSerializeParse.WriteObject(Self, SO(JSON));
end;

procedure TSuperObjectHelper.AssignFromJSON(JSON: ISuperObject);
begin
  TSerializeParse.WriteObject(Self, JSON);
end;

constructor TSuperObjectHelper.FromJSON(const JSON: String; CreateArgs: array of TValue; const ConstructMethod: String);
var
  IData: ISuperObject;
begin
  IData := TSuperObject.Create(JSON);
  FromJSON(IData, CreateArgs, ConstructMethod);
end;

constructor TSuperObjectHelper.FromJSON(const JSON: ISuperObject; CreateArgs: array of TValue; const ConstructMethod: String);
var
  Ctx: TRttiContext;
  Typ: TRttiType;
  Method: TRttiMethod;
begin
  Ctx := TRttiContext.Create;
  try
    Typ := Ctx.GetType(ClassType);
    if not Assigned(Typ) then Exit;
    Method := Typ.GetMethod(ConstructMethod);
    if (not Assigned(Method)) or not Method.IsConstructor then Exit;
    Method.Invoke(Self, CreateArgs);
  finally
    Ctx.Free;
    TSerializeParse.WriteObject(Self, JSON);
  end;
end;

constructor TSuperObjectHelper.FromJSON(JSON: ISuperObject);
begin
  FromJSON(JSON, []);
end;

{ TSerializeParse }

class procedure TSerializeParse.ReadMembers(Data: Pointer; aType: TRttiType; IJsonData: ISuperObject);
var
  Prop: TRttiProperty;
  Field: TRttiField;
  MemberName: String;
  RevalAttribute: REVAL;
  Value: TValue;
  Attributes: TArray<TCustomAttribute>;
begin
  for Prop in aType.GetProperties do
  begin
    if (not (Prop.Visibility in TSerializeParseOptions.Visibilities))
       {$IFDEF AUTOREFCOUNT} or (Prop.Parent.AsInstance.MetaclassType = TObject){$ENDIF}
       or (Prop.Parent.AsInstance.MetaclassType = TCollectionItem) then Continue;

    MemberName := Prop.Name;
    Attributes := Prop.GetAttributes;
    // * Read Disable
    if IsDisabled(Attributes) or IsDisabledWrite(Attributes) then
       Continue;

    // * Read Alias Name
       GetAliasName(Attributes, MemberName);

    Value := Prop.GetValue(Data);

    // * Read Reval Attribute
       RevalAttribute := GetREVAL(Attributes);
       if (RevalAttribute <> Nil) and (RevalAttribute.CheckEQ(Value)) then
           Value := TValue.FromVariant(RevalAttribute.Value);

    ReadMember<IJSONObject, String>(MemberName, Prop.PropertyType.Handle, Value, IJSonData);
  end;

  for Field in aType.GetFields do
  begin
    if not (Field.Visibility in TSerializeParseOptions.Visibilities) then Continue;

    MemberName := Field.Name;
    Attributes := Field.GetAttributes;
    // * Read Disable
    if IsDisabled(Attributes) or IsDisabledWrite(Attributes) then
       Continue;

    // * Read Alias Name
      GetAliasName(Field.GetAttributes, MemberName);

    Value := Field.GetValue(Data);

    // * Read Reval Attribute
       RevalAttribute := GetREVAL(Field.GetAttributes);
       if (RevalAttribute <> Nil) and (RevalAttribute.CheckEQ(Value)) then
           Value := TValue.FromVariant(RevalAttribute.Value);

    ReadMember<IJSONObject, String>(MemberName, Field.FieldType.Handle, Value, IJSonData);
  end;
end;

class procedure TSerializeParse.ReadObject(AObject: TObject; IResult: ISuperObject);
var
  Ctx: TRttiContext;
  Typ: TRttiType;
begin
  Ctx := TRttiContext.Create;
  try
    Typ := Ctx.GetType(AObject.ClassType);
    if not Assigned(Typ) then Exit;
    ReadMembers(AObject, Typ, IResult) ;
  finally
    Ctx.Free;
  end;
end;

class procedure TSerializeParse.ReadRecord(Info: PTypeInfo; ARecord: Pointer; IResult: ISuperObject);
var
  Ctx: TRttiContext;
  Typ: TRttiRecordType;
begin
  Ctx := TRttiContext.Create;
  try
    Typ := Ctx.GetType(Info).AsRecord;
    if not Assigned(Typ) then Exit;
    ReadMembers(ARecord, Typ, IResult) ;
  finally
    Ctx.Free;
  end;
end;

class function TSerializeParse.ReadRecordEx<T>(Rec: T): ISuperObject;
var
  IResult: ISuperObject;
begin
  try
    IResult := TSuperObject.Create;
    with TValue.From<T>(Rec) do
      ReadRecord(TypeInfo, GetReferenceToRawData, IResult);
  finally
    Result := IResult;
  end;
end;

class function TSerializeParse.CheckObject<Typ>(Data: Pointer;
  Member: TRttiObject; MIdx: Typ; var Obj: TObject): Boolean;
var
  rtype: TRttiType;
  rawData: Pointer;
  Val, ArrVal: TValue;
begin
  Obj := Nil;
  rawData := GetArrayRawData(Member);
  rtype := GetMemberType(Member);
   if rawData <> nil then
  begin
    Obj := GetValue<Typ>(rawData, Member, MIdx).AsObject;
    if (Obj = Nil) and (ObjectConstructorParamCount(rtype.AsInstance.MetaclassType) = 0 ) then
    begin
      Obj := ObjectConstructor(rtype.AsInstance.MetaclassType);
      TValue.Make(@Obj, rtype.Handle , Val);
      if Member.ClassType = TRttiDynamicArrayType then begin
         TValue.Make(rawData, TRttiDynamicArrayType(Member).Handle, ArrVal);
         rawData := ArrVal.GetReferenceToRawArrayElement(PInteger(@MIdx)^)

      end else if Member.ClassType = TRttiArrayType then begin
         TValue.Make(rawData, TRttiArrayType(Member).Handle, ArrVal);
         rawData := ArrVal.GetReferenceToRawArrayElement(PInteger(@MIdx)^)

      end;

      SetValue<Typ>(rawData, Member, MIdx, Val);
    end;
  end
  else
  begin
    Obj := GetValue<String>(Data, Member, '').AsObject;
    if (Obj = Nil) and (ObjectConstructorParamCount(rtype.AsInstance.MetaclassType) = 0 ) then
    begin
      Obj := ObjectConstructor(rtype.AsInstance.MetaclassType);
      TValue.Make(@Obj, rtype.Handle , Val);
      SetValue<String>(Data, Member, '', Val);
    end;
  end;
  Result := Obj <> nil;
end;

class procedure TSerializeParse.ClearArrayRawData(Member: TRttiObject);
begin
  SetArrayRawData(Member, Nil);
end;

class constructor TSerializeParse.Create;
begin
  FGenericsCache := TObjectDictionary<TClass, TGenericsInfo>.Create([doOwnsValues]);
end;

class destructor TSerializeParse.Destroy;
begin
  FGenericsCache.Free;
end;

{$WARNINGS OFF}
class function TSerializeParse.GetArrayRawData(Member: TRttiObject): Pointer;
begin
  if Member is TRttiProperty  then
     Result := TSuperProperty(Member).ArrayRawData

  else if Member is TRttiField then
     Result := TSuperField(Member).ArrayRawData

  else if Member is TRttiDynamicArrayType then
     Result := TSuperDynArr(Member).ArrayRawData

  else if Member is TRttiArrayType then
     Result := TSuperArr(Member).ArrayRawData;

end;
{$WARNINGS ON}

class function TSerializeParse.GetAttribute(AttributeType: TAttributeClass; Attributes: TArray<TCustomAttribute>): TCustomAttribute;
var
  Attr: TCustomAttribute;
begin
  for Attr in Attributes do
      if Attr is AttributeType then
         Exit(Attr);
  Result := Nil;
end;

class function TSerializeParse.GetGenericsCreateArgs(Cls: TRttiType): TArray<TValue>;
var
  Info: TGenericsInfo;
begin
  SetLength(Result, 0);
  if FGenericsCache.TryGetValue(Cls.AsInstance.MetaclassType, Info) then
     Result := Info.CreateArgs
  else
  if Cls.AsInstance.MetaclassType.InheritsFrom(TStringStream) then
  begin
     SetLength(Result, 3);
     Result[0] := TValue.From<String>('');
     Result[1] := TValue.From<TEncoding>(TEncoding.UTF8);
     Result[2] := TValue.From<Boolean>(True)
  end;

end;

class function TSerializeParse.GetGenericType(Cls: TClass): TGenericsType;
var
  Temp: String;
begin
  Temp := Cls.ClassName;
  if Copy(Temp, 1, 6) = 'TList<' then
     Result := gtList
  else if Copy(Temp, 1, 12) = 'TObjectList<' then
     Result := gtObjectList
  else
     Result := gtNil
end;

{$WARNINGS OFF}
class function TSerializeParse.GetMemberType(Member: TRttiObject; const GetArray: Boolean): TRttiType;
begin
  if Member is TRttiProperty  then begin
     Result := TRttiProperty(Member).PropertyType;
     if GetArray and (TSuperProperty(Member).ArrayRawData <> Nil) then
        if Result is TRttiArrayType then
           Result := TRttiArrayType(Result).ElementType
        else
           Result := TRttiDynamicArrayType(Result).ElementType;

  end else if Member is TRttiField then begin
     Result := TRttiField(Member).FieldType;
     if GetArray and (TSuperField(Member).ArrayRawData <> Nil) then
        if Result is TRttiArrayType then
           Result := TRttiArrayType(Result).ElementType
        else
           Result := TRttiDynamicArrayType(Result).ElementType;

  end else if Member is TRttiDynamicArrayType then begin
     Result := TRttiDynamicArrayType(Member).ElementType

  end else if Member is TRttiArrayType then begin
     Result := TRttiArrayType(Member).ElementType

  end;
end;
{$WARNINGS ON}

class function TSerializeParse.GetMemberTypeInfo(
  Member: TRttiObject; const GetArray: Boolean): PTypeInfo;
begin
  Result := GetMemberType(Member, GetArray).Handle
end;

class function TSerializeParse.GetREVAL(const Attributues: TArray<TCustomAttribute>): REVAL;
begin
  Result := REVAL(GetAttribute(REVAL, Attributues));
end;

class function TSerializeParse.PropGetterType(Prop: TRttiProperty): TPropertyGetterType;
var
  Getter: Pointer;
begin
  if Prop is TRttiInstanceProperty then begin
     Getter := TRttiInstanceProperty(Prop).PropInfo^.GetProc;
     if (IntPtr(Getter) and PROPSLOT_MASK) <> PROPSLOT_FIELD then
        Exit(pgtMethod);
  end;
  Result := pgtField;
end;

class function TSerializeParse.GetValue<Typ>(Data: Pointer;
  Member: TRttiObject; MIdx: Typ): TValue;
begin
  if (TypeInfo(Typ) = TypeInfo(Integer) ) and ( GetMemberTypeInfo(Member, False).Kind in [tkDynArray, tkArray] ) then
      Result := GetValue<String>(GetArrayRawData(Member), Member, '').GetArrayElement(PInteger(@MIdx)^)

  else if Member is TRttiProperty  then begin
     if (TRttiProperty(Member).PropertyType.Handle.Kind = tkDynArray) and (PropGetterType(TRttiProperty(Member)) = pgtMethod) then begin
        TValue.Make(Nil, TRttiProperty(Member).PropertyType.Handle, Result);
        Exit;
     end;
     Result := TRttiProperty(Member).GetValue(Data)

  end else if Member is TRttiField then
     Result := TRttiField(Member).GetValue(Data)

  else if Member is TRttiDynamicArrayType then begin
     TValue.Make(GetArrayRawData(Member), TRttiDynamicArrayType(Member).Handle, Result);
     Result := Result.GetArrayElement(PInteger(@MIdx)^);
  end;
end;

class function TSerializeParse.IsCollection(Cls: TRttiType): Boolean;
begin
  if Cls = Nil then Exit(False);
  Result := Cls.AsInstance.MetaclassType.InheritsFrom(TCollection);
end;

class function TSerializeParse.IsCollection(Cls: TClass): Boolean;
begin
  with TRttiContext.Create do
    try
      Result := IsCollection(GetType(Cls));
    finally
      Free;
    end;
end;

class function TSerializeParse.IsDisabled(const Attributes: TArray<TCustomAttribute>): Boolean;
begin
  Result := GetAttribute(DISABLE, Attributes) <> Nil;
end;

class function TSerializeParse.IsDisabledRead(const Attributes: TArray<TCustomAttribute>): Boolean;
begin
  Result := GetAttribute(DISABLEREAD, Attributes) <> Nil;
end;

class function TSerializeParse.IsDisabledWrite(const Attributes: TArray<TCustomAttribute>): Boolean;
begin
  Result := GetAttribute(DISABLEWRITE, Attributes) <> Nil;
end;

class function TSerializeParse.IsGenerics(Cls: TClass): Boolean;
var
  Info: TGenericsInfo;
  ctx: TRttiContext;
  typ: TRttiType;
begin
  if FGenericsCache.TryGetValue(Cls, Info) then
     Result := Info.IsGeneric
  else
  begin
     Result := False;
     ctx := TRttiContext.Create;
     try
        typ := ctx.GetType(Cls);
        if typ <> Nil then
           Result := IsGenerics(typ)
     finally
       ctx.Free;
     end;
  end;
end;

class function TSerializeParse.IsGenerics(Cls: TRttiType): Boolean;
var
  C: TClass;
  Mtd: TRttiMethod;
  Info: TGenericsInfo;
  Gt: TGenericsType;
begin
  Result := False;
  C := Cls.AsInstance.MetaclassType;
  if FGenericsCache.TryGetValue(C, Info) then
     Exit(Info.IsGeneric);

  if C.UnitName = GenericsUnit then begin
     Gt := GetGenericType(C);
     if Gt in [gtList, gtObjectList] then begin
        Mtd := Cls.GetMethod('First');
        if (Mtd <> Nil) and (Mtd.MethodKind = mkFunction) then begin // TList<> or TObjectList<>
           Info := TGenericsInfo.Create(C, True, Mtd.ReturnType);
           if Gt = gtObjectList then begin
              SetLength(Info.CreateArgs, 1);
              Info.CreateArgs[0] := True;
           end;
           FGenericsCache.Add(C, Info);
           Exit(True);
        end
     end;
  end;
end;

class function TSerializeParse.ObjectConstructor(
  Instance: TClass): TObject;
var
  Ctx: TRttiContext;
  Typ: TRttiType;
  Mtd: TRttiMethod;
  function InEncoding(List: TArray<TRttiParameter>): Boolean;
  var
    I: Integer;
  begin
     if Length(List) <> 3 then Exit(False);
     for I := 0 to High(List) do
         if (List[I].ParamType.TypeKind = tkClass) and (List[I].ParamType.AsInstance.MetaclassType.InheritsFrom(TEncoding)) then
             Exit(True);
     Result := False;
  end;
begin
  Ctx := TRttiContext.Create;
  try
    Typ := Ctx.GetType(Instance);
    Mtd := nil;
    {$IFNDEF Android or IOS}
      if Instance.InheritsFrom(TStringStream) then begin
        for Mtd in Typ.GetMethods do
           if (CompareText(Mtd.Name, 'Create') = 0) and InEncoding(Mtd.GetParameters) then
              Break;
       Assert(Assigned(Mtd));
    end
    else
    {$ENDIF}
      Mtd := Typ.GetMethod('Create');

    Result := Mtd.Invoke(Instance, GetGenericsCreateArgs(Typ)).AsObject;
  finally
    Ctx.Free;
  end;
end;

class function TSerializeParse.ObjectConstructorParamCount(
  Instance: TClass): Integer;
var
  Ctx: TRttiContext;
  Typ: TRttiType;
  Mtd: TRttiMethod;
begin
  Result := -1;
  Ctx := TRttiContext.Create;
  try
    Typ := Ctx.GetType(Instance);
    if IsGenerics(Typ) then
       Exit(0);
    if not Assigned(Typ) then Exit;
    Mtd := Typ.GetMethod('Create');
    if not Assigned(Mtd) then Exit;
    Result := Length( Mtd.GetParameters );
  finally
    Ctx.Free;
  end;
end;

class procedure TSerializeParse.ReadCollection(ACollection: TCollection; IResult: ISuperArray);
var
  I: Integer;
  Item: TCollectionItem;
begin
  for I := 0 to ACollection.Count - 1 do
  begin
    Item := ACollection.Items[I];
    if Item <> Nil then
       ReadObject(Item, IResult.O[I])
  end;
end;

class procedure TSerializeParse.ReadGeneric(AObject: TObject; IResult: ISuperArray);
var
  I, Len: Integer;
  Info: TGenericsInfo;
  Item: TObject;
begin
  Info := FGenericsCache[AObject.ClassType];
  Len := Info.Count(AObject);
  for I := 0 to Len - 1 do
  begin
    Item := Info.Item(AObject, I);
    if Item <> Nil then
       ReadObject(Item, IResult.O[I])
  end;
end;

class procedure TSerializeParse.ReadMember<T, Typ>(Member: Typ; RType: PTypeInfo; MemberValue: TValue; IJsonData: IBaseJSON<T, Typ>);
var
  I: Integer;
  SubVal: TValue;
  Obj: TObject;
begin
  if MemberValue.IsEmpty and not (RType.Kind in [tkArray, tkDynArray]) then
     IJSONDATA.Null[Member] := jNull
  else
  if RType = TypeInfo(TDateTime) then
     IJSonData.D[Member] := MemberValue.AsType<TDateTime>
  else
  if RType = TypeInfo(TDate) then
     IJSONData.Date[Member] := MemberValue.AsType<TDate>
  else
  if RType = TypeInfo(TTime) then
     IJsonData.Time[Member] := MemberValue.AsType<TTime>
  else
  case RType.Kind of
    tkInteger:
       IJSonData.I[Member] := MemberValue.AsInteger;

    tkInt64:
       IJSonData.I[Member] := MemberValue.AsInt64;

    tkChar, tkString, tkWChar, tkLString, tkWString, tkUString:
       IJSonData.S[Member] := MemberValue.AsString ;

    tkEnumeration:
       if MemberValue.TypeInfo = TypeInfo(Boolean) then
          IJsonData.B[Member] := Boolean( MemberValue.AsOrdinal )
       else
          IJsonData.I[Member] := MemberValue.AsOrdinal;

    tkFloat:
       IJsonData.F[Member] := MemberValue.AsExtended;

    tkSet:
       ReadSet(MemberValue, IJsonData.A[Member]);

    tkClass, tkPointer:
       if MemberValue.IsObject and (MemberValue.AsObject <> Nil) then
       begin
          Obj := MemberValue.AsObject;
          if Obj is TStream then begin
             IJSONData.S[Member] := '';

          end else if IsGenerics(Obj.ClassType) then
             ReadGeneric(Obj, IJSonData.A[Member])
          else if IsCollection(Obj.ClassType) then
             ReadCollection(Obj as TCollection, IJSONData.A[Member])
          else
             ReadObject(Obj, IJSonData.O[Member]);
       end;

    tkVariant:
       if TypeInfo(Typ) = TypeInfo(String) then
          ReadVariantOfObject(MemberValue.AsVariant, PString(@Member)^, ISuperObject(IJsonData))
       else
       if TypeInfo(Typ) = TypeInfo(Integer) then
          ReadVariantOfArray(MemberValue.AsVariant, ISuperArray(IJsonData) );

    tkArray, tkDynArray:
       if not MemberValue.IsArray then
          IJSONDATA.Null[Member] := jNull
       else begin
          IJsonData.A[Member];
          with MemberValue do
              for I := 0 to GetArrayLength - 1 do begin
                  SubVal := GetArrayElement(I);
                  ReadMember<IJSONArray, Integer>( I, SubVal.TypeInfo, SubVal, IJsonData.A[Member]);
              end;
       end;

    tkRecord:
       ReadRecord(MemberValue.TypeInfo, MemberValue.GetReferenceToRawData, IJSonData.O[Member]);

    tkInterface:
      if (TypeInfo(ISuperObject) = MemberValue.TypeInfo) then
         IJsonData.O[Member] := MemberValue.AsType<ISuperObject>.Clone
      else
      if (TypeInfo(ISuperArray) = MemberValue.TypeInfo) then
         IJsonData.A[Member] := MemberValue.AsType<ISuperArray>.Clone;

  end;
end;

class procedure TSerializeParse.ReadSet(Val: TValue;  IJsonData: ISuperArray);
var
  S: TIntegerSet;
  I: Integer;
begin
  Integer(S) := TValueData(Val).FAsULong;
  for I := 0 to SizeOf(Integer) * 8 - 1 do
    if I in S then
       IJsonData.Add(I);
end;

class procedure TSerializeParse.ReadTValueOfArray(Val: TValue;
  IJsonData: ISuperArray);
begin

end;

class procedure TSerializeParse.ReadVariantOfArray(Val: Variant; IJsonData: ISuperArray);
begin
   IJSonData.Add(Val);
end;

class procedure TSerializeParse.ReadVariantOfObject(Val: Variant; const Name: String; IJsonData: ISuperObject);
begin
  IJsonData.SetData(Name, Val);
end;



class procedure TSerializeParse.SetArrayRawData(Member: TRttiObject;
  RawData: Pointer);
begin
  if Member is TRttiProperty  then
     TSuperProperty(Member).ArrayRawData := RawData

  else if Member is TRttiField then
     TSuperField(Member).ArrayRawData:= RawData

  else if Member is TRttiDynamicArrayType then
     TSuperDynArr(Member).ArrayRawData := RawData

  else if Member is TRttiArrayType then
     TSuperArr(Member).ArrayRawData := RawData;
end;

class procedure TSerializeParse.SetValue<Typ>(var Data: Pointer; Member: TRttiObject; MIdx: Typ; Val: TValue);
var
  RVal: REVAL;
begin
  if (TypeInfo(Typ) = TypeInfo(Integer) ) then
      case GetMemberTypeInfo(Member, False).Kind of
         tkArray: begin
             Val.ExtractRawData(Data);
             Exit;
         end;

         tkDynArray: begin
             GetValue<String>(GetArrayRawData(Member), Member, '').SetArrayElement(PInteger(@MIdx)^, Val);
             Exit;
         end;
      end;

  if Member is TRttiProperty  then begin
     RVal := GetREVAL(TRttiProperty(Member).GetAttributes);
     if (RVal <> Nil) and (RVal.CheckEQ(Val)) then
         Val := TValue.FromVariant(RVal.Value);
     TRttiProperty(Member).SetValue(Data, Val)

  end else if Member is TRttiField then begin
       RVal := GetREVAL(TRttiProperty(Member).GetAttributes);
       if (RVal <> Nil) and (RVal.CheckEQ(Val)) then
           Val := TValue.FromVariant(RVal.Value);
       TRttiField(Member).SetValue(Data, Val);

  end else begin
      if Val.IsObject then
         PPointer(Data)^ := Val.AsObject
      else
         PPointer(Data)^ := Val.GetReferenceToRawData
  end;

end;

class procedure TSerializeParse.WriteCollection(ACollection: TCollection; IData: ISuperArray);
var
  ItemType: TRttiType;
  Item: TCollectionItem;
  JMembers: IMember;
begin
  with TRttiContext.Create do
      try
        ItemType := GetType(ACollection.ItemClass)
      finally
        Free;
      end;

  if ItemType = Nil then
     raise ESerializeError.CreateFmt('Unknown collection item type (%s).', [ACollection.ItemClass.ClassName]);

  for JMembers in IData do
      if JMembers.DataType <> dtObject then
         Continue
      else begin
         Item := ACollection.Add;
         WriteMembers(Item, ItemType, JMembers.AsObject);
      end;
end;

class procedure TSerializeParse.WriteGeneric(AObject: TObject; IData: ISuperArray);
var
  Info: TGenericsInfo;
  Item: TObject;
  JMembers: IMember;
begin
  if IData.DatatYpe = dtNil then
    Exit;

  Info := FGenericsCache[AObject.ClassType];
  for JMembers in IData do
      if JMembers.DataType <> dtObject then
         Continue
      else
      begin
         Item := ObjectConstructor(Info.Typ.AsInstance.MetaclassType);
         WriteMembers(Item, Info.Typ, JMembers.AsObject);
         Info.AddVal(AObject, Item);
      end;
end;

class procedure TSerializeParse.WriteMember<T, Typ>(Data: Pointer; Member: Typ;
  RType: PTypeInfo; MemberValue: TRttiObject; IJsonData: IBaseJSON<T, Typ>);
var
  I: Integer;
  J: NativeInt;
  P: Pointer;
  V: Variant;
  SubVal: TValue;
  SubArr: ISuperArray;
  DataType: TDataType;
  Obj: TObject;
  Ancestor: IJSONAncestor;
begin
  if not IJsonData.Contains(Member) then
     Exit;

  if (RType = TypeInfo(TDateTime)) or (RType = TypeInfo(TDate)) or (RType = TypeInfo(TTime)) then
  begin
    Ancestor := IJSONData.Ancestor[Member];
    if not (Ancestor.DataType in [dtNull, dtString]) then
       SetValue<Typ>(Data, MemberValue, Member, TValue.From<TDateTime>(Ancestor.AsVariant))
  end
  else
  case RType.Kind of
    tkInteger:
       SetValue<Typ>(Data, MemberValue, Member, Integer(IJSonData.I[Member]));

    tkInt64:
       SetValue<Typ>(Data, MemberValue, Member, IJSonData.I[Member]);

    tkChar,  tkWChar:
       if IJsonData.S[Member] > '' then
          SetValue<Typ>(Data, MemberValue, Member, TValue.From<Char>(IJSonData.S[Member]{$IFDEF XE2UP}.Chars[CharIndex]{$ELSE}[CharIndex]{$ENDIF}));

    tkString,tkLString, tkWString, tkUString:
       SetValue<Typ>(Data, MemberValue, Member, IJSonData.S[Member]);

    tkEnumeration:
       if GetMemberTypeInfo(MemberValue) = TypeInfo(Boolean) then
       begin
          SetValue<Typ>(Data, MemberValue, Member, IJSONData.B[Member]);
       end
       else
       begin
          TValue.Make(IJSONData.I[Member], GetMemberTypeInfo(MemberValue), SubVal );
          SetValue<Typ>(Data, MemberValue, Member, SubVal);
       end;


    tkFloat:
       SetValue<Typ>(Data, MemberValue, Member, IJsonData.F[Member]);

    tkSet:
       WriteSet(Data, MemberValue, IJsonData.A[Member]);

    tkClass:
       begin
          if CheckObject<Typ>(Data, MemberValue, Member, Obj) then
             if (Obj is TStream) then begin
                
             end else if IsGenerics(Obj.ClassType) then
                WriteGeneric(Obj, IJSONData.A[Member])
             else if IsCollection(Obj.ClassType) then
                WriteCollection(Obj as TCollection, IJSONData.A[Member])
             else
                WriteObject(Obj, IJSonData.O[Member]);
       end;

    tkVariant:
       begin
         V := IJSONData.V[Member];
         if not VarIsNull(V) then
         begin
            TValue.Make(@V, GetMemberTypeInfo(MemberValue), SubVal);
            SetValue<Typ>(Data, MemberValue, Member, SubVal);
         end;
       end;

    tkDynArray, tkArray:
       begin
         if IJSonData.Null[Member] = jAssigned then
         begin
           SetArrayRawData(MemberValue, Data);
           try
             DataType := IJSONData.DataType;
             if DataType = dtArray then begin
                SubArr := IJSONData.AsArray;
                J := IJsonData.AsArray.Length;
                if RType.Kind = tkDynArray then
                   DynArraySetLength(PPointer(Data)^, RType, 1, @J);
                TValue.Make(Data, Rtype, SubVal);

             end else begin
                J := IJSonData.A[Member].Length;
                SubVal := GetValue<Typ>(Data, MemberValue, Member);
                if RType.Kind = tkDynArray then begin
                   DynArraySetLength(PPointer(SubVal.GetReferenceToRawData)^, SubVal.TypeInfo, 1, @J);
                   if (MemberValue is TRttiProperty) and (PropGetterType(TRttiProperty(MemberValue)) = pgtMethod) then begin
                      WriteMember<IJSONArray, Integer>(SubVal.GetReferenceToRawData,
                                          0,
                                          RType,
                                          TRttiProperty(MemberValue).PropertyType,
                                          IJSONData.A[Member]);
                      SetValue<String>(Data, MemberValue,'', SubVal );
                      Exit;

                   end else
                      SetValue<String>(Data, MemberValue,'', SubVal );
                   
                end;
             end;

             for I := 0 to J-1 do begin
                 if DataType <> dtArray then
                    SubArr := IJSONData.A[Member];
                 WriteMember<IJSONArray, Integer>
                            (SubVal.GetReferenceToRawArrayElement(I),
                             I,
                             GetMemberTypeInfo(MemberValue),
                             MemberValue,
                             SubArr);
             end;

             if DataType = dtArray then
                SubVal.ExtractRawData(Data)
             else SetValue<String>(Data, MemberValue,'', SubVal );

           finally
             ClearArrayRawData(MemberValue);
           end;
          end;
       end;

    tkRecord:
       begin
         if (MemberValue.ClassType = TRttiDynamicArrayType) or (MemberValue.ClassType = TRttiArrayType) then
            WriteRecord(GetMemberTypeInfo(MemberValue), Data, IJSonData.O[Member])
         else begin
            P := IValueData(TValueData( GetValue<Typ>(Data, MemberValue, Member) ).FValueData).GetReferenceToRawData;
            WriteRecord(GetMemberTypeInfo(MemberValue), P, IJSonData.O[Member]);
            TValue.Make(P, GetMemberTypeInfo(MemberValue), SubVal);
            SetValue<Typ>(Data, MemberValue, Member, SubVal );
         end;
       end;

    tkInterface:
       if (TypeInfo(ISuperObject) = GetMemberTypeInfo(MemberValue)) And (IJsonData.Ancestor[Member].DataType = dtObject) then
           SetValue<Typ>(Data, MemberValue, Member, TValue.From<ISuperObject>(IJsonData.O[Member].Clone))
       else
       if (TypeInfo(ISuperArray) = GetMemberTypeInfo(MemberValue)) And (IJsonData.Ancestor[Member].DataType = dtArray) then
           SetValue<Typ>(Data, MemberValue, Member, TValue.From<ISuperArray>(IJsonData.A[Member].Clone));
  end;
end;

class procedure TSerializeParse.GetAliasName(const Attributes: TArray<TCustomAttribute>; var Result: String);
var
  Attr: Alias;
begin
  Attr := Alias(GetAttribute(Alias, Attributes));
  if Assigned(Attr) then
     Result := Attr.Name;
end;

class procedure TSerializeParse.WriteMembers(Data: Pointer; aType: TRttiType;
  IJsonData: ISuperObject);
var
  Prop: TRttiProperty;
  Field: TRttiField;
  MemberName: String;
begin
  for Prop in aType.GetProperties do
      if Prop.PropertyType <> Nil then
      begin
         if not (Prop.Visibility in TSerializeParseOptions.Visibilities) then Continue;
         MemberName := Prop.Name;
         if IsDisabled(Prop.GetAttributes) or IsDisabledRead(Prop.GetAttributes) then
            Continue;
         GetAliasName(Prop.GetAttributes, MemberName);
         WriteMember<IJSONObject, String>(Data, MemberName, Prop.PropertyType.Handle, TSuperProperty(Prop), IJSonData);
      end;
  for Field in aType.GetFields do
      if Field.FieldType <> Nil then
      begin
         if not (Field.Visibility in TSerializeParseOptions.Visibilities) then Continue;
         MemberName := Field.Name;
         if IsDisabled(Field.GetAttributes) or IsDisabledRead(Field.GetAttributes) then
            Continue;
         GetAliasName(Field.GetAttributes, MemberName);
         WriteMember<IJSONObject, String>(Data, MemberName, Field.FieldType.Handle, TSuperField(Field), IJSonData);
      end;
end;

class procedure TSerializeParse.WriteObject(AObject: TObject;
  IData: ISuperObject);
var
  Ctx: TRttiContext;
  Typ: TRttiType;
begin
  Ctx := TRttiContext.Create;
  try
    Typ := Ctx.GetType(AObject.ClassType);
    if (not Assigned(Typ)) or (IData.DataType = dtNil) then Exit;
    WriteMembers(AObject, Typ, IData);
  finally
    Ctx.Free;
  end;
end;

class procedure TSerializeParse.WriteRecord(Info: PTypeInfo; ARecord: Pointer;
  IData: ISuperObject);
var
  Ctx: TRttiContext;
  Typ: TRttiType;
begin
  Ctx := TRttiContext.Create;
  try
    Typ := Ctx.GetType(Info);
    if (not Assigned(Typ)) or (IData.DataType = dtNil) then
      Exit;
    WriteMembers(ARecord, Typ, IData);
  finally
    Ctx.Free;
  end;
end;

class procedure TSerializeParse.WriteRecordEx<T>(Rec: T;
  IData: ISuperObject);
begin
   with TValue.From<T>(Rec) do
        WriteRecord(TypeInfo, GetReferenceToRawData, IData);
end;

class procedure TSerializeParse.WriteSet(Data: Pointer; Member: TRttiObject;
  IJSONData: ISuperArray);
var
  Sets: TIntegerSet;
  I: Integer;
  Val: TValue;
begin
  Sets := [];
  for I := 0 to IJSONData.Length -1 do
      Include(Sets, IJSONData.I[I]);
  TValue.Make(Integer(Sets), GetMemberTypeInfo(Member), Val);
  SetValue<String>(Data, Member, '', Val);
end;

{ TSuperRecord<T> }

class function TBaseSuperRecord<T>.AsJSON(Rec: T): String;
begin
  Result := AsJSONObject(Rec).AsJSON;
end;

class function TBaseSuperRecord<T>.FromJSON(JSON: String): T;
begin
   Result := FromJSON(SO(JSON));
end;

class function TBaseSuperRecord<T>.AsJSONObject(Rec: T): ISuperObject;
begin
  Result := XSuperObject.TSerializeParse.ReadRecordEx<T>(Rec);
end;

class function TBaseSuperRecord<T>.FromJSON(JSON: ISuperObject): T;
var
  Val: TValue;
  P: Pointer;
begin
  FillChar(Result, SizeOf(T), 0);
  Val := TValue.From<T>(Result);
  P := IValueData(TValueData(Val).FValueData).GetReferenceToRawData;
  TSerializeParse.WriteRecord(Val.TypeInfo, P, JSON);
  Result := T(P^);
end;

{ TJSONValueHelper }

function TJSONValueHelper.ValueEx<T>: Variant;
var
  Valuable: Boolean;
  pV: PTypeInfo;
const
  Int = 0;
  Str = '';
begin
  Valuable := (Self <> Nil) and not isNull;
  pV := TypeInfo(T);
  if pV = TypeInfo(Int64) then begin
     if Valuable then
        Result := (Self as TJSONInteger).Value
     else
        Result := Int;
  end
  else
  if pV = TypeInfo(Double) then begin
     if Valuable then
        Result := (Self as TJSONFloat).Value
     else
        Result := Int
  end
  else
  if pV = TypeInfo(Boolean) then begin
     if Valuable then
        Result := (Self as TJSONBoolean).Value
     else
        Result := False
  end
  else
  if pV = TypeInfo(String) then
     if Valuable then
        Result := (Self as TJSONString).Value
     else
        Result := Str
end;

{ TSuperExpression }

constructor TSuperExpression.Create(Base: IJSONAncestor; const Expr: String; const BlockException: Boolean);
begin
  FInterpreter := TJSONInterpreter.Create(Expr, Base, BlockException);
  inherited Create(FInterpreter.ReadExpression);
end;

destructor TSuperExpression.Destroy;
begin
  FInterpreter.Free;
  inherited;
end;

{ TCast }

constructor TCast.Create(Base: IJSONAncestor);
begin
  FJSON := Base;
  FName := '';
end;

constructor TCast.Create(Base: IJSONPair);
begin
  FJSON := Base.JSONValue;
  FName := Base.Name;
end;

class function TCast.CreateFrom<T>(Base: T): ICast;
var
  IFace: IInterface;
begin
  IFace := TValue.From<T>(Base).AsInterface;
  if IFace is TJSONAncestor then
     Result := TCast.Create(IFace as TJSONAncestor)
  else
  if IFace is TJSONPair then
     Result := TCast.Create(IFace as TJSONPair)
  else
     Result := TCast.Create(TJSONAncestor(Nil));
end;

destructor TCast.Destroy;
begin
  FJSON := Nil;
  inherited;
end;

function TCast.GetArray: ISuperArray;
begin
  if not Assigned(FJSON) then
     Result := Nil
  else
     Result := TSuperArray.Create(FJSON as TJSONArray);
end;

function TCast.GetBoolean: Boolean;
begin
  if not Assigned(FJSON) then
     Result := False
  else
     Result := TJSONBoolean(FJSON).Value;
end;

function TCast.GetDataType: TDataType;
begin
  if FJSON = Nil then
     Result := dtNil
  else if FJSON is TJSONNull then
     Result := dtNull
  else if FJSON is TJSONString then
     Result := dtString
  else if FJSON is TJSONInteger then
     Result := dtInteger
  else if FJSON is TJSONFloat then
     Result := dtFloat
  else if FJSON is TJSONBoolean then
     Result := dtBoolean
  else if FJSON is TJSONObject then
     Result := dtObject
  else if FJSON is TJSONArray then
     Result := dtArray
  else if FJSON is TJSONDateTime then
     Result := dtDateTime
  else if FJSON is TJSONDate then
     Result := dtDate
  else if FJSON is TJSONTime then
     Result := dtTime
  else
     raise SOException.Create('Unknown JSON Type');
end;

function TCast.GetDate: TDate;
begin
  if not Assigned(FJSON) then
     Result := 0
  else
     Result := TJSONDate(FJSON).Value;
end;

function TCast.GetDateTime: TDateTime;
begin
  if not Assigned(FJSON) then
     Result := 0
  else
     Result := TJSONDateTime(FJSON).Value;
end;

function TCast.GetFloat: Double;
begin
  if not Assigned(FJSON) then
     Result := 0
  else
     if FJSON is TJSONInteger then
        Result := TJSONInteger(FJSON).Value
     else
        Result := TJSONFloat(FJSON).Value;
end;

function TCast.GetInteger: Int64;
begin
  if not Assigned(FJSON) then
     Result := 0
  else
  if DataType <> dtInteger then
     Result := StrToIntDef(VarToStr(GetVariant), 0)
  else
     Result := TJSONInteger(FJSON).Value;
end;

function TCast.GetName: String;
begin
  Result := FName;
end;

function TCast.GetObject: ISuperObject;
begin
  if not Assigned(FJSON) then
     Result := Nil
  else
     Result := TSuperObject.Create(FJSON as TJSONObject);
end;

function TCast.GetString: String;
begin
  if not Assigned(FJSON) then
     Result := ''
  else
     if FJSON is TJSONString then
        Result := TJSONString(FJSON).Value
     else
        Result := VarToStr(FJSON.AsVariant);
end;

function TCast.GetTime: TTime;
begin
  if not Assigned(FJSON) then
     Result := 0
  else
     Result := TJSONTime(FJSON).Value;
end;

function TCast.GetVariant: Variant;
begin
   case DataType of
     dtNil, dtNull, dtObject, dtArray:
        Result := Null;
     dtString:
        Result := AsString;
     dtInteger:
        Result := AsInteger;
     dtFloat:
        Result := AsFloat;
     dtBoolean:
        Result := AsBoolean;
     dtDateTime:
        Result := AsDateTime;
     dtDate:
        Result := AsDate;
     dtTime:
        Result := AsTime;
   end;
end;

procedure TCast.SetBoolean(const Value: Boolean);
begin
  if not Assigned(FJSON) then Exit;
  TJSONBoolean(FJSON).Value := Value;
end;

procedure TCast.SetDate(const Value: TDate);
begin
  if not Assigned(FJSON) then Exit;
  TJSONDate(FJSON).Value := Value;

end;

procedure TCast.SetDateTime(const Value: TDateTime);
begin
  if not Assigned(FJSON) then Exit;
  TJSONDateTime(FJSON).Value := Value;
end;

procedure TCast.SetFloat(const Value: Double);
begin
  if not Assigned(FJSON) then Exit;
  TJSONFloat(FJSON).Value := Value;
end;

procedure TCast.SetInteger(const Value: Int64);
begin
  if not Assigned(FJSON) then Exit;
  TJSONInteger(FJSON).Value := Value;
end;

procedure TCast.SetString(const Value: String);
begin
  if not Assigned(FJSON) then Exit;
  TJSONString(FJSON).Value := Value;
end;


procedure TCast.SetTime(const Value: TTime);
begin
  if not Assigned(FJSON) then Exit;
  TJSONTime(FJSON).Value := Value;
end;

procedure TCast.SetVariant(const Value: Variant);
begin
  case DataType of
     dtString:
        AsString := VarToStr(Value);
     dtInteger:
        AsInteger := Value;
     dtFloat:
        AsFloat   := Value;
     dtBoolean:
        AsBoolean := Value;
     dtDateTime:
        AsDateTime := Value;
     dtDate:
        AsDate := Value;
     dtTime:
        AsTime := Value;
   end;
end;

function TCast.ToString(const Ident, UniversalTime: Boolean): String;
var
  SBuilder: TJSONWriter;
begin
  SBuilder := TJSONWriter.Create(Ident, UniversalTime);
  try
    FJSON.AsJSONString(SBuilder);
    Result := SBuilder.ToString;
  finally
    SBuilder.Free;
  end;
end;

{ TJSONEnumerator<T> }

function TSuperEnumerator<T>.GetCurrent: ICast;
begin
  Result := TCast.CreateFrom<T>(List.List[Index]);
end;

function TSuperEnumerator<T>.MoveNext: Boolean;
begin
  Result := Index < List.List.Count - 1;
  if Result then
    Inc(Index);
end;

{ TBase }

function TBase.AsArray: ISuperArray;
begin
  Result := Nil;
end;

function TBase.AsObject: ISuperObject;
begin
  Result := Nil;
end;

{ TGenericsInfo }

procedure TGenericsInfo.AddVal(Instance: TObject; Val: TValue);
begin
  FAddMethod.Invoke(Instance, [Val]);
end;

function TGenericsInfo.Count(Instance: TObject): Integer;
begin
  Result := FCountProperty.GetValue(Instance).AsInteger;
end;

constructor TGenericsInfo.Create(GenericClass: TClass;const AIsGeneric: Boolean; AType: TRttiType);
begin
  IsGeneric := AIsGeneric;
  Typ := AType;
  if GenericClass <> Nil then
  begin
     FContext := TRttiContext.Create;
     FType := FContext.GetType(GenericClass);
     FAddMethod := FType.GetMethod('Add');
     FCountProperty := FType.GetProperty('Count');
     FGetItemMethod := FType.GetIndexedProperty('Items');
  end
end;

destructor TGenericsInfo.Destroy;
begin
  if IsGeneric then
     FContext.Free;
  inherited;
end;

function TGenericsInfo.Item(Instance: TObject; const Index: Integer): TObject;
begin
  Result := FGetItemMethod.GetValue(Instance, [Index]).AsObject;
end;

{ ReNameField }

constructor Alias.Create(const AName: String);
begin
  FName := AName;
end;

{ REVAL }

function REVAL.CheckEQ(Val: TValue): Boolean;
begin
  case FOption of
    roNone:
      Result := Val.AsVariant = FEqual;
    roEmptyArrayToNull:
      Result := Val.GetArrayLength = 0;
    else raise Exception.CreateFmt('Unknown option: %d', [Ord(FOption)]);
  end;
end;

constructor REVAL.Create(EQVal, NewVal: Integer);
begin
  FOption := roNone;
  FEqual := EQVal;
  FValue := NewVal;
end;

constructor REVAL.Create(EQVal, NewVal: String);
begin
  FOption := roNone;
  FEqual := EQVal;
  FValue := NewVal;
end;

constructor REVAL.Create(EQVal, NewVal: Double);
begin
  FOption := roNone;
  FEqual := EQVal;
  FValue := NewVal;
end;

constructor REVAL.Create(EQVal: String);
begin
  FOption := roNone;
  FEqual := EQVal;
  FValue := System.Variants.Null;
end;

constructor REVAL.Create(EQVal, NewVal: Boolean);
begin
  FOption := roNone;
  FEqual := EQVal;
  FValue := NewVal;
end;

constructor REVAL.Create(EQVal: Integer);
begin
  FOption := roNone;
  FEqual := EQVal;
  FValue := System.Variants.Null;
end;

constructor REVAL.Create(EQVal: Double);
begin
  FOption := roNone;
  FEqual := EQVal;
  FValue := System.Variants.Null;
end;

constructor REVAL.Create(EQVal: Boolean);
begin
  FOption := roNone;
  FEqual := EQVal;
  FValue := System.Variants.Null;
end;

constructor REVAL.Create(Option: TRevalOption);
const
  EMPTY_DATE: TDateTime = 0;
begin
  FOption := Option;
  case FOption of
    roEmptyArrayToNull:
       FValue := System.Variants.Null;
  end;
end;

{ TSerializeParseOptions }

class constructor TSerializeParseOptions.Create;
begin
  FVisibilities := [mvPublic, mvPublished];
end;

{ TSerialize<T> }

class function TJSON.SuperObject(Value: TValue): ISuperObject;
var
  Ctx: TRttiContext;
  Typ: TRttiType;
  I: Integer;
  SubVal: TValue;
  _Array: ISuperArray;
begin
  Ctx := TRttiContext.Create;
  try
    Typ := Ctx.GetType(Value.TypeInfo);
    if not Assigned(Typ) then Exit(SO);

    if Typ.IsRecord then begin
       Result := TSuperObject.Create;
       TSerializeParse.ReadRecord(Value.TypeInfo, Value.GetReferenceToRawData, Result)

    end else if Typ.IsInstance then begin
       Result := TSuperObject.Create;
       TSerializeParse.ReadObject(Value.AsObject, Result);

    end else if Typ.TypeKind = tkInterface then begin
       if Typ.Handle = TypeInfo(ISuperObject) then
          Result := Value.AsType<ISuperObject>
       else if Typ.Handle = TypeInfo(ISuperArray) then
          Result := TSuperObject.CreateCasted(Value.AsType<ISuperArray>.Self)

    end else if Typ.Handle = TypeInfo(TDateTime) then begin
       Result := TSuperObject.CreateCasted(TJSONDateTime.Create(Value.AsExtended));

    end else if Typ.Handle = TypeInfo(TDate) then begin
       Result := TSuperObject.CreateCasted(TJSONDate.Create(Value.AsExtended));

    end else if Typ.Handle = TypeInfo(TTime) then begin
       Result := TSuperObject.CreateCasted(TJSONTime.Create(Value.AsExtended));

    end else if Typ.Handle = TypeInfo(Boolean) then begin
       Result := TSuperObject.CreateCasted(TJSONBoolean.Create(Value.AsBoolean));

    end else begin
       case Typ.TypeKind of
         tkInteger:
            Result := TSuperObject.CreateCasted(TJSONInteger.Create(Int64(Value.AsInteger)));

         tkFloat:
            Result := TSuperObject.CreateCasted(TJSONFloat.Create(Value.AsExtended));

         tkInt64:
            Result := TSuperObject.CreateCasted(TJSONInteger.Create(Value.AsInt64));

         tkString:
            Result := TSuperObject.CreateCasted(TJSONString.Create(Value.AsString));

         tkDynArray, tkArray: begin
            Result := TSuperObject.CreateCasted(TJSONArray.Create);
            _Array := Result.AsArray;
            for I := 0 to Value.GetArrayLength - 1 do begin
                SubVal := Value.GetArrayElement(I);
                TSerializeParse.ReadMember<IJSONArray, Integer>(
                                I,
                                SubVal.TypeInfo,
                                SubVal,
                                _Array);
            end;
         end;

         tkUString:
            Result := TSuperObject.CreateCasted(TJSONString.Create(Value.AsString));
       end;

    end;
  except
    Ctx.Free;
    raise;
  end;
end;


class function TJSON.Parse<T>(JSON: ISuperObject): T;
var
  Ctx: TRttiContext;
  Typ: TRttiType;
  DType: TDataType;
  _Array: ISuperArray;
  _PResult: Pointer;
  I: Integer;
type PTime = ^TTime;
begin
  Ctx := TRttiContext.Create;
  try
    Typ := Ctx.GetType(TypeInfo(T));
    if not Assigned(Typ) then
       Exit(Default(T));

    if Typ.IsRecord then
       Result := TBaseSuperRecord<T>.FromJSON(JSON)

    else if Typ.IsInstance then begin
       Result := Typ.GetMethod('Create').Invoke(Typ.AsInstance.MetaclassType, []).AsType<T>;
       TSerializeParse.WriteObject(TValue.From<T>(Result).AsObject, JSON);

    end else if Typ.Handle = TypeInfo(TDateTime) then begin
       PDateTime(@Result)^ := JSON.Cast.AsDateTime

    end else if Typ.Handle = TypeInfo(TDate) then begin
       PDate(@Result)^ := JSON.Cast.AsDate

    end else if Typ.Handle = TypeInfo(TTime) then begin
       PTime(@Result)^ := JSON.Cast.AsTime

    end else if Typ.Handle = TypeInfo(Boolean) then begin
       PBoolean(@Result)^ := JSON.Cast.AsBoolean

    end else begin
       case Typ.TypeKind of
         tkInteger:
            PInteger(@Result)^ := Integer(JSON.Cast.AsInteger);

         tkFloat:
            PDouble(@Result)^ := JSON.Cast.AsFloat;

         tkInt64:
            PInt64(@Result)^ := JSON.Cast.AsInteger;

         tkString:
           {$IFDEF NEXTGEN}
            PString(@Result)^ := JSON.Cast.AsString;
           {$ELSE}
            PAnsiString(@Result)^ := AnsiString(JSON.Cast.AsString);
           {$ENDIF}

         tkDynArray, tkArray: begin
            _Array := JSON.AsArray;
            _PResult := @Result;
            TSerializeParse.WriteMember<IJSONArray, Integer>(
                                _PResult,
                                0,
                                Typ.Handle,
                                Typ,
                                _Array);
         end;

         tkUString:
            PString(@Result)^ := JSON.Cast.AsString;
       end;

    end;
  except
    Ctx.Free;
    raise;
  end;
end;


class function TJSON.Parse<T>(JSON: ISuperArray): T;
var
  Ctx: TRttiContext;
  _PResult: Pointer;
  Typ: TRttiType;
begin
  Ctx := TRttiContext.Create;
  try
    Typ := Ctx.GetType(TypeInfo(T));
    if not Assigned(Typ) then Exit(Default(T));
    _PResult := @Result;
    TSerializeParse.WriteMember<IJSONArray, Integer>(
                        _PResult,
                        0,
                        Typ.Handle,
                        Typ,
                        JSON);
   finally
     Ctx.Free;
   end;
end;

class function TJSON.Stringify(Value: TValue; Indent, UniversalTime: Boolean): String;
begin
  Result := SuperObject(Value).AsJSON(Indent, UniversalTime);
end;

class function TJSON.Stringify<T>(Value: T; Indent: Boolean; UniversalTime: Boolean): String;
begin
  Result := SuperObject<T>(Value).AsJSON(Indent, UniversalTime);
end;

class function TJSON.Parse<T>(const Value: String): T;
begin
  Result := Parse<T>(SO(Value));
end;

class function TJSON.SuperObject<T>(Value: T): ISuperObject;
begin
  Result := SuperObject(TValue.From<T>(Value));
end;

initialization

  GenericsUnit := TEnumerable<Boolean>.UnitName;

end.
