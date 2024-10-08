// ---------------------------------------------------------------------------
// \file       QCharClass.pas
// \author     (c) 2024 Jens Kallup - paule32
// \copyright  Alle Rechte vorbehalten.
// ---------------------------------------------------------------------------
{$define DEBUG}
{$ifdef FPC}
    {$mode delphi}{$H+}
{$else}
    {$if CompilerVersion >= 36}
    {$define RADCE}
    {$M+}
    {$endif}
{$endif}
unit QCharClass;

interface
uses
    {$ifdef win64}
    Windows,
    {$endif}
    SysUtils, misc, fpcmain;

type
    ClassVHelper = record
        VPointer: Pointer;
        VType  : uint32;
        //
        Value_u1  : uint8;
        Value_u2  : uint16;
        Value_u3  : uint32;
        Value_u4  : uint64;
        //
        Value_s1  : int8;
        Value_s2  : int16;
        Value_s3  : int32;
        Value_s4  : int64;
        //
        Length: uint64;
        Name  : PChar ;
    end;
type
    ResultVHelper = record
        VType1 : ClassVHelper;
        VType2 : ClassVHelper;
        VType3 : ClassVHelper;
    end;

const
    /// Qt Klasse - QChar
    stQChar          = 100;

// ---------------------------------------------------------------------------
// QChar ctor/dtor ...
// ---------------------------------------------------------------------------
function  ctor_QChar(s: PChar; memvar: Pointer): Pointer; cdecl; external dllname;
procedure dtor_QChar(v: Pointer); cdecl; external dllname;

function isAlpha_QChar(v: uint64): Boolean; cdecl; external dllname;
function isAlphaNumber_QChar(v: uint64): Boolean; cdecl; external dllname;
function isAscii_QChar(v: uint64): Boolean; cdecl; external dllname;
function isBlank_QChar(v: uint64): Boolean; cdecl; external dllname;
function isDigit_QChar(v: uint64): Boolean; cdecl; external dllname;
function isLetter_QChar(v: uint64): Boolean; cdecl; external dllname;
function isLetterOrNumber_QChar(v: uint64): Boolean; cdecl; external dllname;
function isLower_QChar(v: uint64): Boolean; cdecl; external dllname;
function isMark_QChar(v: uint64): Boolean; cdecl; external dllname;
function isNonCharacter_QChar(v: uint64): Boolean; cdecl; external dllname;
function isNull_QChar(v: uint64): Boolean; cdecl; external dllname;
function isNumber_QChar(v: uint64): Boolean; cdecl; external dllname;
function isPrint_QChar(v: uint64): Boolean; cdecl; external dllname;
function isPunct_QChar(v: uint64): Boolean; cdecl; external dllname;
function isSpace_QChar(v: uint64): Boolean; cdecl; external dllname;
function isSurrogate_QChar(v: uint64): Boolean; cdecl; external dllname;
function isSymbol_QChar(v: uint64): Boolean; cdecl; external dllname;
function isTitleCase_QChar(v: uint64): Boolean; cdecl; external dllname;
function isUpper_QChar(v: uint64): Boolean; cdecl; external dllname;

function toAscii_QChar(v: uint64): uint8; cdecl; external dllname;
function toLatin1_QChar(v: uint64): uint8; cdecl; external dllname;
function toLower_QChar(v: uint64): uint16; cdecl; external dllname;
function toTitleCase_QChar(v: uint64): uint16; cdecl; external dllname;
function toUpper_QChar(v: uint64): uint16; cdecl; external dllname;

type
    /// <summary>
    ///  QChar ist eine Beispielklasse
    /// </summary>
    QChar<T> = class
    public
        type
        /// <enum>
        /// Dieser Aufzählungs katalogisiert die Unicode Zeichen Kategorie
        /// Die folgenden Zeichen entsprechen den bestehenden Richtlinien
        /// </enum>
        QChar_Category = (
            Mark_NonSpacing       =  0,
            Mark_SpacingCombining =  1,
            Mark_Enclosing        =  2,
            Number_DecimalDigit   =  3,
            Number_Letter         =  4,
            Number_Other          =  5,
            Separator_Space       =  6,
            Separator_Line        =  7,
            Separator_Paragraph   =  8,
            Other_Control         =  9,
            Other_Format          = 10,
            Other_Surrogate       = 11,
            Other_PrivateUse      = 12,
            Other_NotAssigned     = 13
        );

        /// <enum>
        /// Dieser Aufzählungs-Typ definiert die Unicode decompositon Attribute
        /// Schauen Sie sich den Unicode Standard an, um die Beschreibung der
        /// einzelnen Werte zu erhalten.
        /// </enum>
        QChar_Decomposition = (
            NoDecomposition =  0,
            Canonical       =  1,
            Font            =  2,
            NoBreak         =  3,
            Initial         =  4,
            Medial          =  5,
            Final           =  6,
            Isolated        =  7,
            Circle          =  8,
            Super           =  9,
            Sub             = 10,
            Vertical        = 11,
            Wide            = 12,
            Narrow          = 13,
            Small           = 14,
            Square          = 15,
            Compat          = 16,
            Fraction        = 17
        );
        /// <enum>
        /// This enum type defines the Unicode direction attributes.
        /// See the Unicode Standard for a description of the values.
        /// <p>
        /// In order to conform to C/C++ naming conventions "Dir" is prepended
        /// to the codes used in the Unicode Standard.
        //  </p>
        /// </enum>
        QChar_Direction = (
            DirL   =  0,
            DirR   =  1,
            DirEN  =  2,
            DirES  =  3,
            DirET  =  4,
            DirAN  =  5,
            DirCS  =  6,
            DirB   =  7,
            DirS   =  8,
            DirWS  =  9,
            DirON  = 10,
            DirLRE = 11,
            DirLRO = 12,
            DirAL  = 13,
            DirRLE = 14,
            DirRLO = 15,
            DirPDF = 16,
            DirNSM = 17,
            DirBN  = 18,
            DirLRI = 19,
            DirRLI = 20,
            DirFSI = 21,
            DirPDI = 22
        );
    private
        ClassName: PChar;
        ptr_val: ResultVHelper;
        c_type: uint16;
    private
        FCategory:      QChar_Category;
        FDecomposition: QChar_Decomposition;
        FDirection:     QChar_Direction;
    private
        function check_ptr(name: PChar; ptr: uint64): Boolean;
    protected
        function getOrigin: uint64;
    public
        /// <summary>
        /// Erstellt eine Instanz von QChar ohne Parameter.
        /// </summary>
        /// <remarks>
        /// Dies ist der Standardkonstruktor.
        /// </remarks>
        constructor Create; overload;

        /// <summary>
        /// Erstellt eine Instanz von QChar mit einen Byte als Parameter.
        /// </summary>
        /// <param name="c">
        /// Ein Byte für das Zeichen.
        /// </param>
        /// <remarks>
        /// Dies ist der Standardkonstruktors für QChar.
        /// </remarks>
        constructor Create(c: Byte); overload;

        /// <summary>
        /// Erstellt eine Instanz für ein AnsiChar mit einen Byte als Parameter.
        /// </summary>
        /// <param name="c">
        /// Ein AnsiChar für das Zeichen.
        /// </param>
        /// <remarks>
        /// Dies ist der AnsiChar Konstruktor für QChar.
        /// </remarks>
        constructor Create(c: Char); overload;

        /// <summary>
        /// Erstellt eine Instanz für ein DWORD Zeichen mit DWORD als Parameter.
        /// </summary>
        /// <param name="c">
        /// Ein DWORD für das Zeichen.
        /// </param>
        /// <remarks>
        /// Dies ist der DWORD Konstruktor für QChar.
        /// </remarks>
        constructor Create(c: DWORD); overload;

        /// <summary>
        /// Erstellt eine Instanz für ein WORD Zeichen mit WORD als Parameter.
        /// </summary>
        /// <param name="c">
        /// Ein WORD für das Zeichen.
        /// </param>
        /// <remarks>
        /// Dies ist der WORD Konstruktor für QChar.
        /// </remarks>
        constructor Create(c: Word); overload;

        /// <summary>
        /// Erstellt eine Instanz für ein SmallInt Zeichen mit Parameter.
        /// </summary>
        /// <param name="c">
        /// Ein SmallInt für das Zeichen.
        /// </param>
        /// <remarks>
        /// Dies ist der SmallInt Konstruktor für QChar.
        /// </remarks>
        constructor Create(c: int16); overload;

        constructor Create(c: Pointer); overload;

        /// <summary>
        /// Erstellt eine Instanz für zwei 8-Bit Array of Byte Zeichen, die im
        /// Parameter-Block angegeben sind.
        /// </summary>
        /// <param name="c">
        /// Ein Array of Byte für die Zeichen.
        /// </param>
        /// <remarks>
        /// Dies ist der Array of Byte Konstruktor für QChar.
        /// Jeder Array-Eintrag stellt ein 8-Bit Zeichen zur Verfügung.
        /// Es können mehr als zwei Einträge vorhanden sein, aber nur die
        /// ersten zwei Werte werden hierbei berücksichtigt.
        /// </remarks>
        constructor Create(c: Array of Byte); overload;

        /// <summary>
        /// Erstellt eine Instanz für zwei 8-Bit Array of Char Zeichen, die im
        /// Parameter-Block angegeben sind.
        /// </summary>
        /// <param name="c">
        /// Ein Array of Byte für die Zeichen.
        /// </param>
        /// <remarks>
        /// Dies ist der Array of Byte Konstruktor für QChar.
        /// Jeder Array-Eintrag stellt ein 8-Bit Zeichen zur Verfügung.
        /// Es können mehr als zwei Einträge vorhanden sein, aber nur die
        /// ersten zwei Werte werden hierbei berücksichtigt.
        /// </remarks>
        constructor Create(c: Array of Char); overload;

        /// <summary>
        /// Erstellt eine Instanz für zwei 16-Bit Array of Word Zeichen, die im
        /// Parameter-Block angegeben sind.
        /// </summary>
        /// <param name="c">
        /// Ein Array of Byte für die Zeichen.
        /// </param>
        /// <remarks>
        /// Dies ist der Array of Byte Konstruktor für QChar.
        /// Jeder Array-Eintrag stellt ein 16-Bit Zeichen zur Verfügung.
        /// Es können mehr als zwei Einträge vorhanden sein, aber nur die
        /// ersten zwei Werte werden hierbei berücksichtigt.
        /// </remarks>
        constructor Create(c: Array of Word); overload;

        /// <summary>
        /// Erstellt eine Instanz für zwei 32-Bit Array of DWord Zeichen, die im
        /// Parameter-Block angegeben sind.
        /// </summary>
        /// <param name="c">
        /// Ein Array of Byte für die Zeichen.
        /// </param>
        /// <remarks>
        /// Dies ist der Array of Byte Konstruktor für QChar.
        /// Jeder Array-Eintrag stellt ein 32-Bit Zeichen zur Verfügung.
        /// Es können mehr als zwei Einträge vorhanden sein, aber nur die
        /// ersten zwei Werte werden hierbei berücksichtigt.
        /// </remarks>
        constructor Create(c: Array of DWord); overload;

        /// <summary>
        /// Erstellt eine Instanz für einen WideChar als Parameter.
        /// </summary>
        /// <param name="c">
        /// Ein WideChar für das Zeichen.
        /// </param>
        /// <remarks>
        /// Dies ist der WideChar Konstruktor für QChar.
        /// </remarks>
        {$ifndef RADCE}
        constructor Create(c: Array of WideChar); overload;
        {$endif}

        constructor Create(c: Array of Pointer); overload;

        destructor Destroy; override;

        function Lt(const B: QChar<T>): Boolean;
        function Eq(const B: QChar<T>): Boolean;
        function Gt(const B: QChar<T>): Boolean;

        /// <summary>
        ///    <de>eine alpha Funktion</de>
        ///    <en>this is a alpha function</en>
        /// </summary>
        /// <returns>
        ///    <de>Boolean True, wenn alpha. sonst False.</de>
        ///    <en>Boolean True, if alpha. else False.</en>
        /// </returns>
        function isAlpha: Boolean;
        function isAlphaNumber: Boolean;

        function isAscii: Boolean;
        function isBlank: Boolean;

        /// <summary>
        /// prüft, ob das gepeicherte QChar Zeichen ein einzelnes, mathematisches
        /// Objekt entspricht.
        /// </summary>
        /// <remarks>
        ///    Dies und das
        /// </remarks>
        function isDigit: Boolean;

        /// <summary>
        /// prüft, ob das gepeicherte QChar Zeichen ein einzelnes, Schriftzeichen
        /// entspricht.
        /// </summary>
        function isLetter: Boolean;

        /// <summary>
        /// prüft, ob das gepeicherte QChar Zeichen ein einzelnes, Schriftzeichen
        /// oder ein einzelnes mathematisches Objekt entspricht.
        /// </summary>
        function isLetterOrNumber: Boolean;

        /// <summary>
        /// prüft, ob das gepeicherte QChar Zeichen ein einzelnes, kleines
        /// Schriftzeichen entspricht.
        /// </summary>
        function isLower: Boolean;

        function isMark: Boolean;
        function isNonCharacter: Boolean;

        /// <summary>
        /// prüft, ob das gepeicherte QChar Zeichen null entspricht.
        /// </summary>
        function isNull: Boolean;

        function isNumber: Boolean;
        function isPrint: Boolean;
        function isPunct: Boolean;
        function isSpace: Boolean;
        function isSurrogate: Boolean;
        function isSymbol: Boolean;
        function isTitleCase: Boolean;
        function isUpper: Boolean;

        function toLatin1: uint16;
        function toLower: uint16;
        function toTitleCase: uint16;
        function toUpper: uint16;

    published
        property Category:      QChar_Category      read FCategory;
        property Decomposition: QChar_Decomposition read FDecomposition;
        property Direction:     QChar_Direction     read FDirection;
    end;

implementation

/// <summary>
///   prüft, ob eine Instanz mit dem im Parameter "name" stehende Symbol
///   initialisiert wurde.
/// </summary>
function QChar<T>.check_ptr(name: PChar; ptr: uint64): Boolean;
begin
  result := false;
  if ptr = 0 then
  begin
    ErrorMessage(PChar(Format('Error: %s not constructed.',[name])));
    exit;
  end;
  result := true;
end;

{ QChar }

/// <summary>
///  Erstellt eine Instanz von QChar ohne Parameter.
/// </summary>
constructor QChar<T>.Create;
begin
    inherited Create;

    {$ifdef DEBUG}
    WriteLn('create qchar');
    {$endif}

    ptr_val.VType1.VPointer := ctor_QChar(PChar('ctor_QChar'), nil);

    if not check_ptr(ClassName, getOrigin) then
    begin Free; exit; end;

    c_type := Ord('A');
end;

/// <summary>
///  Erstellt eine Instanz von QChar mit einen Byte als Parameter.
/// </summary>
/// <param name="c">
///  Ein Byte für das Zeichen.
/// </param>
constructor QChar<T>.Create(c: Byte);
var
  memvar: SmallInt;
  ptr: Pointer;
begin
  inherited Create;
  memvar := c;
  ptr := @memvar;

  {$ifdef DEBUG}
  WriteLn(Format('create byte: 0x%p', [ptr]));
  {$endif}

  ptr_val.VType1.VPointer := ctor_QChar(PChar('ctor_QChar_Byte'), ptr);

  if not check_ptr(ClassName, getOrigin) then
  begin Free; exit; end;

  c_type := c;
end;

/// <summary>
///  Erstellt eine Instanz von QChar mit einen AnsiChar als Parameter.
/// </summary>
/// <param name="c">
///  Ein AnsiChar für das Zeichen.
/// </param>
constructor QChar<T>.Create(c: Char);
var
    pch_str: PChar;
begin
    inherited Create;
    pch_str          := 'char';
    // ----------------------------
    ptr_val.VType2.VType    := stQChar;
    ptr_val.VType2.Value_u1 := Byte(c);
    ptr_val.VType2.Length   := strlen(pch_str);
    ptr_val.VType2.Name     := pch_str;
    // ----------------------------
    pch_str := 'ctor_QChar_Char';
    ptr_val.VType3.Length  := strlen(pch_str);
    ptr_val.VType3.Name    := pch_str;

    ptr_val.VType1.VPointer := ctor_QChar(pch_str, @ptr_val);

    if not check_ptr(ClassName, getOrigin) then
    begin Free; exit; end;
end;

/// <summary>
///  Erstellt eine Instanz der Klasse QChar mit einen DWORD als Parameter.
/// </summary>
/// <param name="c">
///  Ein DWORD für das Zeichen.
/// </param>
constructor QChar<T>.Create(c: DWORD);
begin
  inherited Create;

  {$ifdef DEBUG}
  WriteLn('create dword');
  {$endif}

  ptr_val.VType1.VPointer := ctor_QChar(PChar('ctor_QChar_DWord'), @ptr_val);

  if not check_ptr(ClassName, getOrigin) then
  begin Free; exit; end;

  c_type := c;
end;

/// <summary>
///  Erstellt eine Instanz der Klasse QChar mit einen Word als Parameter.
/// </summary>
/// <param name="c">
///  Ein Word für das Zeichen.
/// </param>
constructor QChar<T>.Create(c: Word);
begin
  inherited Create;

  {$ifdef DEBUG}
  WriteLn('create word');
  {$endif}

  ptr_val.VType1.VPointer := ctor_QChar(PChar('ctor_QChar_Word'), @ptr_val);

  if not check_ptr(ClassName, getOrigin) then
  begin Free; exit; end;

  c_type := c;
end;

/// <summary>
///  Erstellt eine Instanz der Klasse QChar mit einen SmallInt als Parameter.
/// </summary>
/// <param name="c">
///  Ein SmallInt für das Zeichen.
/// </param>
constructor QChar<T>.Create(c: int16);
var
    pch_str: PChar;
begin
    inherited Create;

    {$ifdef DEBUG}
    WriteLn('create smallint');
    {$endif}

    pch_str          := 'smallint';
    // ----------------------------
    ptr_val.VType2.VType    := stQChar;
    ptr_val.VType2.Value_s2 := c;
    ptr_val.VType2.Length   := strlen(pch_str);
    ptr_val.VType2.Name     := pch_str;
    // ----------------------------
    pch_str := 'ctor_QChar_SmallInt';
    ptr_val.VType3.Length  := strlen(pch_str);
    ptr_val.VType3.Name    := pch_str;

    ptr_val.VType1.VPointer := ctor_QChar(pch_str, @ptr_val);

    if not check_ptr(ClassName, getOrigin) then
    begin Free; exit; end;

    c_type := c;
end;

constructor QChar<T>.Create(c: Pointer);
var
    pch_str: PChar;
begin

end;

constructor QChar<T>.Create(c: Array of Byte);
var
    pch_str: PChar;
begin
    inherited Create;
    if @c = nil then
    begin
        ErrorMessage('Error: c: Array of Byte not allocated.');
        Destroy;
    end;
    if Length(c) < 2 then
    begin
        ErrorMessage('Error: QChar minimum array size == 2');
        Destroy;
    end;

    {$ifdef DEBUG}
    writeln('array of byte ctor delp');
    {$endif}

    pch_str          := 'arraybyte';
    // ----------------------------
    ptr_val.VType2.VType    := stQChar;
    ptr_val.VType2.Value_u1 := int8(c[0]);
    ptr_val.VType2.Value_u2 := int8(c[1]);
    ptr_val.VType2.Length   := 2;
    ptr_val.VType2.Name     := pch_str;
    // ----------------------------
    pch_str := 'ctor_QChar_ArrayOfByte';
    ptr_val.VType3.Length  := strlen(pch_str);
    ptr_val.VType3.Name    := pch_str;

    ptr_val.VType1.VPointer := ctor_QChar(pch_str, @ptr_val);

    if not check_ptr(ClassName, getOrigin) then
    begin Free; exit; end;
end;

constructor QChar<T>.Create(c: Array of Char);
var
    pch_str: PChar;
begin
    inherited Create;
    if @c = nil then
    begin
        ErrorMessage('Error: c: Array of Char not allocated.');
        Destroy;
    end;
    if Length(c) < 2 then
    begin
        ErrorMessage('Error: QChar minimum array size == 2');
        Destroy;
    end;

    {$ifdef DEBUG}
    writeln('array of char ctor delp');
    {$endif}

    pch_str          := 'arraychar';
    // ----------------------------
    ptr_val.VType2.VType    := stQChar;
    ptr_val.VType2.Value_u1 := int8(c[0]);
    ptr_val.VType2.Value_u2 := int8(c[1]);
    ptr_val.VType2.Length   := 2;
    ptr_val.VType2.Name     := pch_str;
    // ----------------------------
    pch_str := 'ctor_QChar_ArrayOfChar';
    ptr_val.VType3.Length  := strlen(pch_str);
    ptr_val.VType3.Name    := pch_str;

    ptr_val.VType1.VPointer := ctor_QChar(pch_str, @ptr_val);

    if not check_ptr(ClassName, getOrigin) then
    begin Free; exit; end;
end;

constructor QChar<T>.Create(c: Array of Word);
var
    pch_str: PChar;
begin
    inherited Create;
    if @c = nil then
    begin
        ErrorMessage('Error: c: Array of Word not allocated.');
        Destroy;
    end;
    if Length(c) < 2 then
    begin
        ErrorMessage('Error: QChar minimum array size == 2');
        Destroy;
    end;

    {$ifdef DEBUG}
    writeln('array of word ctor delp');
    {$endif}

    pch_str          := 'arrayword';
    // ----------------------------
    ptr_val.VType2.VType    := stQChar;
    ptr_val.VType2.Value_u1 := int8(c[0]);
    ptr_val.VType2.Value_u2 := int8(c[1]);
    ptr_val.VType2.Length   := 2;
    ptr_val.VType2.Name     := pch_str;
    // ----------------------------
    pch_str := 'ctor_QChar_ArrayOfWord';
    ptr_val.VType3.Length  := strlen(pch_str);
    ptr_val.VType3.Name    := pch_str;

    ptr_val.VType1.VPointer := ctor_QChar(pch_str, @ptr_val);

    if not check_ptr(ClassName, getOrigin) then
    begin Free; exit; end;
end;

constructor QChar<T>.Create(c: Array of DWord);
var
    pch_str: PChar;
begin
    inherited Create;
    if @c = nil then
    begin
        ErrorMessage('Error: c: Array of DWORD not allocated.');
        Destroy;
    end;
    if Length(c) < 2 then
    begin
        ErrorMessage('Error: QChar minimum array size == 2');
        Destroy;
    end;

    {$ifdef DEBUG}
    writeln('array of DWord ctor delp');
    {$endif}

    pch_str          := 'arraydword';
    // ----------------------------
    ptr_val.VType2.VType    := stQChar;
    ptr_val.VType2.Value_u1 := int8(c[0]);
    ptr_val.VType2.Value_u2 := int8(c[1]);
    ptr_val.VType2.Length   := 2;
    ptr_val.VType2.Name     := pch_str;
    // ----------------------------
    pch_str := 'ctor_QChar_ArrayOfDWord';
    ptr_val.VType3.Length  := strlen(pch_str);
    ptr_val.VType3.Name    := pch_str;

    ptr_val.VType1.VPointer := ctor_QChar(pch_str, @ptr_val);

    if not check_ptr(ClassName, getOrigin) then
    begin Free; exit; end;
end;

{$ifndef RADCE}
/// <summary>
///  Erstellt eine Instanz der Klasse QChar mit einen Array of WideChar als
//   Parameter.
/// </summary>
/// <param name="c">
///  Ein WideChar für das Zeichen.
/// </param>
constructor QChar<T>.Create(c: Array of WideChar);
var
    pch_str: PChar;
begin
    inherited Create;
    if @c = nil then
    begin
        ErrorMessage('Error: c: Array of WideChar not allocated.');
        Destroy;
    end;
    if Length(c) < 2 then
    begin
        ErrorMessage('Error: QChar minimum array size == 2');
        Destroy;
    end;

    pch_str          := 'arraywidechar';
    // ----------------------------
    ptr_val.VType2.VType    := stQChar;
    ptr_val.VType2.Value_u1 := int16(c[0]);
    ptr_val.VType2.Value_u2 := int16(c[1]);
    ptr_val.VType2.Length   := 2;
    ptr_val.VType2.Name     := pch_str;
    // ----------------------------
    pch_str := 'ctor_QChar_ArrayOfWideChar';
    ptr_val.VType3.Length  := strlen(pch_str);
    ptr_val.VType3.Name    := pch_str;

    ptr_val.VType1.VPointer := ctor_QChar(pch_str, @ptr_val);

    if not check_ptr(ClassName, getOrigin) then
    begin Free; exit; end;
end;
{$endif}

constructor QChar<T>.Create(c: Array of Pointer);
var
    pch_str: PChar;
begin
    inherited Create;
    if @c = nil then
    begin
        ErrorMessage('Error: c: Array of Pointer not allocated.');
        Destroy;
    end;
    if Length(c) < 2 then
    begin
        ErrorMessage('Error: QChar minimum array size == 2');
        Destroy;
    end;

    pch_str          := 'arraypointer';
    // ----------------------------
    ptr_val.VType2.VType    := stQChar;
    ptr_val.VType2.Value_u1 := int64(c[0]);
    ptr_val.VType2.Value_u2 := int64(c[1]);
    ptr_val.VType2.Length   := 2;
    ptr_val.VType2.Name     := pch_str;
    // ----------------------------
    pch_str := 'ctor_QChar_ArrayOfPointer';
    ptr_val.VType3.Length   := strlen(pch_str);
    ptr_val.VType3.Name     := pch_str;

    ptr_val.VType1.VPointer := ctor_QChar(pch_str, @ptr_val);

    if not check_ptr(ClassName, getOrigin) then
    begin Free; exit; end;
end;

/// <summary>
///  Bereinigt eine Instanz der Klasse QChar.
/// </summary>
destructor QChar<T>.Destroy;
begin
    {$ifdef DEBUG}
    WriteLn('qchar delete');
    {$endif}

    dtor_QChar(ptr_val.VType1.VPointer);

    inherited Destroy;
end;

function QChar<T>.Lt(const B: QChar<T>): Boolean;
begin
  result := (c_type < B.c_type);
end;

function QChar<T>.Eq(const B: QChar<T>): Boolean;
begin
  result := (c_type = B.c_type);
end;

function QChar<T>.Gt(const B: QChar<T>): Boolean;
begin
  result := (c_type > B.c_type);
end;

function QChar<T>.getOrigin: uint64;
begin
  result := uint64(ptr_val.VType1.VPointer);
end;

function QChar<T>.isAlpha: Boolean;
begin
    result := isAlpha_QChar(uint64(ptr_val.VType1.VPointer));
end;

function QChar<T>.isAlphaNumber: Boolean;
begin
    result := isAlphaNumber_QChar(uint64(ptr_val.VType1.VPointer));
end;

function QChar<T>.isAscii: Boolean;
begin
    result := isAscii_QChar(uint64(ptr_val.VType1.VPointer));
end;

function QChar<T>.isBlank: Boolean;
begin
    result := isBlank_QChar(uint64(ptr_val.VType1.VPointer));
end;

/// <summary>
/// prüft, ob das gepeicherte QChar Zeichen ein einzelnes, mathematisches
/// Objekt (Zahl) entspricht.
/// </summary>
/// <noparam>
/// Boolean Funktion ohne Parameter
/// </noparam>
/// <remarks>
/// Wurde die QChar Klasse mit einer Zahl initializiert wird die Funktion
/// isDigit TRUE zurück geben, sonst FALSE.
/// </remarks>
function QChar<T>.isDigit: Boolean;
begin
  WriteLn('isDigit');
  result := isDigit_QChar(uint64(ptr_val.VType1.VPointer));
end;

function QChar<T>.isLetter: Boolean;
begin
  result := isLetter_QChar(uint64(ptr_val.VType1.VPointer));
end;

function QChar<T>.isLetterOrNumber: Boolean;  // done. pass ok.
begin
  result := isLetterOrNumber_QChar(uint64(ptr_val.VType1.VPointer));
end;

function QChar<T>.isLower: Boolean;  // done. pass ok.
begin
  result := isLower_QChar(uint64(ptr_val.VType1.VPointer));
end;

function QChar<T>.isMark: Boolean;
begin
    result := isLower_QChar(uint64(ptr_val.VType1.VPointer));
end;

function QChar<T>.isNonCharacter: Boolean;
begin
    result := isNonCharacter_QChar(uint64(ptr_val.VType1.VPointer));
end;

function QChar<T>.isNull: Boolean;  // done. pass ok.
begin
  result := isNull_QChar(uint64(ptr_val.VType1.VPointer));
end;

function QChar<T>.isNumber: Boolean;  // done. pass ok.
begin
    result := isNumber_QChar(uint64(ptr_val.VType1.VPointer));
end;

function QChar<T>.isPrint: Boolean;  // done. pass ok.
begin
    result := isPrint_QChar(uint64(ptr_val.VType1.VPointer));
end;

function QChar<T>.isPunct: Boolean;
begin
    result := isPunct_QChar(uint64(ptr_val.VType1.VPointer));
end;

function QChar<T>.isSpace: Boolean;  // done. pass ok.
begin
    result := isSpace_QChar(uint64(ptr_val.VType1.VPointer));
end;

function QChar<T>.isSurrogate: Boolean;
begin
    result := isSurrogate_QChar(uint64(ptr_val.VType1.VPointer));
end;

function QChar<T>.isSymbol: Boolean;
begin
    result := isSymbol_QChar(uint64(ptr_val.VType1.VPointer));
end;

function QChar<T>.isTitleCase: Boolean;
begin
    result := isTitleCase_QChar(uint64(ptr_val.VType1.VPointer));
end;

function QChar<T>.isUpper: Boolean;  // done. pass ok.
begin
    result := isUpper_QChar(uint64(ptr_val.VType1.VPointer));
end;

function QChar<T>.toLatin1: uint16;
begin
    result := toLatin1_QChar(uint64(ptr_val.VType1.VPointer));
end;

function QChar<T>.toLower: uint16;
begin
    result := toLower_QChar(uint64(ptr_val.VType1.VPointer));
end;

function QChar<T>.toTitleCase: uint16;
begin
    result := toTitleCase_QChar(uint64(ptr_val.VType1.VPointer));
end;

function QChar<T>.toUpper: uint16;
begin
    result := toUpper_QChar(uint64(ptr_val.VType1.VPointer));
end;

end.

