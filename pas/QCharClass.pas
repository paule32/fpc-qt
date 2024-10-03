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
        NLength: uint64;
        SLength: uint64;
        //
        NName  : PChar ;
        SName  : PChar ;
    end;

const
    /// Qt Klasse - QChar
    stQChar          = 100;

// ---------------------------------------------------------------------------
// QChar ctor/dtor ...
// ---------------------------------------------------------------------------
function  ctor_QChar(s: PChar; memvar: Pointer): Pointer; cdecl; external dllname;
procedure dtor_QChar(v: Pointer); cdecl; external dllname;

function isAscii_QChar(v: int64): Boolean; cdecl; external dllname;
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
    QChar = class
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
        ptr_cc: Pointer;
        ptr_val: ClassVHelper;
        c_type: uint16;
    private
        FCategory:      QChar_Category;
        FDecomposition: QChar_Decomposition;
        FDirection:     QChar_Direction;
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

        {$ifndef RADCE}
        /// <summary>
        /// Erstellt eine Instanz für einen WideChar als Parameter.
        /// </summary>
        /// <param name="c">
        /// Ein WideChar für das Zeichen.
        /// </param>
        /// <remarks>
        /// Dies ist der WideChar Konstruktor für QChar.
        /// </remarks>
        constructor Create(c: WideChar); overload;
        {$endif}

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
        constructor Create(c: Array of  Byte); overload;

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
        constructor Create(c: Array of  Char); overload;

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
        constructor Create(c: Array of  Word); overload;

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

        destructor Destroy; override;

        function Lt(const B: QChar): Boolean;
        function Eq(const B: QChar): Boolean;
        function Gt(const B: QChar): Boolean;

        function isAscii: Boolean;

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
    protected
        function getOrigin: uint64;

    published
        property Category:      QChar_Category      read FCategory      default Other_NotAssigned;
        property Decomposition: QChar_Decomposition read FDecomposition default NoDeComposition;
        property Direction:     QChar_Direction     read FDirection     default DirEN;
    end;

implementation

/// <summary>
///   prüft, ob eine Instanz mit dem im Parameter "name" stehende Symbol
///   initialisiert wurde.
/// </summary>
function check_ptr(name: PChar; ptr: uint64): Boolean;
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
constructor QChar.Create;
begin
    inherited Create;

    {$ifdef DEBUG}
    WriteLn('create qchar');
    {$endif}

    ptr_cc := ctor_QChar(PChar('ctor_QChar'), nil);

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
constructor QChar.Create(c: Byte);
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

  ptr_cc := ctor_QChar(PChar('ctor_QChar_Byte'), ptr);

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
constructor QChar.Create(c: Char);
var
    pch_str: PChar;
begin
    inherited Create;
    pch_str          := 'char';
    // ----------------------------
    ptr_val.VType    := stQChar;
    ptr_val.Value_u1 := Byte(c);
    ptr_val.NLength  := strlen(pch_str);
    ptr_val.NName    := pch_str;
    // ----------------------------
    pch_str := 'ctor_QChar_Char';
    ptr_val.SLength  := strlen(pch_str);
    ptr_val.SName    := pch_str;

    ptr_cc := ctor_QChar(pch_str, @ptr_val);

    if not check_ptr(ClassName, getOrigin) then
    begin Free; exit; end;
end;

/// <summary>
///  Erstellt eine Instanz der Klasse QChar mit einen WideChar als Parameter.
/// </summary>
/// <param name="c">
///  Ein WideChar für das Zeichen.
/// </param>
{$ifndef RADCE}
constructor QChar.Create(c: WideChar);
var
  memvar: WideChar;
  ptr: Pointer;
begin
  inherited Create;
ErrorMessage('kukuku  wwwwchar');
  memvar := c;
  ptr := @memvar;

  {$ifdef DEBUG}
  GetMem(str_debug, 2048);
  str_debug := StrCopy(str_debug, PChar('->' + #13#10));
  str_debug := StrCat (str_debug, PChar(Format('create widechar: 0x%p', [ptr])));
  ErrorMessage(str_debug);
  Dispose(str_debug);
  {$endif}

  ptr_cc := ctor_QChar(PChar('ctor_QChar_WideChar'), ptr);

  {$ifdef DEBUG}
  GetMem(str_debug, 2048);
  str_debug := StrCat(str_debug, PChar(#13#10 + 'created.' + #13#10));
  str_debug := StrCat(str_debug, PChar(Format('wide char: 0x%p', [ptr_cc])));
  ErrorMessage(str_debug);
  Dispose(str_debug);
  {$endif}

  if not check_ptr(ClassName, getOrigin) then
  begin Free; exit; end;

  c_type := Ord(c);
end;
{$endif}

/// <summary>
///  Erstellt eine Instanz der Klasse QChar mit einen DWORD als Parameter.
/// </summary>
/// <param name="c">
///  Ein DWORD für das Zeichen.
/// </param>
constructor QChar.Create(c: DWORD);
begin
  inherited Create;

  {$ifdef DEBUG}
  WriteLn('create dword');
  {$endif}

  ptr_cc := ctor_QChar(PChar('ctor_QChar_DWord'), nil);

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
constructor QChar.Create(c: Word);
begin
  inherited Create;

  {$ifdef DEBUG}
  WriteLn('create word');
  {$endif}

  ptr_cc := ctor_QChar(PChar('ctor_QChar_Word'), nil);

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
constructor QChar.Create(c: int16);
var
    pch_str: PChar;
begin
    inherited Create;

    {$ifdef DEBUG}
    WriteLn('create smallint');
    {$endif}

    pch_str          := 'smallint';
    // ----------------------------
    ptr_val.VType    := stQChar;
    ptr_val.Value_s2 := c;
    ptr_val.NLength  := strlen(pch_str);
    ptr_val.NName    := pch_str;
    // ----------------------------
    pch_str := 'ctor_QChar_SmallInt';
    ptr_val.SLength  := strlen(pch_str);
    ptr_val.SName    := pch_str;

    ptr_cc := ctor_QChar(pch_str, @ptr_val);

    if not check_ptr(ClassName, getOrigin) then
    begin Free; exit; end;

    c_type := c;
end;

constructor QChar.Create(c: Array of Byte);
var
    pch_str: PChar;
begin
    {$ifdef DEBUG}
    writeln('array of byte ctor delp');
    {$endif}

    pch_str          := 'arraybyte';
    // ----------------------------
    ptr_val.VType    := stQChar;
    ptr_val.Value_u1 := int8(c[0]);
    ptr_val.Value_u2 := int8(c[1]);
    ptr_val.NLength  := 2;
    ptr_val.NName    := pch_str;
    // ----------------------------
    pch_str := 'ctor_QChar_ArrayOfByte';
    ptr_val.SLength  := strlen(pch_str);
    ptr_val.SName    := pch_str;

    ptr_cc := ctor_QChar(pch_str, @ptr_val);

    if not check_ptr(ClassName, getOrigin) then
    begin Free; exit; end;
end;

constructor QChar.Create(c: Array of Char);
var
    pch_str: PChar;
begin
    {$ifdef DEBUG}
    writeln('array of char ctor delp');
    {$endif}

    pch_str          := 'arraychar';
    // ----------------------------
    ptr_val.VType    := stQChar;
    ptr_val.Value_u1 := int8(c[0]);
    ptr_val.Value_u2 := int8(c[1]);
    ptr_val.NLength  := 2;
    ptr_val.NName    := pch_str;
    // ----------------------------
    pch_str := 'ctor_QChar_ArrayOfChar';
    ptr_val.SLength  := strlen(pch_str);
    ptr_val.SName    := pch_str;

    ptr_cc := ctor_QChar(pch_str, @ptr_val);

    if not check_ptr(ClassName, getOrigin) then
    begin Free; exit; end;
end;

constructor QChar.Create(c: Array of Word);
var
    pch_str: PChar;
begin
    {$ifdef DEBUG}
    writeln('array of word ctor delp');
    {$endif}

    pch_str          := 'arrayword';
    // ----------------------------
    ptr_val.VType    := stQChar;
    ptr_val.Value_u1 := int8(c[0]);
    ptr_val.Value_u2 := int8(c[1]);
    ptr_val.NLength  := 2;
    ptr_val.NName    := pch_str;
    // ----------------------------
    pch_str := 'ctor_QChar_ArrayOfWord';
    ptr_val.SLength  := strlen(pch_str);
    ptr_val.SName    := pch_str;

    ptr_cc := ctor_QChar(pch_str, @ptr_val);

    if not check_ptr(ClassName, getOrigin) then
    begin Free; exit; end;
end;

constructor QChar.Create(c: Array of DWord);
var
    pch_str: PChar;
begin
    {$ifdef DEBUG}
    writeln('array of DWord ctor delp');
    {$endif}

    pch_str          := 'arraydword';
    // ----------------------------
    ptr_val.VType    := stQChar;
    ptr_val.Value_u1 := int8(c[0]);
    ptr_val.Value_u2 := int8(c[1]);
    ptr_val.NLength  := 2;
    ptr_val.NName    := pch_str;
    // ----------------------------
    pch_str := 'ctor_QChar_ArrayOfDWord';
    ptr_val.SLength  := strlen(pch_str);
    ptr_val.SName    := pch_str;

    ptr_cc := ctor_QChar(pch_str, @ptr_val);

    if not check_ptr(ClassName, getOrigin) then
    begin Free; exit; end;
end;

/// <summary>
///  Bereinigt eine Instanz der Klasse QChar.
/// </summary>
destructor QChar.Destroy;
begin
    {$ifdef DEBUG}
    GetMem(str_debug, 2048);
    str_debug := StrCat(str_debug, PChar(#13#10));
    str_debug := StrCat(str_debug, PChar('delete...'));
    ErrorMessage(str_debug);
    Dispose(str_debug);
    {$endif}

    dtor_QChar(ptr_cc);

    inherited Destroy;
end;

function QChar.Lt(const B: QChar): Boolean;
begin
  result := (c_type < B.c_type);
end;

function QChar.Eq(const B: QChar): Boolean;
begin
  result := (c_type = B.c_type);
end;

function QChar.Gt(const B: QChar): Boolean;
begin
  result := (c_type > B.c_type);
end;

function QChar.getOrigin: uint64;
begin
  result := uint64(ptr_cc);
end;

function QChar.isAscii: Boolean;
begin
writeln('check if ascii');
    result := isAscii_QChar(uint64(ptr_cc));
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
function QChar.isDigit: Boolean;
begin
  WriteLn('isDigit');
  result := isDigit_QChar(uint64(ptr_cc));
end;

function QChar.isLetter: Boolean;
begin
  result := isLetter_QChar(uint64(ptr_cc));
end;

function QChar.isLetterOrNumber: Boolean;  // done. getestet
begin
  result := isLetterOrNumber_QChar(uint64(ptr_cc));
end;

function QChar.isLower: Boolean;
begin
  result := isLower_QChar(uint64(ptr_cc));
end;

function QChar.isMark: Boolean;
begin
    result := isLower_QChar(uint64(ptr_cc));
end;

function QChar.isNonCharacter: Boolean;
begin
    result := isNonCharacter_QChar(uint64(ptr_cc));
end;

/// <summary>
/// testung
/// </summary>
function QChar.isNull: Boolean;
begin
  result := isNull_QChar(uint64(ptr_cc));
end;

function QChar.isNumber: Boolean;  // done. getestet
begin
    result := isNumber_QChar(uint64(ptr_cc));
end;

function QChar.isPrint: Boolean;
begin
    result := isPrint_QChar(uint64(ptr_cc));
end;

function QChar.isPunct: Boolean;
begin
    result := isPunct_QChar(uint64(ptr_cc));
end;

function QChar.isSpace: Boolean;
begin
    result := isSpace_QChar(uint64(ptr_cc));
end;

function QChar.isSurrogate: Boolean;
begin
    result := isSurrogate_QChar(uint64(ptr_cc));
end;

function QChar.isSymbol: Boolean;
begin
    result := isSymbol_QChar(uint64(ptr_cc));
end;

function QChar.isTitleCase: Boolean;
begin
    result := isTitleCase_QChar(uint64(ptr_cc));
end;

function QChar.isUpper: Boolean;
begin
    result := isUpper_QChar(uint64(ptr_cc));
end;

function QChar.toLatin1: uint16;
begin
    result := toLatin1_QChar(uint64(ptr_cc));
end;

function QChar.toLower: uint16;
begin
    result := toLower_QChar(uint64(ptr_cc));
end;

function QChar.toTitleCase: uint16;
begin
    result := toTitleCase_QChar(uint64(ptr_cc));
end;

function QChar.toUpper: uint16;
begin
    result := toUpper_QChar(uint64(ptr_cc));
end;

end.

