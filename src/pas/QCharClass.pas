// ---------------------------------------------------------------------------
// \file       QCharClass.pas
// \author     (c) 2024 Jens Kallup - paule32
// \copyright  Alle Rechte vorbehalten.
// ---------------------------------------------------------------------------
{$ifdef FPC}
    {$mode delphi}{$H+}
{$else}
    {$if CompilerVersion >= 36}
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

const
    /// Qt Klasse - QChar
    stQChar          = 100;
    stQChar_Byte     = 102;
    stQChar_AnsiChar = 103;
    stQChar_WideChar = 104;
    stQChar_DWord    = 105;
    stQChar_Word     = 106;
    stQChar_SmallInt = 107;

// ---------------------------------------------------------------------------
// QChar ctor/dtor ...
// ---------------------------------------------------------------------------
function  ctor_QChar(s: PChar; t: uint32; memvar: Pointer): Pointer; cdecl; external dllname;
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
        constructor Create(c: AnsiChar); overload;

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
        constructor Create(c: SmallInt); overload;
        destructor Destroy; override;

        function Lt(const B: QChar): Boolean;
        function Eq(const B: QChar): Boolean;
        function Gt(const B: QChar): Boolean;

        function isAscii: Boolean;

        /// <summary>
        /// prüft, ob das gepeicherte QChar Zeichen ein einzelnes, mathematisches
        /// Objekt entspricht.
        /// </summary>
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
    WriteLn('create qchar');
    ClassName := PChar('QChar');
    ptr_cc := ctor_QChar(PChar('ctor_QChar'), stQChar, 0);

    if not check_ptr(ClassName, getOrigin) then
    begin
        Free;
        exit;
    end;

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
  s: String;
begin
  inherited Create;
  memvar := c;
  ptr := @memvar;
  WriteLn(Format('create byte: 0x%p', [ptr]));
  ClassName := PChar('QChar');
  ptr_cc := ctor_QChar(PChar('ctor_QChar_Byte'), stQChar_Byte, ptr);

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
constructor QChar.Create(c: AnsiChar);
var
  memvar: AnsiChar;
  ptr: Pointer;
begin
  inherited Create;

  memvar := c;
  ptr := @memvar;
//  { $ i fdef DEBUG}
  GetMem(str_debug, 2048);
  str_debug := StrCopy(str_debug, PChar('->' + #13#10));
  str_debug := StrCat (str_debug, PChar(Format('create ansichar: 0x%p', [ptr])));
//  { $ e ndif}
//  ErrorMessage(PChar(str_debug));

  ClassName := PChar('QChar');
  ptr_cc := ctor_QChar(PChar('ctor_QChar_AnsiChar'), stQChar_AnsiChar, ptr);

  str_debug := StrCat(str_debug, PChar(#13#10 + 'created.' + #13#10));
  str_debug := StrCat(str_debug, PChar(Format('ansichar: 0x%p', [ptr_cc])));
  ErrorMessage(str_debug);

  if not check_ptr(ClassName, getOrigin) then
  begin Free; exit; end;

  c_type := Ord(c);
end;

/// <summary>
///  Erstellt eine Instanz der Klasse QChar mit einen WideChar als Parameter.
/// </summary>
/// <param name="c">
///  Ein WideChar für das Zeichen.
/// </param>
constructor QChar.Create(c: WideChar);
var
  memvar: WideChar;
  ptr: Pointer;
begin
  inherited Create;

  memvar := c;
  ptr := @memvar;
//  { $ i fdef DEBUG}
  GetMem(str_debug, 2048);
  str_debug := StrCopy(str_debug, PChar('->' + #13#10));
  str_debug := StrCat (str_debug, PChar(Format('create widechar: 0x%p', [ptr])));
//  { $ e ndif}
//  ErrorMessage(str_debug);

  ClassName := PChar('QChar');
  ptr_cc := ctor_QChar(PChar('ctor_QChar_WideChar'), stQChar_WideChar, ptr);

  str_debug := StrCat(str_debug, PChar(#13#10 + 'created.' + #13#10));
  str_debug := StrCat(str_debug, PChar(Format('widechar: 0x%p', [ptr_cc])));
  ErrorMessage(str_debug);

  if not check_ptr(ClassName, getOrigin) then
  begin Free; exit; end;

  c_type := Ord(c);
end;

/// <summary>
///  Erstellt eine Instanz der Klasse QChar mit einen DWORD als Parameter.
/// </summary>
/// <param name="c">
///  Ein DWORD für das Zeichen.
/// </param>
constructor QChar.Create(c: DWORD);
begin
  inherited Create;
  WriteLn('create dword');
  ClassName := PChar('QChar');
  ptr_cc := ctor_QChar(PChar('ctor_QChar_DWord'), stQChar_DWord, 0);

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
  WriteLn('create word');
  ClassName := PChar('QChar');
  ptr_cc := ctor_QChar(PChar('ctor_QChar_Word'), stQChar_Word, 0);

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
constructor QChar.Create(c: SmallInt);
begin
    inherited Create;
    WriteLn('create smallint');
    ClassName := PChar('QChar');
    ptr_cc := ctor_QChar(PChar('ctor_QChar_SmallInt'), stQChar_SmallInt, 0);

    if not check_ptr(ClassName, getOrigin) then
    begin Free; exit; end;

    c_type := c;
end;

/// <summary>
///  Bereinigt eine Instanz der Klasse QChar.
/// </summary>
destructor QChar.Destroy;
begin
    str_debug := StrCat(str_debug, PChar(#13#10));
    str_debug := StrCat(str_debug, PChar('delete...'));

    dtor_QChar(ptr_cc);

    ErrorMessage(str_debug);
    Dispose(str_debug);

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
    result := isAscii_QChar(uint64(ptr_cc));
end;

function QChar.isDigit: Boolean;
begin
  WriteLn('isDigit');
  //WriteLn(Format('isDigit: 0x%p', [ptr_cc]));
  result := isDigit_QChar(uint64(ptr_cc));
end;

function QChar.isLetter: Boolean;
begin
  result := isLetter_QChar(uint64(ptr_cc));
end;

function QChar.isLetterOrNumber: Boolean;
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

function QChar.isNumber: Boolean;
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

