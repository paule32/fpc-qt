// ---------------------------------------------------------------------------
// \file       QCharClass.pas
// \author     (c) 2024 Jens Kallup - paule32
// \copyright  Alle Rechte vorbehalten.
// ---------------------------------------------------------------------------
{$define DEBUG}
{$M+}
unit QCharClass;

interface
uses
    Windows,
    System.SysUtils,
    System.Character;

const DLLname = 'fpc-qt.dll';
var
    DLLHandle: HMODULE;

type
symbolType = (
    /// Qt Klasse - QChar
    stQChar          = 100,
    stQChar_Byte     = 102,
    stQChar_AnsiChar = 103,
    stQChar_WideChar = 104,
    stQChar_DWord    = 105,
    stQChar_Word     = 106,
    stQChar_SmallInt = 107
);

// ---------------------------------------------------------------------------
// QChar ctor/dtor ...
// ---------------------------------------------------------------------------
function  ctor_QChar(s: PChar; t: symbolType): uint64; cdecl; external dllname;
procedure dtor_QChar(v: uint64); cdecl; external dllname;

function isAscii_QChar(v: int64): Boolean; cdecl; external dllname;
function isDigit_QChar(v: uint64): Boolean; cdecl; external dllname;
function isLetter_QChar(v: uint64): Boolean; cdecl; external dllname;
function isLetterOrNumber_QChar(v: uint64): Boolean; cdecl; external dllname;
function isLower_QChar(v: uint64): Boolean; cdecl; external dllname;
function isMark_QChar(v: uint64): Boolean; cdecl; external dllname;
function isNonCharacter(v: uint64): Boolean; cdecl; external dllname;
function isNull_QChar(v: uint64): Boolean; cdecl; external dllname;
function isNumber_QChar(v: uint64): Boolean; cdecl; external dllname;
function isPrint_QChar(v: uint64): Boolean; cdecl; external dllname;
function isPunct_QChar(v: uint64): Boolean; cdecl; external dllname;
function isSpace_QChar(v: uint64): Boolean; cdecl; external dllname;
function isSurrogate(v: uint64): Boolean; cdecl; external dllname;
function isSymbol(v: uint64): Boolean; cdecl; external dllname;
function isTitleCase(v: uint64): Boolean; cdecl; external dllname;
function isUpper_QChar(v: uint64): Boolean; cdecl; external dllname;

function toLatin1(v: uint64): uint64; cdecl; external dllname;
function toLower(v: uint64): uint64; cdecl; external dllname;
function toTitleCase(v: uint64): uint64; cdecl; external dllname;
function toUpper(v: uint64): uint64; cdecl; external dllname;

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
            Circle          =  8,
            Compat          = 16,
            Final           =  6,
            Font            =  2,
            Fraction        = 17,
            Initial         =  4,
            Isolated        =  7,
            Medial          =  9,
            Narrow          = 13,
            NoBreak         =  8,
            Small           = 14,
            Square          = 15,
            Sub             = 10,
            Super           =  9,
            Vertical        = 11,
            Wide            = 12
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
            DirAL  = 13,
            DirAN  =  5,
            DirB   =  7,
            DirBN  = 18,
            DirCS  =  6,
            DirEN  =  2,
            DirES  =  3,
            DirET  =  4,
            DirFSI = 21,
            DirL   =  0,
            DirLRE = 11,
            DirLRI = 19,
            DirLRO = 12,
            DirNSM = 17,
            DirON  = 10,
            DirPDF = 16,
            DirPDI = 22,
            DirR   =  1,
            DirRLE = 14,
            DirRLI = 20,
            DirRLO = 15,
            DirS   =  8,
            DirWS  =  9
        );
    private
        ClassName: PChar;
        ptr_cc: uint64;
        c_type: Variant;
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
        constructor Create(c: SmallInt) overload;
        destructor Destroy; override;

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

        /// <summary>
        /// prüft, ob das gepeicherte QChar Zeichen null entspricht.
        /// </summary>
        function isNull: Boolean;

        function isNumber: Boolean;
        function isUpper: Boolean;

        class operator Equal(const A, B: QChar): Boolean;
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
  result := true;
  if ptr = 0 then
  begin
    MessageBoxW(0,
      PChar(Format('Error: %s not constructed.',[name])),
      PChar('Error'),
      MB_OK);
    result := false;
  end;
end;

{ QChar }

/// <summary>
///  Erstellt eine Instanz von QChar ohne Parameter.
/// </summary>
constructor QChar.Create;
begin
  inherited Create;
  ClassName := PChar('QChar');
  ptr_cc := ctor_QChar(PChar('ctor_QChar'), stQChar);

  if not check_ptr(ClassName, getOrigin) then
  begin Free; exit; end;

  c_type := 'A';
end;

/// <summary>
///  Erstellt eine Instanz von QChar mit einen Byte als Parameter.
/// </summary>
/// <param name="c">
///  Ein Byte für das Zeichen.
/// </param>
constructor QChar.Create(c: Byte);
begin
  inherited Create;
  ClassName := PChar('QChar');
  ptr_cc := ctor_QChar(PChar('ctor_QChar_Byte'), stQChar_Byte);

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
begin
  inherited Create;
  ClassName := PChar('QChar');
  ptr_cc := ctor_QChar(PChar('ctor_QChar_AnsiChar'), stQChar_AnsiChar);

  if not check_ptr(ClassName, getOrigin) then
  begin Free; exit; end;

  c_type := c;
end;

/// <summary>
///  Erstellt eine Instanz der Klasse QChar mit einen WideChar als Parameter.
/// </summary>
/// <param name="c">
///  Ein WideChar für das Zeichen.
/// </param>
constructor QChar.Create(c: WideChar);
begin
  inherited Create;
  ClassName := PChar('QChar');
  ptr_cc := ctor_QChar(PChar('ctor_QChar_WideChar'), stQChar_WideChar);

  if not check_ptr(ClassName, getOrigin) then
  begin Free; exit; end;

  c_type := c;
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
  ClassName := PChar('QChar');
  ptr_cc := ctor_QChar(PChar('ctor_QChar_DWord'), stQChar_DWord);

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
  ClassName := PChar('QChar');
  ptr_cc := ctor_QChar(PChar('ctor_QChar_Word'), stQChar_Word);

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
  ClassName := PChar('QChar');
  ptr_cc := ctor_QChar(PChar('ctor_QChar_SmallInt'), stQChar_SmallInt);

  if not check_ptr(ClassName, getOrigin) then
  begin Free; exit; end;

  c_type := c;
end;

/// <summary>
///  Bereinigt eine Instanz der Klasse QChar.
/// </summary>
destructor QChar.Destroy;
begin
  dtor_QChar(ptr_cc);
  ptr_cc := 0;
  WriteLn('pas: QChar: dtor...');
  inherited Destroy;
end;

class operator QChar.Equal(const A, B: QChar): Boolean;
begin
  result := (A.c_type = B.c_type);
end;

function QChar.getOrigin: uint64;
begin
  result := ptr_cc;
end;

function QChar.isAscii: Boolean;
begin
    result := isAscii_QChar(ptr_cc);
end;

function QChar.isDigit: Boolean;
begin
  result := isDigit_QChar(ptr_cc);
end;

function QChar.isLetter: Boolean;
begin
  result := isLetter_QChar(ptr_cc);
end;

function QChar.isLetterOrNumber: Boolean;
begin
  result := isLetterOrNumber_QChar(ptr_cc);
end;

function QChar.isLower: Boolean;
begin
  result := isLower_QChar(ptr_cc);
end;

/// <summary>
/// testung
/// </summary>
function QChar.isNull: Boolean;
begin
  result := isNull_QChar(ptr_cc);
end;

function QChar.isNumber: Boolean;
begin
    result := isNumber_QChar(ptr_cc);
end;

function QChar.isUpper: Boolean;
begin
    result := isUpper_QChar(ptr_cc);
end;
end.
