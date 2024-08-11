// ---------------------------------------------------------------------------
// \file       QCharClass.pas
// \author     (c) 2024 Jens Kallup - paule32
// \copyright  Alle Rechte vorbehalten.
// ---------------------------------------------------------------------------
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

function isDigit_QChar(v: uint64): Boolean; cdecl; external dllname;
function isLetter_QChar(w: uint64): Boolean; cdecl; external dllname;
function isLetterOrNumber_QChar(w: uint64): Boolean; cdecl; external dllname;
function isLower_QChar(w: uint64): Boolean; cdecl; external dllname;
function isNull_QChar(w: uint64): Boolean; cdecl; external dllname;

type
  /// <summary>
  ///  QChar ist eine Beispielklasse
  /// </summary>
  QChar = class
  private
    ClassName: PChar;
    ptr_cc: uint64;
    c_type: Variant;
  public
    /// <summary>
    ///  Erstellt eine Instanz von QChar ohne Parameter.
    /// </summary>
    /// <remarks>
    ///   Dies ist der Standardkonstruktor.
    /// </remarks>
    constructor Create; overload;

    /// <summary>
    ///  Erstellt eine Instanz von QChar mit einen Byte als Parameter.
    /// </summary>
    /// <param name="c">
    ///  Ein Byte für das Zeichen.
    /// </param>
    /// <remarks>
    ///   Dies ist der Standardkonstruktors.
    /// </remarks>
    constructor Create(c: Byte); overload;

    /// <summary>
    ///  Erstellt eine Instanz von AnsiChar mit einen Byte als Parameter.
    /// </summary>
    /// <param name="c">
    ///  Ein AnsiChar für das Zeichen.
    /// </param>
    /// <remarks>
    ///   Dies ist der AnsiChar Konstruktor
    /// </remarks>
    constructor Create(c: AnsiChar); overload;
    constructor Create(c: WideChar); overload;
    constructor Create(c: DWORD); overload;
    constructor Create(c: Word); overload;
    constructor Create(c: SmallInt) overload;
    destructor Destroy; override;

    function isDigit: Boolean;
    function isLetter: Boolean;
    function isLetterOrNumber: Boolean;
    function isLower: Boolean;
    function isNull: Boolean;

    function getOrigin: uint64;
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
//  ptr_cc := 0;
    WriteLn('11212');
  inherited Destroy;
end;

function QChar.getOrigin: uint64;
begin
  result := ptr_cc;
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

function QChar.isNull: Boolean;
begin
  result := isNull_QChar(ptr_cc);
end;

end.
