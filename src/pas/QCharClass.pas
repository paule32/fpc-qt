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
    stQChar_Byte     = 101,
    stQChar_AnsiChar = 102,
    stQChar_WideChar = 103,
    stQChar_DWord    = 104,
    stQChar_Word     = 105
);

// ---------------------------------------------------------------------------
// QChar ctor/dtor ...
// ---------------------------------------------------------------------------
function  ctor_QChar(s: PChar; t: symbolType): uint64; cdecl; external dllname;
procedure dtor_QChar(v: uint64); cdecl; external dllname;
function isDigit_QChar(v: uint64): Boolean; cdecl; external dllname;

type
  /// <summary>
  ///  QChar ist eine Beispielklasse
  /// </summary>
  QChar = class
  private
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
    destructor Destroy; override;
    function isDigit: Boolean;
  end;

var
  myQChar: QChar;
implementation

{ QChar }

/// <summary>
///  Erstellt eine Instanz von QChar ohne Parameter.
/// </summary>
constructor QChar.Create;
begin
  inherited Create;
  ptr_cc := ctor_QChar(PChar('ctor_QChar'), stQChar);
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
  ptr_cc := ctor_QChar(PChar('ctor_QChar_Byte'), stQChar_Byte);
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
  ptr_cc := ctor_QChar(PChar('ctor_QChar_AnsiChar'), stQChar_AnsiChar);
  c_type := c;
end;
constructor QChar.Create(c: WideChar);
begin
  inherited Create;
  ptr_cc := ctor_QChar(PChar('ctor_QChar_WideChar'), stQChar_WideChar);
  c_type := c;
end;
constructor QChar.Create(c: DWORD);
begin
  inherited Create;
  ptr_cc := ctor_QChar(PChar('ctor_QChar_DWord'), stQChar_DWord);
  c_type := c;
end;
constructor QChar.Create(c: Word);
begin
  inherited Create;
  ptr_cc := ctor_QChar(PChar('ctor_QChar_Word'), stQChar_Word);
  c_type := c;
end;
destructor QChar.Destroy;
begin
  if ptr_cc = 0 then
  begin
    MessageBoxW(0,
      PChar('Error: QChar not constructed.'),
      PChar('Error'),
      MB_OK);
    ExitProcess(1);
  end;
  dtor_QChar(ptr_cc);
  ptr_cc := 0;
  inherited Destroy;
end;

function QChar.isDigit: Boolean;
begin
  result := isDigit_QChar(ptr_cc);
  if result = true then
  begin
    WriteLn('digit ok');
  end else
  begin
    WriteLn('digit not ok');
  end;
end;

end.
