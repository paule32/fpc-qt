{$APPTYPE CONSOLE}
program fpcqt;

uses Windows, SysUtils;

const DLLname = 'fpc-qt.dll';
var
  DLLHandle: HMODULE;

function  ctor_QChar(s: PChar): uint64; cdecl; external dllname;
procedure dtor_QChar(v: uint64); cdecl; external dllname;
function isDigit_QChar(v: uint64): Boolean; cdecl; external dllname;

type
  QChar = class
  private
    ptr_cc: uint64;
  public
    constructor Create;
    destructor Destroy; override;
    function isDigit: Boolean;
  end;

var
  myQChar: QChar;

constructor QChar.Create;
begin
  inherited Create;
  ptr_cc := ctor_QChar(PChar('ctor_QChar'));
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
var
  res: Boolean;
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

begin
  DLLHandle := LoadLibrary('fpc-qt.dll');
  if DLLHandle = 0 then
  begin
    MessageBox(0,'Error: DLL could not be loaded.','Error',0);
    ExitProcess(1);
  end;
  try
    myQChar := QChar.Create;
    myQChar.isDigit;

    myQChar.Free;
  finally
      FreeLibrary(DLLHandle);
      ExitProcess(0);
  end;
end.

