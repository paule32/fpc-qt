{$APPTYPE CONSOLE}
program fpcqt;

uses Windows, SysUtils;

var
  DLLHandle: HMODULE;

type
  TMyFunction = procedure(s: PChar; v: DWORD); cdecl;
  TCTOR_CreateQChar = function(s: PChar): Pointer; cdecl;
var
  MyFunction: TMyFunction;
  ptr: Pointer;
  CTOR_CreateQChar: TCTOR_CreateQChar;

type
  QtExample = class
  public
    constructor Create;
    destructor Destroy;
  end;

constructor QtExample.Create;
begin

end;

destructor QtExample.Destroy;
begin

end;

begin
  DLLHandle := LoadLibrary('fpc-qt.dll');
  if DLLHandle = 0 then
  begin
    MessageBox(0,'Error: DLL could not be loaded.','Error',0);
    ExitProcess(1);
  end;
  try
    @MyFunction := GetProcAddress(DLLHandle, 'addsymbol');
    @CTOR_CreateQChar := GetProcAddress(DLLHandle, 'ctor_CreateQChar');
    if @CTOR_CreateQChar = nil then
    begin
      MessageBox(0, 'Error: CTOR for QChar not found in DLL.', 'Error',0);
      ExitProcess(1);
    end;
    if @MyFunction = nil then
    begin
      MessageBox(0, 'Error: TestFunction not found in DLL.', 'Error', 0);
      ExitProcess(1);
    end;
    MyFunction(PChar('jukel'), 2);
    ptr := CTOR_CreateQChar(PChar('mopsi'));
    WriteLn(Format('PTR := 0x%08x', [ptr]));
  finally
      FreeLibrary(DLLHandle);
      ExitProcess(0);
  end;
end.

