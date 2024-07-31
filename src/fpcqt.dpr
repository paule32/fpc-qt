{$APPTYPE CONSOLE}
program fpcqt;

uses Windows;

var
  DLLHandle: HMODULE;

type
  TMyFunction = procedure(s: PChar); cdecl;
var
  MyFunction: TMyFunction;

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
    @MyFunction := GetProcAddress(DLLHandle, 'TestFunction');
    if @MyFunction = nil then
    begin
      MessageBox(0, 'Error: TestFunction not found in DLL.', 'Error', 0);
      ExitProcess(1);
    end;
    MyFunction(PChar('jukel'));
  finally
      FreeLibrary(DLLHandle);
      ExitProcess(0);
  end;
end.

