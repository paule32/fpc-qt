// ---------------------------------------------------------------------------
// \file       misc.pas
// \author     (c) 2024 Jens Kallup - paule32
// \copyright  Alle Rechte vorbehalten.
// ---------------------------------------------------------------------------
{$ifdef FPC}
    {$mode delphi}{$H+}
{$endif}
unit misc;

interface
uses
    {$ifdef win64}
    Windows,
    {$endif}
    {$ifdef Unix}
    DynLibs,
    {$endif}
    SysUtils;

    {$ifdef win64}
    const DLLname = 'fpc-qt.dll';
    {$endif}
    {$ifdef Unix}
    const DLLname = 'fpcso.so';
    {$endif}
type
    TMainCallback = procedure(argc: Integer; argv: Array of String);

    function InitLibrary(Callback: TMainCallback): Boolean;
    function ErrorMessage(s: AnsiString): Boolean; cdecl; external dllname;

implementation
uses fpcmain;

function InitLibrary(Callback: TMainCallback): Boolean;
begin
  result := False;
//  DLLHandle := LoadLibrary('fpc-qt.so');
//  if DLLHandle = 0 then
//  begin
//    ErrorMessage('Error: DLL could not be loaded.');
//    Halt(1);
//  end;
  try
    Callback(ParamCount, ParamStr(1));
  finally
//      FreeLibrary(DLLHandle);
//      ReadLn;
      Halt(0);
  end;
end;

end.

