// ---------------------------------------------------------------------------
// \file       fpcqt.pas
// \author     (c) 2024 Jens Kallup - paule32
// \copyright  Alle Rechte vorbehalten.
// ---------------------------------------------------------------------------
{$ifdef FPC}
    {$mode delphi}{$H+}

{$else}
    {$APPTYPE CONSOLE}
{$endif}
program fpcqt;
uses
  {$ifdef win64}
  Windows,
  {$endif }
  appsettings in 'pas/appsettings.pas',
  misc        in 'pas/misc.pas',
  fpcmain     in 'pas/fpcmain.pas',
  QCharClass  in 'pas/QCharClass.pas';

procedure EntryPoint(argc: Integer; argv: Array of String);
var
    myQChar: QChar;
begin
    WriteLn('start...');
    myQChar := QChar.Create([' ', ' ']);
    if myQChar.isBlank then
    begin
        WriteLn('ok');
        ReadLn;
    end else
    begin
        WriteLn('not ok');
        Readln;
    end;
    myQChar.Free;
end;

begin
    InitLibrary( EntryPoint );
end.

