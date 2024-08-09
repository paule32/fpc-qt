// ---------------------------------------------------------------------------
// \file       fpcqt.pas
// \author     (c) 2024 Jens Kallup - paule32
// \copyright  Alle Rechte vorbehalten.
// ---------------------------------------------------------------------------

{$APPTYPE CONSOLE}
program fpcqt;
uses
  Windows,
  QCharClass;

begin
  DLLHandle := LoadLibrary('fpc-qt.dll');
  if DLLHandle = 0 then
  begin
    MessageBox(0,'Error: DLL could not be loaded.','Error',0);
    ExitProcess(1);
  end;
  try
    myQChar := QChar.Create;

    if myQChar.isDigit then
    WriteLn('ok') else
    WriteLn('not ok');

    myQChar.Free;
  finally
      FreeLibrary(DLLHandle);
      ReadLn;
      ExitProcess(0);
  end;
end.

