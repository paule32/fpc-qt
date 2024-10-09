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

type
    QWORD = uint64;

    QCharArrayChar  = QChar<TArray<Char >>;
    QCharArrayByte  = QChar<TArray<Byte >>;
    QCharArrayWord  = QChar<TArray<Word >>;
    QCharArrayInt16 = QChar<TArray<Int16>>;
    QCharArrayInt32 = QChar<TArray<Int32>>;
    QCharArrayDWORD = QChar<TArray<DWORD>>;
    QCharArrayQWORD = QChar<TArray<QWORD>>;

procedure EntryPoint(argc: Integer; argv: TArray<String>);
var
    myQChar: QChar<TArray<Char>>;
begin
    WriteLn('start...');
    myQChar := QCharArrayChar.Create([' ', ' ']);
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

