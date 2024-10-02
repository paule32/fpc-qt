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
    {$if defined(Win64)}
        Windows,
    {$elseif defined(Unix)}
        DynLibs,
    {$endif}
        SysUtils;

const CompilerFPC = 1;
const CompilerDCC = 2;

    {$if defined(Win64)}
        const DLLname = 'fpcqt.dll';
    {$elseif defined(Unix)}
        const DLLname = 'fpcqt.so';
    {$else}
        {$message fatal 'Compiler nicht unterstützt'}
    {$endif}
type
    TMainCallback = procedure(argc: Integer; argv: Array of String);

    function InitLibrary(Callback: TMainCallback): Boolean;
    function ErrorMessage(s: PChar): Boolean; cdecl; external dllname;

    procedure SetPascalCompiler(v: uint8); cdecl; external dllname;
    function  GetPascalCompiler: uint8; cdecl; external dllname;

var
    str_debug: PChar;
implementation
uses fpcmain;

function InitLibrary(Callback: TMainCallback): Boolean;
begin
    result := False;
    try
        {$IFDEF CONDITIONALEXPRESSIONS}
            {$IF CompilerVersion >= 36.0}
                {$IF Defined(FPC)}
                    SetPascalCompiler(CompilerFPC);
                {$ELSE}
                    SetPascalCompiler(CompilerDCC);
                {$ENDIF}
            {$ENDIF}
        {$ENDIF}
        Callback(ParamCount, ParamStr(1));
    finally
        Halt(0);
    end;
end;

end.

