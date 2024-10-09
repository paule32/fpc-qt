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

    {$ifdef Win64}
        const DLLname = 'fpcqt.dll';
    {$else}
        const DLLname = 'fpcqt.so';
    {$endif}
type
    TMainCallback = procedure(argc: Integer; argv: TArray<String>);

    function InitLibrary(Callback: TMainCallback): Boolean;
    function ErrorMessage(s: PChar): Boolean; cdecl; external dllname;

    procedure SetPascalCompiler(v: uint8); cdecl; external dllname;
    function  GetPascalCompiler: uint8; cdecl; external dllname;

var
    str_debug: PChar;
implementation
uses fpcmain;

function InitLibrary(Callback: TMainCallback): Boolean;
var
    astr: TArray<String>;
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
        astr := TArray<String>(ParamStr(1));
        Callback(ParamCount, astr);
    finally
        Halt(0);
    end;
end;

end.

