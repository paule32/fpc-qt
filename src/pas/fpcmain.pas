// ---------------------------------------------------------------------------
// \file       fpc-main.pas
// \author     (c) 2024 Jens Kallup - paule32
// \copyright  Alle Rechte vorbehalten.
// ---------------------------------------------------------------------------
{$ifdef FPC}
    {$mode delphi}{$H+}
{$endif}
unit fpcmain;

interface
{$ifdef win64}
var
    DLLHandle: HMODULE;
{$else}
//uses dynlibs, sysutils;
const DLLname = 'fpc-qt.so';
//var
//    DLLHandle: TLibHandle;
{$endif}

implementation

end.
