// ---------------------------------------------------------------------------
// \file       appsettings.pas
// \author     (c) 2024 Jens Kallup - paule32
// \copyright  Alle Rechte vorbehalten.
// ---------------------------------------------------------------------------
{$ifdef FPC}
    {$mode delphi}{$H+}
{$endif}
unit appsettings;

interface

uses
    {$ifdef Win64}
    Windows,
    {$endif}
    Classes, SysUtils, IniFiles;

/// ---------------------------------------------------------------------------
/// <summary>
///    Delphi Compiler: Version Nummer.
/// </summary
/// ---------------------------------------------------------------------------
const
    CompilerVersionDelphi1              =  8;    // Delphi: VER80
    CompilerVersionDelphi2              =  9;    // Delphi: VER90
    CompilerVersionDelphi3              = 10;    // Delphi: VER100
    CompilerVersionDelphi4              = 12;    // Delphi: VER120
    CompilerVersionDelphi5              = 13;    // Delphi: VER130
    CompilerVersionDelphi6              = 14;    // Delphi: VER140
    CompilerVersionDelphi7              = 15;    // Delphi: VER150
    CompilerVersionDelphi8              = 16;    // Delphi: VER160
    CompilerVersionDelphi2005           = 17;    // Delphi: VER170
    CompilerVersionDelphi2006           = 18;    // Delphi: VER180
    CompilerVersionDelphi2007           = 18.5;  // Delphi: VER180 / VER185
    CompilerVersionDelphi2007NET        = 19;    // Delphi: VER190
    CompilerVersionDelphi2009           = 20;    // Delphi: VER200
    CompilerVersionDelphi2010           = 21;    // Delphi: VER210
    CompilerVersionDelphiXE             = 22;    // Delphi: VER220
    CompilerVersionDelphiXE2            = 23;    // Delphi: VER230
    CompilerVersionDelphiXE3            = 24;    // Delphi: VER240
    CompilerVersionDelphiXE4            = 25;    // Delphi: VER250
    CompilerVersionDelphiXE5            = 26;    // Delphi: VER260
    CompilerVersionDelphiXE6            = 27;    // Delphi: VER270
    CompilerVersionDelphiXE7            = 28;    // Delphi: VER280
    CompilerVersionDelphiXE8            = 29;    // Delphi: VER290
    CompilerVersionDelphi10Seattle      = 30;    // Delphi: VER300
    CompilerVersionDelphi101Berlin      = 31;    // Delphi: VER310
    CompilerVersionDelphi102Tokyo       = 32;    // Delphi: VER320
    CompilerVersionDelphi103Rio         = 33;    // Delphi: VER330
    CompilerVersionDelphi104Sydney      = 34;    // Delphi: VER340
    CompilerVersionDelphi110Alexandria  = 35;    // Delphi: VER350
    CompilerVersionDelphi120Athens      = 36;    // Delphi: VER360

/// ---------------------------------------------------------------------------
/// <summary>
///    Delphi Laufzeit-Bibliothek: Version Nummer.
/// </summary
/// ---------------------------------------------------------------------------
const
    RtlVersionDelphi102Tokyo      = 32;
    RtlVersionDelphi103Berlin     = 33;
    RtlVersionDelphi104Sydney     = 34;
    RtlVersionDelphi110Alexandria = 35;
    RtlVersionDelphi120Athens     = 36;

type
    { TConfigurations }
    TConfigurations = class(TObject)
    private
        FAppConfigFile: string;
        FAppConfigPath: string;

        FAppWorkingDir: string;
        FAppDirectroy : string;
        FAppExeName   : string;
    public
        constructor Create;
        destructor Destroy; override;

        procedure ReadFromFile(Sender: TObject);
        procedure Save(Sender: TObject);
    published
        property AppDirectory : string read FAppDirectroy;
        property AppExeName   : string read FAppExeName;
        property AppWorkingDir: string read FAppWorkingDir;

        property AppConfigPath: string read FAppConfigPath;
        property AppConfigFile: string read FAppConfigFile;
    end;

var
    vConfigurations: TConfigurations;

implementation

const
    DefaultDirectory = '/usr/share/myapp/';

    SectionGeneral   = 'General';
    SectionUnix      = 'UNIX';

    IdentMyDirectory = 'MyDirectory';

var
    MyDirectory: string;

constructor TConfigurations.Create;
begin
{$ifdef win64}
    FAppConfigPath := ExtractFilePath(Paramstr(0));
    FAppConfigFile := ExtractFilePath(ParamStr(0)) + 'myapp.ini';
{$endif}
{$ifdef Unix}
    FAppConfigPath := GetAppConfigFile(False) + '.conf';
{$endif}

 ReadFromFile(nil);
end;

destructor TConfigurations.Destroy;
begin
    Save(nil);
    inherited Destroy;
end;

procedure TConfigurations.Save(Sender: TObject);
var
    MyFile: TIniFile;
begin
    MyFile := TIniFile.Create(AppConfigFile);
    try
        MyFile.WriteString(SectionUnix, IdentMyDirectory, MyDirectory);
    finally
        MyFile.Free;
    end;
end;

procedure TConfigurations.ReadFromFile(Sender: TObject);
var
    MyFile: TIniFile;
begin
    MyFile := TIniFile.Create(AppConfigFile);
    try
        {$ifdef Win64}
        MyDirectory := MyFile.ReadString(SectionUnix, IdentMyDirectory,
        ExtractFilePath(ParamStr(0)));
        {$else}
        MyDirectory := MyFile.ReadString(SectionUnix, IdentMyDirectory,
        DefaultDirectory);
    {$endif}
    finally
        MyFile.Free;
    end;
end;

initialization
    vConfigurations := TConfigurations.Create;

finalization
    FreeAndNil(vConfigurations);
end.
