unit Terminal.Theme;

interface

uses
  System.Classes, System.UITypes, System.Types;

type
  TTerminalTheme = class(TPersistent)
  private
    FAnsiColors: array[0..15] of TAlphaColor;
    FDefaultFG: TAlphaColor;
    FDefaultBG: TAlphaColor;
    FName: string; // Добавили имя темы
    function GetAnsiColor(Index: Integer): TAlphaColor;
    procedure SetAnsiColor(Index: Integer; const Value: TAlphaColor);

    // --- Новый приватный загрузчик ---
    function HexToColor(const Hex: string): TAlphaColor;

  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;

    // --- Эти методы теперь загружают файлы ---
    procedure LoadDefault;
    procedure LoadSolarizedDark;
    procedure LoadLinuxConsole;
    procedure Monokai;
    procedure LoadThemeFromFile(const AFileName: string);

    // --- Свойства ---
    property Name: string read FName;
    property AnsiColors[Index: Integer]: TAlphaColor read GetAnsiColor write SetAnsiColor;
    property DefaultFG: TAlphaColor read FDefaultFG write FDefaultFG;
    property DefaultBG: TAlphaColor read FDefaultBG write FDefaultBG;
  end;

implementation

uses
  System.IniFiles, System.SysUtils, System.IOUtils, System.Math;

{ TTerminalTheme }

constructor TTerminalTheme.Create;
begin
  inherited Create;
  LoadDefault; // Загружаем тему по умолчанию при создании
end;

procedure TTerminalTheme.Assign(Source: TPersistent);
var
  I: Integer;
  SrcTheme: TTerminalTheme;
begin
  if Source is TTerminalTheme then
  begin
    SrcTheme := Source as TTerminalTheme;
    for I := 0 to 15 do
      Self.FAnsiColors[I] := SrcTheme.FAnsiColors[I];
    Self.FDefaultFG := SrcTheme.FDefaultFG;
    Self.FDefaultBG := SrcTheme.FDefaultBG;
    Self.FName := SrcTheme.FName;
  end
  else
    inherited Assign(Source);
end;

function TTerminalTheme.GetAnsiColor(Index: Integer): TAlphaColor;
begin
  if InRange(Index, 0, 15) then
    Result := FAnsiColors[Index]
  else
    Result := FDefaultFG;
end;

procedure TTerminalTheme.SetAnsiColor(Index: Integer; const Value: TAlphaColor);
begin
  if InRange(Index, 0, 15) then
    FAnsiColors[Index] := Value;
end;

function TTerminalTheme.HexToColor(const Hex: string): TAlphaColor;
var
  TempHex: string;
begin
  Result := TAlphaColor($FF000000); // По умолчанию черный, если ошибка
  if Hex = '' then Exit;

  TempHex := Hex;
  if TempHex.StartsWith('#') then
    Delete(TempHex, 1, 1);
  if TempHex.StartsWith('$') then
    Delete(TempHex, 1, 1);

  // Если это 6-значный RGB (как в HTML), добавляем Alpha
  if Length(TempHex) = 6 then
    TempHex := 'FF' + TempHex;

  if Length(TempHex) <> 8 then
    Exit; // Неверный формат AARRGGBB

  Try
    Result := StrToUInt64('$' + TempHex);
  Except
    // Ошибка преобразования, Result остается черным
  End;
end;

procedure TTerminalTheme.LoadThemeFromFile(const AFileName: string);
var
  Ini: TMemIniFile;
  I: Integer;
begin

  if not TFile.Exists(AFileName) then
  begin
    // Если файл не найден, просто выходим
    // (останется тема, загруженная по умолчанию в конструкторе)
    if FName = '' then // Загружаем "аварийный" default
      LoadDefault;
    Exit;
  end;

  Ini := TMemIniFile.Create(AFileName);
  try
    FName := AFileName;// Ini.ReadString('Theme', 'Name', ExtractFileName(AFileName.Split(['.'])[0]));

    FDefaultFG := HexToColor(Ini.ReadString('Colors', 'DefaultFG', 'FFE5E5E5'));
    FDefaultBG := HexToColor(Ini.ReadString('Colors', 'DefaultBG', 'FF000000'));

    for I := 0 to 15 do
    begin
      FAnsiColors[I] := HexToColor(Ini.ReadString('Colors', 'Ansi'+IntToStr(I), 'FF000000'));
    end;

  finally
    Ini.Free;
  end;
end;

procedure TTerminalTheme.LoadDefault;
begin
  // --- ИЗМЕНЕНИЕ: Загружаем из файла ---
  // (Оставляем "вшитый" вариант как fallback, если файл не найден)
  FName := 'Default (MC)';
  FDefaultFG := $FFE5E5E5;
  FDefaultBG := $FF000000;
  FAnsiColors[0] := $FF000000; FAnsiColors[1] := $FFCD3131;
  FAnsiColors[2] := $FF0DBC79; FAnsiColors[3] := $FFE5E510;
  FAnsiColors[4] := $FF2472C8; FAnsiColors[5] := $FFBC3FBC;
  FAnsiColors[6] := $FF11A8CD; FAnsiColors[7] := $FFE5E5E5;
  FAnsiColors[8] := $FF666666; FAnsiColors[9] := $FFF14C4C;
  FAnsiColors[10] := $FF23D18B; FAnsiColors[11] := $FFF5F543;
  FAnsiColors[12] := $FF3B8EEA; FAnsiColors[13] := $FFD670D6;
  FAnsiColors[14] := $FF29B8DB; FAnsiColors[15] := $FFFFFFFF;

  // Пытаемся перезаписать из файла
  LoadThemeFromFile('Default-MC.theme');
end;

procedure TTerminalTheme.LoadSolarizedDark;
begin
  // --- ИЗМЕНЕНИЕ: Загружаем из файла ---
  LoadThemeFromFile('Solarized-Dark.theme');
end;

procedure TTerminalTheme.LoadLinuxConsole;
begin
  // --- ИЗМЕНЕНИЕ: Загружаем из файла ---
  LoadThemeFromFile('Linux-Console.theme');
end;
procedure TTerminalTheme.Monokai;
begin
  // --- ИЗМЕНЕНИЕ: Загружаем из файла ---
  LoadThemeFromFile('Monokai.theme');
end;

end.
