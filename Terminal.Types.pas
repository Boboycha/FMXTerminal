unit Terminal.Types;

interface

uses
  System.SysUtils, System.Classes, System.UITypes, System.Types, Terminal.Theme;

type
  {
   TTerminalTheme
   Этот класс теперь хранит все цвета.
  }

  // Атрибуты символа
  TCharAttributes = record
    Bold: Boolean;
    Faint: Boolean;
    Italic: Boolean;
    Underline: Boolean;
    Blink: Boolean;
    Inverse: Boolean;
    Hidden: Boolean;
    Strikethrough: Boolean;
    ForegroundColor: TAlphaColor;
    BackgroundColor: TAlphaColor;
    procedure Reset(ATheme: TTerminalTheme);
    class function Default(ATheme: TTerminalTheme): TCharAttributes; static;
  end;

  // Символ в терминале
  TTerminalChar = record
    Char: WideChar;
    Attributes: TCharAttributes;
  end;

  // Строка терминала
  TTerminalLine = array of TTerminalChar;

  // Позиция курсора
  TTerminalCursor = record
    X: Integer;
    Y: Integer;
    Visible: Boolean;
  end;

  TMouseTrackingMode = (
    mtm1000_Click,       // ?1000 (Click)
    mtm1002_Wheel,       // ?1002 (Click + Wheel)
    mtm1003_Any,         // ?1003 (Click + Wheel + Move)
    mtm1006_SGR         // ?1006 (SGR Extended Mode)
  );
  TMouseTrackingModes = set of TMouseTrackingMode;

  // --- ФУНКЦИИ ДЛЯ ПСЕВДОГРАФИКИ ---
  function IsBoxDrawingChar(C: WideChar): Boolean;
  function IsVerticalLine(C: WideChar): Boolean;
  function IsHorizontalLine(C: WideChar): Boolean;
  // ---------------------------------

implementation

uses
  System.Math;

{ TCharAttributes }

procedure TCharAttributes.Reset(ATheme: TTerminalTheme);
begin
  Bold := False;
  Faint := False;
  Italic := False;
  Underline := False;
  Blink := False;
  Inverse := False;
  Hidden := False;
  Strikethrough := False;
  ForegroundColor := ATheme.DefaultFG;
  BackgroundColor := ATheme.DefaultBG;
end;

class function TCharAttributes.Default(ATheme: TTerminalTheme): TCharAttributes;
begin
  Result.Reset(ATheme);
end;

// --- РЕАЛИЗАЦИЯ ФУНКЦИЙ ПСЕВДОГРАФИКИ ---

function IsBoxDrawingChar(C: WideChar): Boolean;
begin
  // Диапазон Box Drawing в Unicode: U+2500 .. U+257F
  Result := (Ord(C) >= $2500) and (Ord(C) <= $257F);
end;

function IsVerticalLine(C: WideChar): Boolean;
begin
  case Ord(C) of
    $2502, // │ Light vertical
    $2503, // ┃ Heavy vertical
    $2551, // ║ Double vertical
    $2506, // ┆ Light vertical dashed
    $2507, // ┇ Heavy vertical dashed
    $250A, // ┊ Light vertical dotted
    $250B  // ┋ Heavy vertical dotted
    : Result := True;
  else
    Result := False;
  end;
end;

function IsHorizontalLine(C: WideChar): Boolean;
begin
  case Ord(C) of
    $2500, // ─ Light horizontal
    $2501, // ━ Heavy horizontal
    $2550, // ═ Double horizontal
    $2504, // ┄ Light horizontal dashed
    $2505, // ┅ Heavy horizontal dashed
    $2508, // ┈ Light horizontal dotted
    $2509, // ┉ Heavy horizontal dotted
    $254C, // ╌ Light horizontal dashed
    $254D  // ╍ Heavy horizontal dashed
    : Result := True;
  else
    Result := False;
  end;
end;

end.
