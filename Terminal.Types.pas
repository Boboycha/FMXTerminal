unit Terminal.Types;

interface

uses
  System.SysUtils, System.Classes, System.UITypes, System.Types, Terminal.Theme;

type
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
  // Храним STRING, чтобы вмещать суррогатные пары и ZWJ
  TTerminalChar = record
    Char: string;
    Attributes: TCharAttributes;
    Width: Byte;  // 0 = продолжение wide-символа, 1 = обычный, 2 = wide
  end;

  TTerminalLine = array of TTerminalChar;

  TTerminalCursor = record
    X: Integer;
    Y: Integer;
    Visible: Boolean;
  end;

  TMouseTrackingMode = (
    mtm1000_Click,
    mtm1002_Wheel,
    mtm1003_Any,
    mtm1006_SGR
  );
  TMouseTrackingModes = set of TMouseTrackingMode;

  // Функции псевдографики
  function IsBoxDrawingChar(const C: string): Boolean;
  function IsVerticalLine(const C: string): Boolean;
  function IsHorizontalLine(const C: string): Boolean;
  
  // Определение ширины символа (1 или 2 колонки)
  function GetCharDisplayWidth(const Ch: string): Integer;
  
  // Проверка на Zero-Width символы (ZWJ, variation selectors и т.д.)
  function IsZeroWidthChar(const Ch: string): Boolean;

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

// --- ОПРЕДЕЛЕНИЕ ШИРИНЫ СИМВОЛА ---

function GetCodePoint(const Ch: string): Cardinal;
begin
  Result := 0;
  if Length(Ch) = 0 then Exit;
  
  // Суррогатная пара (эмодзи, редкие символы > U+FFFF)
  if (Length(Ch) >= 2) and 
     (Ord(Ch[1]) >= $D800) and (Ord(Ch[1]) <= $DBFF) and
     (Ord(Ch[2]) >= $DC00) and (Ord(Ch[2]) <= $DFFF) then
    Result := $10000 + ((Ord(Ch[1]) - $D800) shl 10) + (Ord(Ch[2]) - $DC00)
  else
    Result := Ord(Ch[1]);
end;

function IsZeroWidthChar(const Ch: string): Boolean;
var
  Code: Cardinal;
begin
  if Length(Ch) = 0 then Exit(True);
  
  Code := GetCodePoint(Ch);
  
  // Zero-width символы
  Result := 
    (Code = $200B) or  // Zero Width Space
    (Code = $200C) or  // Zero Width Non-Joiner
    (Code = $200D) or  // Zero Width Joiner (ZWJ)
    (Code = $2060) or  // Word Joiner
    (Code = $FEFF) or  // BOM / Zero Width No-Break Space
    ((Code >= $FE00) and (Code <= $FE0F)) or  // Variation Selectors
    ((Code >= $E0100) and (Code <= $E01EF));  // Variation Selectors Supplement
end;

function GetCharDisplayWidth(const Ch: string): Integer;
var
  Code: Cardinal;
begin
  if Length(Ch) = 0 then Exit(0);
  
  // Для ZWJ-последовательностей (несколько символов склеенных) 
  // возвращаем 2 — эмодзи обычно wide
  if Length(Ch) > 2 then Exit(2);
  
  Code := GetCodePoint(Ch);
  
  // Zero-width
  if IsZeroWidthChar(Ch) then Exit(0);
  
  // Control characters
  if Code < 32 then Exit(0);
  
  // Wide character ranges
  if // Hangul Jamo
     ((Code >= $1100) and (Code <= $115F)) or
     ((Code >= $11A3) and (Code <= $11A7)) or
     ((Code >= $11FA) and (Code <= $11FF)) or
     // Miscellaneous symbols, Dingbats, Emoticons
     ((Code >= $2300) and (Code <= $23FF)) or
     ((Code >= $2600) and (Code <= $27BF)) or
     // CJK Radicals
     ((Code >= $2E80) and (Code <= $2EFF)) or
     // Kangxi Radicals
     ((Code >= $2F00) and (Code <= $2FDF)) or
     // CJK Symbols and Punctuation
     ((Code >= $3000) and (Code <= $303F)) or
     // Hiragana, Katakana
     ((Code >= $3040) and (Code <= $30FF)) or
     // Bopomofo, Hangul Compat Jamo, Kanbun
     ((Code >= $3100) and (Code <= $319F)) or
     // Enclosed CJK Letters
     ((Code >= $3200) and (Code <= $32FF)) or
     // CJK Compatibility
     ((Code >= $3300) and (Code <= $33FF)) or
     // CJK Unified Ideographs Extension A
     ((Code >= $3400) and (Code <= $4DBF)) or
     // CJK Unified Ideographs
     ((Code >= $4E00) and (Code <= $9FFF)) or
     // Yi Syllables and Radicals
     ((Code >= $A000) and (Code <= $A4CF)) or
     // Hangul Syllables
     ((Code >= $AC00) and (Code <= $D7AF)) or
     // CJK Compatibility Ideographs
     ((Code >= $F900) and (Code <= $FAFF)) or
     // Vertical Forms
     ((Code >= $FE10) and (Code <= $FE1F)) or
     // CJK Compatibility Forms
     ((Code >= $FE30) and (Code <= $FE4F)) or
     // Fullwidth Forms
     ((Code >= $FF00) and (Code <= $FF60)) or
     ((Code >= $FFE0) and (Code <= $FFE6)) or
     // CJK Ext B, C, D, E, F
     ((Code >= $20000) and (Code <= $2FFFD)) or
     ((Code >= $30000) and (Code <= $3FFFD)) or
     // Emoji (most are wide)
     ((Code >= $1F300) and (Code <= $1F9FF)) or
     ((Code >= $1FA00) and (Code <= $1FAFF)) then
    Result := 2
  else
    Result := 1;
end;

// --- ПСЕВДОГРАФИКА ---

function IsBoxDrawingChar(const C: string): Boolean;
var
  Code: Cardinal;
begin
  if Length(C) = 0 then Exit(False);
  Code := GetCodePoint(C);
  Result := (Code >= $2500) and (Code <= $257F);
end;

function IsVerticalLine(const C: string): Boolean;
var
  Code: Cardinal;
begin
  if Length(C) = 0 then Exit(False);
  Code := GetCodePoint(C);
  case Code of
    $2502, $2503, $2551, $2506, $2507, $250A, $250B: Result := True;
  else
    Result := False;
  end;
end;

function IsHorizontalLine(const C: string): Boolean;
var
  Code: Cardinal;
begin
  if Length(C) = 0 then Exit(False);
  Code := GetCodePoint(C);
  case Code of
    $2500, $2501, $2550, $2504, $2505, $2508, $2509, $254C, $254D: Result := True;
  else
    Result := False;
  end;
end;

end.
