unit Terminal.AnsiParser;

interface

uses
  System.SysUtils, System.Classes, System.UITypes, System.UIConsts,
  System.Generics.Collections, System.Math,
  Terminal.Types,
  Terminal.Theme;

type
  TAnsiParserCommand = (apcNone, apcPrintChar,
    apcCursorUp, apcCursorDown, apcCursorForward,
    apcCursorBack, apcCursorNextLine, apcCursorPrevLine, apcCursorHorizontalAbs,
    apcCursorPosition, apcVerticalPositionAbs, apcVerticalPositionRel,
    apcHorizPositionAbs, apcHorizPositionRel, apcCursorBackwardTab,
    apcEraseDisplay, apcEraseLine, apcEraseChar, apcScrollUp, apcScrollDown,
    apcInsertLine, apcDeleteLine, apcInsertChar, apcDeleteChar, apcRepeatChar,
    apcSetGraphicsMode, apcSetMode, apcResetMode, apcSetPrivateMode,
    apcResetPrivateMode, apcSetScrollingRegion, apcSoftTerminalReset,
    apcSetCursorStyle, apcSaveCursorPosition, apcRestoreCursorPosition,
    apcDeviceAttributes, apcDeviceStatusReport, apcTabClear,
    apcReverseIndex
    );

  TAnsiCommand = record
    Command: TAnsiParserCommand;
    Params: TArray<Integer>;
    Char: WideChar;
    Attributes: TCharAttributes;
  end;

  TCharacterSet = (csASCII, csLineDrawing);

  TAnsiParser = class
  private
    FState: (psNormal, psEscape, psCSI, psOSC, psCharsetG0, psCharsetG1);
    FParamBuffer: string;
    FCurrentAttributes: TCharAttributes;
    FTheme: TTerminalTheme;

    FG0: TCharacterSet;
    FG1: TCharacterSet;
    FUseG1: Boolean;

    procedure ParseSGR(const Params: TArray<Integer>);
    function GetColor256(Index: Integer): TAlphaColor;
    function GetColorRGB(R, G, B: Integer): TAlphaColor;
    function MapChar(Ch: WideChar): WideChar;

  public
    constructor Create(ATheme: TTerminalTheme);
    function GetParamInt(const S: string; Index: Integer; Default: Integer = 1): Integer;
    function Parse(const Input: string;
      out Commands: TArray<TAnsiCommand>): Boolean;
    procedure SetTheme(ATheme: TTerminalTheme);
    property CurrentAttributes: TCharAttributes read FCurrentAttributes
      write FCurrentAttributes;
  end;

implementation

{ TAnsiParser }

constructor TAnsiParser.Create(ATheme: TTerminalTheme);
begin
  inherited Create;
  FState := psNormal;
  FParamBuffer := '';
  FTheme := ATheme;
  FCurrentAttributes := TCharAttributes.Default(FTheme);

  FG0 := csASCII;
  FG1 := csLineDrawing;
  FUseG1 := False;
end;

procedure TAnsiParser.SetTheme(ATheme: TTerminalTheme);
begin
  FTheme := ATheme;
  FCurrentAttributes.Reset(FTheme);
end;

function TAnsiParser.MapChar(Ch: WideChar): WideChar;
var
  CurrentSet: TCharacterSet;
begin
  if FUseG1 then CurrentSet := FG1 else CurrentSet := FG0;

  if CurrentSet = csASCII then Exit(Ch);

  case Ord(Ch) of
    $5F: Result := #$00A0;
    $60: Result := #$25C6;
    $61: Result := #$2592;
    $62: Result := #$2409;
    $63: Result := #$240C;
    $64: Result := #$240D;
    $65: Result := #$240A;
    $66: Result := #$00B0;
    $67: Result := #$00B1;
    $68: Result := #$2424;
    $69: Result := #$240B;
    $6A: Result := #$2518; // ┘
    $6B: Result := #$2510; // ┐
    $6C: Result := #$250C; // ┌
    $6D: Result := #$2514; // └
    $6E: Result := #$253C; // ┼
    $6F: Result := #$23BA;
    $70: Result := #$23BB;
    $71: Result := #$2500; // ─
    $72: Result := #$23BC;
    $73: Result := #$23BD;
    $74: Result := #$251C; // ├
    $75: Result := #$2524; // ┤
    $76: Result := #$2534; // ┴
    $77: Result := #$252C; // ┬
    $78: Result := #$2502; // │
    $79: Result := #$2264;
    $7A: Result := #$2265;
    $7B: Result := #$03C0;
    $7C: Result := #$2260;
    $7D: Result := #$00A3;
    $7E: Result := #$00B7;
  else
    Result := Ch;
  end;
end;

function TAnsiParser.GetParamInt(const S: string; Index: Integer; Default: Integer): Integer;
var
  Parts: TArray<string>;
begin
  Parts := S.Split([';']);
  if (Index >= 0) and (Index < Length(Parts)) and (Parts[Index] <> '') then
    Result := StrToIntDef(Parts[Index], Default)
  else
    Result := Default;
end;

function TAnsiParser.GetColor256(Index: Integer): TAlphaColor;
var
  R, G, B: Byte;
begin
  if (Index >= 0) and (Index <= 15) then
    Result := FTheme.AnsiColors[Index]
  else if (Index >= 16) and (Index <= 231) then
  begin
    Index := Index - 16;
    R := ((Index div 36) mod 6) * 51;
    G := ((Index div 6) mod 6) * 51;
    B := (Index mod 6) * 51;
    Result := MakeColor(R, G, B);
  end
  else if (Index >= 232) and (Index <= 255) then
  begin
    R := 8 + (Index - 232) * 10;
    Result := MakeColor(R, R, R);
  end
  else
    Result := FTheme.DefaultFG;
end;

function TAnsiParser.GetColorRGB(R, G, B: Integer): TAlphaColor;
begin
  Result := MakeColor(EnsureRange(R, 0, 255), EnsureRange(G, 0, 255),
    EnsureRange(B, 0, 255));
end;

procedure TAnsiParser.ParseSGR(const Params: TArray<Integer>);
var
  I: Integer;
  Param: Integer;
begin
  if Length(Params) = 0 then
  begin
    FCurrentAttributes.Reset(FTheme);
    Exit;
  end;

  I := 0;
  while I < Length(Params) do
  begin
    Param := Params[I];

    case Param of
      0: FCurrentAttributes.Reset(FTheme);
      1: FCurrentAttributes.Bold := True;
      2: FCurrentAttributes.Faint := True;
      3: FCurrentAttributes.Italic := True;
      4: FCurrentAttributes.Underline := True;
      5: FCurrentAttributes.Blink := True;
      7: FCurrentAttributes.Inverse := True;
      8: FCurrentAttributes.Hidden := True;
      9: FCurrentAttributes.Strikethrough := True;

      10: FG0 := csASCII;       // Сброс шрифта
      11: FG0 := csLineDrawing; // Альтернативный шрифт (ncdu)
      12: FG0 := csLineDrawing;

      22: begin FCurrentAttributes.Bold := False; FCurrentAttributes.Faint := False; end;
      23: FCurrentAttributes.Italic := False;
      24: FCurrentAttributes.Underline := False;
      25: FCurrentAttributes.Blink := False;
      27: FCurrentAttributes.Inverse := False;
      28: FCurrentAttributes.Hidden := False;
      29: FCurrentAttributes.Strikethrough := False;

      30 .. 37: FCurrentAttributes.ForegroundColor := FTheme.AnsiColors[Param - 30];
      39: FCurrentAttributes.ForegroundColor := FTheme.DefaultFG;
      40 .. 47: FCurrentAttributes.BackgroundColor := FTheme.AnsiColors[Param - 40];
      49: FCurrentAttributes.BackgroundColor := FTheme.DefaultBG;
      90 .. 97: FCurrentAttributes.ForegroundColor := FTheme.AnsiColors[Param - 90 + 8];
      100 .. 107: FCurrentAttributes.BackgroundColor := FTheme.AnsiColors[Param - 100 + 8];

      38, 48:
        begin
          if I + 1 < Length(Params) then
          begin
            Inc(I);
            case Params[I] of
              5:
                begin
                  if I + 1 < Length(Params) then
                  begin
                    Inc(I);
                    if Param = 38 then
                      FCurrentAttributes.ForegroundColor := GetColor256(Params[I])
                    else
                      FCurrentAttributes.BackgroundColor := GetColor256(Params[I]);
                  end;
                end;
              2:
                begin
                  if I + 3 < Length(Params) then
                  begin
                    if Param = 38 then
                      FCurrentAttributes.ForegroundColor := GetColorRGB(Params[I + 1], Params[I + 2], Params[I + 3])
                    else
                      FCurrentAttributes.BackgroundColor := GetColorRGB(Params[I + 1], Params[I + 2], Params[I + 3]);
                    Inc(I, 3);
                  end;
                end;
            end;
          end;
        end;
    end;
    Inc(I);
  end;
end;

function TAnsiParser.Parse(const Input: string;
  out Commands: TArray<TAnsiCommand>): Boolean;
var
  I: Integer;
  Ch: WideChar;
  Cmd: TAnsiCommand;
  CmdList: TList<TAnsiCommand>;
  Parts: TArray<string>;
  Params: TArray<Integer>;
  J: Integer;
  IsPrivateMode: Boolean;
  ParamStr: string;
begin
  Result := True;
  CmdList := TList<TAnsiCommand>.Create;
  try
    I := 1;
    while I <= Length(Input) do
    begin
      Ch := Input[I];

      case FState of
        psNormal:
          begin
            if Ch = #27 then // ESC
            begin
              FState := psEscape;
              FParamBuffer := '';
            end
            else if Ch = #14 then FUseG1 := True // Shift Out
            else if Ch = #15 then FUseG1 := False // Shift In
            else
            begin
              Cmd.Command := apcPrintChar;
              Cmd.Char := MapChar(Ch);
              Cmd.Attributes := FCurrentAttributes;
              SetLength(Cmd.Params, 0);
              CmdList.Add(Cmd);
            end;
          end;

        psEscape:
          begin
            if Ch = '[' then FState := psCSI
            else if Ch = ']' then FState := psOSC
            else if Ch = '(' then FState := psCharsetG0
            else if Ch = ')' then FState := psCharsetG1
            else if CharInSet(Ch, ['*', '+', '-', '.', '/']) then
            begin
              if I < Length(Input) then Inc(I);
              FState := psNormal;
            end
            else if CharInSet(Ch, ['=', '>', '7', '8', 'E', 'D', 'H', 'c']) then FState := psNormal
            else if Ch = 'M' then
            begin
              Cmd.Command := apcReverseIndex;
              Cmd.Char := #0;
              Cmd.Attributes := FCurrentAttributes;
              SetLength(Cmd.Params, 0);
              CmdList.Add(Cmd);
              FState := psNormal;
            end
            else FState := psNormal;
          end;

        psCharsetG0:
          begin
            case Ch of
              '0': FG0 := csLineDrawing;
              'B': FG0 := csASCII;
            end;
            FState := psNormal;
          end;

        psCharsetG1:
          begin
            case Ch of
              '0': FG1 := csLineDrawing;
              'B': FG1 := csASCII;
            end;
            FState := psNormal;
          end;

        psCSI:
          begin
            if CharInSet(Ch, ['0' .. '9', ';', '?']) then FParamBuffer := FParamBuffer + Ch
            else
            begin
              Cmd.Command := apcNone;
              Cmd.Char := #0;
              Cmd.Attributes := FCurrentAttributes;

              IsPrivateMode := False;
              if (FParamBuffer <> '') and (FParamBuffer[1] = '?') then
              begin
                IsPrivateMode := True;
                FParamBuffer := Copy(FParamBuffer, 2, Length(FParamBuffer) - 1);
              end;

              if FParamBuffer = '' then SetLength(Params, 0)
              else
              begin
                Parts := FParamBuffer.Split([';']);
                SetLength(Params, Length(Parts));
                for J := 0 to High(Parts) do
                begin
                  ParamStr := Parts[J];
                  if ParamStr = '' then Params[J] := 1
                  else
                  begin
                    if ParamStr.StartsWith('?') then ParamStr := Copy(ParamStr, 2, Length(ParamStr) - 1);
                    Params[J] := StrToIntDef(ParamStr, 1);
                  end;
                end;
              end;

              if (Ch = 'm') and (FParamBuffer = '') then begin SetLength(Params, 1); Params[0] := 0; end;
              if CharInSet(Ch, ['J', 'K']) and (FParamBuffer = '') then begin SetLength(Params, 1); Params[0] := 0; end;
              if CharInSet(Ch, ['H', 'f']) and (FParamBuffer = '') then begin SetLength(Params, 2); Params[0] := 1; Params[1] := 1; end;

              Cmd.Params := Params;

              case Ch of
                'A': Cmd.Command := apcCursorUp;
                'B': Cmd.Command := apcCursorDown;
                'C': Cmd.Command := apcCursorForward;
                'D': Cmd.Command := apcCursorBack;
                'E': Cmd.Command := apcCursorNextLine;
                'F': Cmd.Command := apcCursorPrevLine;
                'G': Cmd.Command := apcCursorHorizontalAbs;
                'H', 'f': Cmd.Command := apcCursorPosition;
                'd': Cmd.Command := apcVerticalPositionAbs;
                'e': Cmd.Command := apcVerticalPositionRel;
                '`': Cmd.Command := apcHorizPositionAbs;
                'a': Cmd.Command := apcHorizPositionRel;
                'Z': Cmd.Command := apcCursorBackwardTab;
                'J': Cmd.Command := apcEraseDisplay;
                'K': Cmd.Command := apcEraseLine;
                'X': Cmd.Command := apcEraseChar;
                'S': Cmd.Command := apcScrollUp;
                'T': Cmd.Command := apcScrollDown;
                'L': Cmd.Command := apcInsertLine;
                'M': Cmd.Command := apcDeleteLine;
                '@': Cmd.Command := apcInsertChar;
                'P': Cmd.Command := apcDeleteChar;
                'b': Cmd.Command := apcRepeatChar;
                'h': if IsPrivateMode then Cmd.Command := apcSetPrivateMode else Cmd.Command := apcSetMode;
                'l': if IsPrivateMode then Cmd.Command := apcResetPrivateMode else Cmd.Command := apcResetMode;
                'r': Cmd.Command := apcSetScrollingRegion;
                'p': Cmd.Command := apcSoftTerminalReset;
                'q': Cmd.Command := apcSetCursorStyle;
                's': Cmd.Command := apcSaveCursorPosition;
                'u': Cmd.Command := apcRestoreCursorPosition;
                'c': Cmd.Command := apcDeviceAttributes;
                'n': Cmd.Command := apcDeviceStatusReport;
                'g': Cmd.Command := apcTabClear;
                'm': begin Cmd.Command := apcSetGraphicsMode; ParseSGR(Params); Cmd.Attributes := FCurrentAttributes; end;
              end;

              if Cmd.Command <> apcNone then CmdList.Add(Cmd);
              FState := psNormal;
              FParamBuffer := '';
            end;
          end;

        psOSC:
          begin
            if Ch = #7 then begin FState := psNormal; FParamBuffer := ''; end
            else if (Ch = #27) and (I < Length(Input)) and (Input[I + 1] = '\') then begin FState := psNormal; FParamBuffer := ''; Inc(I); end
            else if (Ch = #27) and (I < Length(Input)) and (Input[I + 1] = ']') then FParamBuffer := ''
            else if I = Length(Input) then begin FState := psNormal; FParamBuffer := ''; end
            else FParamBuffer := FParamBuffer + Ch;
          end;
      end;
      Inc(I);
    end;
    Commands := CmdList.ToArray;
  finally
    CmdList.Free;
  end;
end;

end.
