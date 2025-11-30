unit Terminal.Buffer;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections,
  System.Types, System.Math,
  Terminal.Types, Terminal.AnsiParser, Terminal.Theme;

type
  TTerminalBuffer = class
  private
    FLines: TList<TTerminalLine>;
    FWidth: Integer;
    FHeight: Integer;
    FCursor: TTerminalCursor;
    FCurrentAttributes: TCharAttributes;
    FScrollback: TList<TTerminalLine>;
    FMaxScrollback: Integer;
    FLastChar: WideChar;
    FScrollTop: Integer;
    FScrollBottom: Integer;
    FAlternateBuffer: TList<TTerminalLine>;
    FUseAlternateBuffer: Boolean;
    FSavedCursorMain: TTerminalCursor;
    FSavedCursorAlt: TTerminalCursor;
    FSavedScrollTopMain: Integer;
    FSavedScrollBottomMain: Integer;
    FSavedScrollTopAlt: Integer;
    FSavedScrollBottomAlt: Integer;
    FSavedCursor: TTerminalCursor;
    FAppCursorKeys: Boolean;
    FTheme: TTerminalTheme;

    FLinesDirty: TArray<Boolean>;
    FVisualScrollDelta: Integer;
    FViewportOffset: Integer;

    FMouseModes: TMouseTrackingModes;
    FLastMouseCol: Integer;
    FLastMouseRow: Integer;

    // --- Переменные для выделения ---
    FSelStart: TPoint; // X, AbsoluteY
    FSelEnd: TPoint;   // X, AbsoluteY
    FHasSelection: Boolean;
    // -------------------------------

    function GetLine(Index: Integer): TTerminalLine;
    procedure SetLine(Index: Integer; const Value: TTerminalLine);
    function GetCurrentLines: TList<TTerminalLine>;
    procedure EnsureLine(Index: Integer);
    procedure ScrollUp(Lines: Integer = 1);
    procedure ScrollDown(Lines: Integer = 1);
    procedure RemapBufferColors(Buffer: TList<TTerminalLine>;
      OldTheme, NewTheme: TTerminalTheme);
    function CreateBlankLine: TTerminalLine;

    procedure SetDirty(LineIndex: Integer);
    procedure SetRangeDirty(FromIndex, ToIndex: Integer);

    // Внутренние методы для правильного сдвига
    procedure InternalScrollUp(Top, Bottom, Count: Integer);
    procedure InternalScrollDown(Top, Bottom, Count: Integer);

    // Внутренний метод для нормализации координат выделения
    procedure NormalizeSelection;

  public
    constructor Create(AWidth, AHeight: Integer; ATheme: TTerminalTheme);
    destructor Destroy; override;
    procedure Clear;
    procedure ClearLine(Y: Integer; Mode: Integer = 2);
    procedure WriteChar(Ch: WideChar; Attr: TCharAttributes);
    procedure WriteText(const Text: string; Attr: TCharAttributes);
    procedure ProcessCommand(const Cmd: TAnsiCommand);
    procedure MoveCursor(X, Y: Integer);
    procedure MoveCursorRelative(DX, DY: Integer);
    procedure InsertLine(Y: Integer; Count: Integer = 1);
    procedure DeleteLine(Y: Integer; Count: Integer = 1);
    procedure InsertChar(X, Y: Integer; Count: Integer = 1);
    procedure DeleteChar(X, Y: Integer; Count: Integer = 1);
    procedure EraseChar(X, Y: Integer; Count: Integer = 1);
    procedure SwitchToAlternateBuffer;
    procedure SwitchToMainBuffer;
    procedure AdvanceCursor;
    procedure Resize(NewWidth, NewHeight: Integer);
    procedure SetTheme(ATheme: TTerminalTheme);

    function IsLineDirty(Index: Integer): Boolean;
    procedure CleanLine(Index: Integer);
    procedure SetAllDirty;
    function GetAndResetVisualScrollDelta: Integer;

    procedure ScrollViewport(Delta: Integer);
    procedure ResetViewport;
    function GetRenderLine(Index: Integer): TTerminalLine;

    // --- Методы выделения ---
    procedure SetSelection(StartX, StartY, EndX, EndY: Integer);
    procedure ClearSelection;
    function IsCellSelected(X, ScreenY: Integer): Boolean;
    function GetSelectedText: string;
    function GetTotalLinesCount: Integer;
    function ScreenYToAbsolute(ScreenY: Integer): Integer;
    // ------------------------

    property Lines[Index: Integer]: TTerminalLine read GetLine write SetLine;
    property Width: Integer read FWidth;
    property Height: Integer read FHeight;
    property Cursor: TTerminalCursor read FCursor write FCursor;
    property CurrentAttributes: TCharAttributes read FCurrentAttributes
      write FCurrentAttributes;
    property Scrollback: TList<TTerminalLine> read FScrollback;
    property MaxScrollback: Integer read FMaxScrollback write FMaxScrollback;
    property AppCursorKeys: Boolean read FAppCursorKeys;

    property MouseModes: TMouseTrackingModes read FMouseModes;
    property LastMouseCol: Integer read FLastMouseCol write FLastMouseCol;
    property LastMouseRow: Integer read FLastMouseRow write FLastMouseRow;
    property ViewportOffset: Integer read FViewportOffset;

    property HasSelection: Boolean read FHasSelection;
  end;

implementation

{ TTerminalBuffer }

destructor TTerminalBuffer.Destroy;
begin
  FTheme.Free;
  FLines.Free;
  FScrollback.Free;
  FAlternateBuffer.Free;
  inherited;
end;

function TTerminalBuffer.CreateBlankLine: TTerminalLine;
var
  J: Integer;
begin
  SetLength(Result, FWidth);
  for J := 0 to FWidth - 1 do
  begin
    Result[J].Char := ' ';
    Result[J].Attributes := TCharAttributes.Default(FTheme);
  end;
end;

// --- ВНУТРЕННЯЯ ЛОГИКА СКРОЛЛА ---

procedure TTerminalBuffer.InternalScrollUp(Top, Bottom, Count: Integer);
var
  CurrentLines: TList<TTerminalLine>;
  I, Step: Integer;
begin
  CurrentLines := GetCurrentLines;
  EnsureLine(Bottom);

  for Step := 1 to Count do
  begin
    if (not FUseAlternateBuffer) and (Top = 0) and (Bottom = FHeight - 1) then
    begin
       FScrollback.Add(Copy(CurrentLines[Top]));
       if FScrollback.Count > FMaxScrollback then FScrollback.Delete(0);
    end;

    for I := Top to Bottom - 1 do
      CurrentLines[I] := CurrentLines[I + 1];

    CurrentLines[Bottom] := CreateBlankLine;
  end;

  SetRangeDirty(Top, Bottom);
end;

procedure TTerminalBuffer.InternalScrollDown(Top, Bottom, Count: Integer);
var
  CurrentLines: TList<TTerminalLine>;
  I, Step: Integer;
begin
  CurrentLines := GetCurrentLines;
  EnsureLine(Bottom);

  for Step := 1 to Count do
  begin
    for I := Bottom downto Top + 1 do
      CurrentLines[I] := CurrentLines[I - 1];

    CurrentLines[Top] := CreateBlankLine;
  end;

  SetRangeDirty(Top, Bottom);
end;

// ---------------------------------------------------

procedure TTerminalBuffer.ScrollUp(Lines: Integer);
var
  IsFullScreenScroll: Boolean;
  K: Integer;
begin
  IsFullScreenScroll := (FScrollTop = 0) and (FScrollBottom = FHeight - 1);

  InternalScrollUp(FScrollTop, FScrollBottom, Lines);

  if IsFullScreenScroll then
  begin
    Inc(FVisualScrollDelta, Lines);
    if Lines < Length(FLinesDirty) then
    begin
      Move(FLinesDirty[Lines], FLinesDirty[0], (Length(FLinesDirty) - Lines) * SizeOf(Boolean));
      for K := FHeight - Lines to FHeight - 1 do FLinesDirty[K] := True;
    end
    else
      SetAllDirty;
  end;
end;

procedure TTerminalBuffer.ScrollDown(Lines: Integer);
begin
  InternalScrollDown(FScrollTop, FScrollBottom, Lines);
end;

procedure TTerminalBuffer.DeleteLine(Y: Integer; Count: Integer);
var
  Limit: Integer;
begin
  if (Y < FScrollTop) or (Y > FScrollBottom) then Y := FScrollTop;

  Limit := FScrollBottom - Y + 1;
  if Count > Limit then Count := Limit;

  if Count > 0 then
    InternalScrollUp(Y, FScrollBottom, Count);
end;

procedure TTerminalBuffer.InsertLine(Y: Integer; Count: Integer);
var
  Limit: Integer;
begin
  if (Y < FScrollTop) or (Y > FScrollBottom) then Y := FScrollTop;

  Limit := FScrollBottom - Y + 1;
  if Count > Limit then Count := Limit;

  if Count > 0 then
    InternalScrollDown(Y, FScrollBottom, Count);
end;

// ---------------------------------------------------

function TTerminalBuffer.GetAndResetVisualScrollDelta: Integer;
begin
  Result := FVisualScrollDelta;
  FVisualScrollDelta := 0;
end;

procedure TTerminalBuffer.ScrollViewport(Delta: Integer);
begin
  if FUseAlternateBuffer then Exit;
  FViewportOffset := EnsureRange(FViewportOffset + Delta, 0, FScrollback.Count);
  SetAllDirty;
end;

procedure TTerminalBuffer.ResetViewport;
begin
  if FViewportOffset <> 0 then
  begin
    FViewportOffset := 0;
    SetAllDirty;
  end;
end;

function TTerminalBuffer.GetRenderLine(Index: Integer): TTerminalLine;
var
  TotalHistory: Integer;
  TargetIndex: Integer;
  CurrentLines: TList<TTerminalLine>;
begin
  if FUseAlternateBuffer then
  begin
    CurrentLines := FAlternateBuffer;
    if (Index >= 0) and (Index < CurrentLines.Count) then
      Result := CurrentLines[Index]
    else
      Result := nil;
    Exit;
  end;

  CurrentLines := FLines;
  TotalHistory := FScrollback.Count;
  TargetIndex := (TotalHistory + Index) - FViewportOffset;

  if TargetIndex < 0 then
    Result := nil
  else if TargetIndex < TotalHistory then
    Result := FScrollback[TargetIndex]
  else
  begin
    TargetIndex := TargetIndex - TotalHistory;
    if (TargetIndex >= 0) and (TargetIndex < CurrentLines.Count) then
      Result := CurrentLines[TargetIndex]
    else
      Result := nil;
  end;
end;

procedure TTerminalBuffer.SetDirty(LineIndex: Integer);
begin
  if (LineIndex >= 0) and (LineIndex < Length(FLinesDirty)) then
    FLinesDirty[LineIndex] := True;
end;

procedure TTerminalBuffer.SetRangeDirty(FromIndex, ToIndex: Integer);
var
  I: Integer;
begin
  for I := Max(0, FromIndex) to Min(High(FLinesDirty), ToIndex) do
    FLinesDirty[I] := True;
end;

procedure TTerminalBuffer.SetAllDirty;
var
  I: Integer;
begin
  for I := 0 to High(FLinesDirty) do
    FLinesDirty[I] := True;
  FVisualScrollDelta := 0;
end;

function TTerminalBuffer.IsLineDirty(Index: Integer): Boolean;
begin
  // Если есть выделение, мы всегда перерисовываем все,
  // так как выделение может меняться динамически
  if FHasSelection then Exit(True);

  if FViewportOffset > 0 then Exit(True);

  if (Index >= 0) and (Index < Length(FLinesDirty)) then
    Result := FLinesDirty[Index]
  else
    Result := True;
end;

procedure TTerminalBuffer.CleanLine(Index: Integer);
begin
  if (Index >= 0) and (Index < Length(FLinesDirty)) then
    FLinesDirty[Index] := False;
end;

procedure TTerminalBuffer.RemapBufferColors(Buffer: TList<TTerminalLine>;
  OldTheme, NewTheme: TTerminalTheme);
var
  I, J, K: Integer;
  Line: TTerminalLine;
  Attr: TCharAttributes;
begin
  if (Buffer = nil) or (OldTheme = nil) or (NewTheme = nil) then Exit;

  for I := 0 to Buffer.Count - 1 do
  begin
    Line := Buffer[I];
    if Line = nil then Continue;

    for J := 0 to Length(Line) - 1 do
    begin
      Attr := Line[J].Attributes;

      if Attr.ForegroundColor = OldTheme.DefaultFG then
      begin
        Line[J].Attributes.ForegroundColor := NewTheme.DefaultFG;
      end
      else
      begin
        for K := 0 to 15 do
        begin
          if Attr.ForegroundColor = OldTheme.AnsiColors[K] then
          begin
            Line[J].Attributes.ForegroundColor := NewTheme.AnsiColors[K];
            Break;
          end;
        end;
      end;

      if Attr.BackgroundColor = OldTheme.DefaultBG then
      begin
        Line[J].Attributes.BackgroundColor := NewTheme.DefaultBG;
      end
      else
      begin
        for K := 0 to 15 do
        begin
          if Attr.BackgroundColor = OldTheme.AnsiColors[K] then
          begin
            Line[J].Attributes.BackgroundColor := NewTheme.AnsiColors[K];
            Break;
          end;
        end;
      end;
    end;
  end;
end;

procedure TTerminalBuffer.SetTheme(ATheme: TTerminalTheme);
var
  OldTheme: TTerminalTheme;
begin
  OldTheme := TTerminalTheme.Create;
  try
    OldTheme.Assign(FTheme);
    FTheme.Assign(ATheme);
    FCurrentAttributes.Reset(FTheme);
    RemapBufferColors(FLines, OldTheme, FTheme);
    RemapBufferColors(FAlternateBuffer, OldTheme, FTheme);
    RemapBufferColors(FScrollback, OldTheme, FTheme);
    SetAllDirty;
  finally
    OldTheme.Free;
  end;
end;

procedure TTerminalBuffer.AdvanceCursor;
begin
  Inc(FCursor.Y);
  if (FCursor.Y >= FScrollTop) and (FCursor.Y <= FScrollBottom) then
  begin
    if FCursor.Y > FScrollBottom then
    begin
      ScrollUp(1);
      FCursor.Y := FScrollBottom;
    end;
  end
  else
  begin
    if FCursor.Y >= FHeight then
      FCursor.Y := FHeight - 1;
  end;
end;

procedure TTerminalBuffer.Clear;
var
  I: Integer;
  CurrentLines: TList<TTerminalLine>;
begin
  CurrentLines := GetCurrentLines;
  CurrentLines.Clear;
  for I := 0 to FHeight - 1 do
    CurrentLines.Add(CreateBlankLine);
  FCursor.X := 0;
  FCursor.Y := 0;
  ResetViewport;
  ClearSelection;
  SetAllDirty;
end;

procedure TTerminalBuffer.ClearLine(Y: Integer; Mode: Integer);
var
  Line: TTerminalLine;
  I, StartX, EndX: Integer;
  CurrentLines: TList<TTerminalLine>;
begin
  if (Y < 0) or (Y >= FHeight) then Exit;
  CurrentLines := GetCurrentLines;
  EnsureLine(Y);
  Line := CurrentLines[Y];

  case Mode of
    0: begin StartX := FCursor.X; EndX := FWidth - 1; end;
    1: begin StartX := 0; EndX := FCursor.X; end;
    2: begin StartX := 0; EndX := FWidth - 1; end;
  else Exit; end;

  for I := StartX to EndX do
  begin
    Line[I].Char := ' ';
    Line[I].Attributes := FCurrentAttributes;
  end;
  CurrentLines[Y] := Line;
  SetDirty(Y);
end;


constructor TTerminalBuffer.Create(AWidth, AHeight: Integer; ATheme: TTerminalTheme);
var
  I: Integer;
begin
  inherited Create;
  FTheme := TTerminalTheme.Create;
  FTheme.Assign(ATheme);

  FWidth := AWidth;
  FHeight := AHeight;
  FLines := TList<TTerminalLine>.Create;
  FScrollback := TList<TTerminalLine>.Create;
  FAlternateBuffer := TList<TTerminalLine>.Create;
  FUseAlternateBuffer := False;
  FMaxScrollback := 10000;
  FCursor.X := 0;
  FCursor.Y := 0;
  FCursor.Visible := True;
  FCurrentAttributes := TCharAttributes.Default(FTheme);
  FAppCursorKeys := False;
  FVisualScrollDelta := 0;
  FViewportOffset := 0;
  FMouseModes := [];
  FLastMouseCol := -1;
  FLastMouseRow := -1;

  // Инициализация выделения
  FHasSelection := False;
  FSelStart := TPoint.Create(0, 0);
  FSelEnd := TPoint.Create(0, 0);

  SetLength(FLinesDirty, FHeight);
  SetAllDirty;

  for I := 0 to FHeight - 1 do FLines.Add(CreateBlankLine);
  for I := 0 to FHeight - 1 do FAlternateBuffer.Add(CreateBlankLine);

  FScrollTop := 0;
  FScrollBottom := FHeight - 1;
  FSavedCursorMain := FCursor;
  FSavedCursorAlt := FCursor;
  FSavedScrollTopMain := 0;
  FSavedScrollBottomMain := FHeight - 1;
  FSavedScrollTopAlt := 0;
  FSavedScrollBottomAlt := FHeight - 1;
  FSavedCursor := FCursor;
  FLastChar := ' ';
end;

procedure TTerminalBuffer.EnsureLine(Index: Integer);
var
  CurrentLines: TList<TTerminalLine>;
begin
  CurrentLines := GetCurrentLines;
  while CurrentLines.Count <= Index do CurrentLines.Add(CreateBlankLine);
end;

procedure TTerminalBuffer.DeleteChar(X, Y: Integer; Count: Integer);
var
  Line: TTerminalLine;
  I: Integer;
  CurrentLines: TList<TTerminalLine>;
begin
  if (Y < 0) or (Y >= FHeight) or (X < 0) or (X >= FWidth) then Exit;
  CurrentLines := GetCurrentLines;
  EnsureLine(Y);
  Line := CurrentLines[Y];
  for I := X to FWidth - Count - 1 do
  begin
    if I + Count < FWidth then Line[I] := Line[I + Count];
  end;
  for I := FWidth - Count to FWidth - 1 do
  begin
    if I >= 0 then
    begin
      Line[I].Char := ' ';
      Line[I].Attributes := FCurrentAttributes;
    end;
  end;
  CurrentLines[Y] := Line;
  SetDirty(Y);
end;

procedure TTerminalBuffer.InsertChar(X, Y: Integer; Count: Integer);
var
  Line: TTerminalLine;
  I: Integer;
  CurrentLines: TList<TTerminalLine>;
begin
  if (Y < 0) or (Y >= FHeight) or (X < 0) or (X >= FWidth) then Exit;
  CurrentLines := GetCurrentLines;
  EnsureLine(Y);
  Line := CurrentLines[Y];
  for I := FWidth - 1 downto X + Count do
  begin
    if I - Count >= 0 then Line[I] := Line[I - Count];
  end;
  for I := X to X + Count - 1 do
  begin
    if I < FWidth then
    begin
      Line[I].Char := ' ';
      Line[I].Attributes := FCurrentAttributes;
    end;
  end;
  CurrentLines[Y] := Line;
  SetDirty(Y);
end;

procedure TTerminalBuffer.EraseChar(X, Y: Integer; Count: Integer);
var
  Line: TTerminalLine;
  I: Integer;
  CurrentLines: TList<TTerminalLine>;
begin
  if (Y < 0) or (Y >= FHeight) or (X < 0) or (X >= FWidth) then Exit;
  CurrentLines := GetCurrentLines;
  EnsureLine(Y);
  Line := CurrentLines[Y];
  for I := X to X + Count - 1 do
  begin
    if I < FWidth then
    begin
      Line[I].Char := ' ';
      Line[I].Attributes := FCurrentAttributes;
    end;
  end;
  CurrentLines[Y] := Line;
  SetDirty(Y);
end;

function TTerminalBuffer.GetCurrentLines: TList<TTerminalLine>;
begin
  if FUseAlternateBuffer then Result := FAlternateBuffer else Result := FLines;
end;

function TTerminalBuffer.GetLine(Index: Integer): TTerminalLine;
var
  CurrentLines: TList<TTerminalLine>;
begin
  CurrentLines := GetCurrentLines;
  if (Index >= 0) and (Index < CurrentLines.Count) then
  begin
    EnsureLine(Index);
    Result := CurrentLines[Index];
  end
  else Result := nil;
end;

procedure TTerminalBuffer.MoveCursor(X, Y: Integer);
begin
  FCursor.X := EnsureRange(X, 0, FWidth - 1);
  FCursor.Y := EnsureRange(Y, 0, FHeight - 1);
end;

procedure TTerminalBuffer.MoveCursorRelative(DX, DY: Integer);
begin
  MoveCursor(FCursor.X + DX, FCursor.Y + DY);
end;

procedure TTerminalBuffer.ProcessCommand(const Cmd: TAnsiCommand);
var
  Param1, Param2: Integer;
begin
  Param1 := 1; Param2 := 1;
  if Length(Cmd.Params) > 0 then Param1 := Cmd.Params[0];
  if Length(Cmd.Params) > 1 then Param2 := Cmd.Params[1];

  if (Cmd.Command = apcSetGraphicsMode) and (Length(Cmd.Params) = 0) then Param1 := 0;
  if (Cmd.Command = apcEraseDisplay) and (Length(Cmd.Params) = 0) then Param1 := 0;
  if (Cmd.Command = apcEraseLine) and (Length(Cmd.Params) = 0) then Param1 := 0;
  if (Cmd.Command = apcCursorPosition) and (Length(Cmd.Params) = 0) then
  begin Param1 := 1; Param2 := 1; end;

  if (Cmd.Command in [apcEraseDisplay, apcScrollUp, apcScrollDown, apcInsertLine, apcDeleteLine]) then
     ResetViewport;

  case Cmd.Command of
    apcPrintChar: WriteChar(Cmd.Char, Cmd.Attributes);
    apcCursorUp: MoveCursorRelative(0, -Param1);
    apcCursorDown: MoveCursorRelative(0, Param1);
    apcCursorForward: MoveCursorRelative(Param1, 0);
    apcCursorBack: MoveCursorRelative(-Param1, 0);
    apcCursorNextLine: begin FCursor.X := 0; MoveCursorRelative(0, Param1); end;
    apcCursorPrevLine: begin FCursor.X := 0; MoveCursorRelative(0, -Param1); end;
    apcCursorHorizontalAbs: MoveCursor(Param1 - 1, FCursor.Y);
    apcCursorPosition: MoveCursor(Param2 - 1, Param1 - 1);
    apcVerticalPositionAbs: MoveCursor(FCursor.X, Param1 - 1);
    apcVerticalPositionRel: MoveCursorRelative(0, Param1);
    apcHorizPositionAbs: MoveCursor(Param1 - 1, FCursor.Y);
    apcHorizPositionRel: MoveCursorRelative(Param1, 0);
    apcCursorBackwardTab: begin FCursor.X := ((FCursor.X div 8) - Param1) * 8; if FCursor.X < 0 then FCursor.X := 0; end;
    apcEraseDisplay:
      begin
        case Param1 of
          0: begin ClearLine(FCursor.Y, 0); for var I := FCursor.Y + 1 to FHeight - 1 do ClearLine(I, 2); end;
          1: begin for var I := 0 to FCursor.Y - 1 do ClearLine(I, 2); ClearLine(FCursor.Y, 1); end;
          2, 3: Clear;
        end;
      end;
    apcEraseLine: ClearLine(FCursor.Y, Param1);
    apcScrollUp: ScrollUp(Param1);
    apcScrollDown: ScrollDown(Param1);
    apcInsertLine: InsertLine(FCursor.Y, Param1);
    apcDeleteLine: DeleteLine(FCursor.Y, Param1);
    apcInsertChar: InsertChar(FCursor.X, FCursor.Y, Param1);
    apcDeleteChar: DeleteChar(FCursor.X, FCursor.Y, Param1);
    apcEraseChar: EraseChar(FCursor.X, FCursor.Y, Param1);
    apcRepeatChar: for Param2 := 1 to Param1 do WriteChar(FLastChar, FCurrentAttributes);
    apcSetGraphicsMode: FCurrentAttributes := Cmd.Attributes;
    apcSetMode: ;
    apcResetMode: ;
    apcSetPrivateMode:
      begin
        FLastMouseCol := -1; FLastMouseRow := -1;
        case Param1 of
          1: FAppCursorKeys := True;
          1000: Include(FMouseModes, mtm1000_Click);
          1002: Include(FMouseModes, mtm1002_Wheel);
          1003: Include(FMouseModes, mtm1003_Any);
          1006: Include(FMouseModes, mtm1006_SGR);
          1049, 1047: SwitchToAlternateBuffer;
          25: FCursor.Visible := True;
        end;
      end;
    apcResetPrivateMode:
      begin
        FLastMouseCol := -1; FLastMouseRow := -1;
        case Param1 of
          1: FAppCursorKeys := False;
          1000: Exclude(FMouseModes, mtm1000_Click);
          1002: Exclude(FMouseModes, mtm1002_Wheel);
          1003: Exclude(FMouseModes, mtm1003_Any);
          1006: Exclude(FMouseModes, mtm1006_SGR);
          1049, 1047: SwitchToMainBuffer;
          25: FCursor.Visible := False;
        end;
      end;
    apcSetScrollingRegion:
      begin
        if Length(Cmd.Params) >= 2 then
        begin
          FScrollTop := Max(0, Cmd.Params[0] - 1);
          FScrollBottom := Min(FHeight - 1, Cmd.Params[1] - 1);
          if FScrollTop >= FScrollBottom then
          begin
            FScrollTop := 0;
            FScrollBottom := FHeight - 1;
          end;
        end
        else begin FScrollTop := 0; FScrollBottom := FHeight - 1; end;
        FCursor.X := 0; FCursor.Y := FScrollTop;
      end;
    apcSoftTerminalReset: begin Clear; FCurrentAttributes.Reset(FTheme); FScrollTop := 0; FScrollBottom := FHeight - 1; SetAllDirty; end;
    apcSetCursorStyle: ;
    apcSaveCursorPosition: FSavedCursor := FCursor;
    apcRestoreCursorPosition: FCursor := FSavedCursor;
    apcDeviceAttributes: ;
    apcDeviceStatusReport: ;
    apcTabClear: ;
    apcReverseIndex: ScrollDown(1);
  end;
end;

procedure TTerminalBuffer.SetLine(Index: Integer; const Value: TTerminalLine);
var CurrentLines: TList<TTerminalLine>;
begin
  if (Index >= 0) and (Index < FHeight) then
  begin
    CurrentLines := GetCurrentLines;
    EnsureLine(Index);
    CurrentLines[Index] := Copy(Value);
    SetDirty(Index);
  end;
end;

procedure TTerminalBuffer.SwitchToAlternateBuffer;
begin
  if FUseAlternateBuffer then Exit;
  FSavedCursorMain := FCursor;
  FSavedScrollTopMain := FScrollTop;
  FSavedScrollBottomMain := FScrollBottom;
  FUseAlternateBuffer := True;
  FCursor := FSavedCursorAlt;
  FScrollTop := FSavedScrollTopAlt;
  FScrollBottom := FSavedScrollBottomAlt;
  FCurrentAttributes.Reset(FTheme);
  Clear;
  SetAllDirty;
end;

procedure TTerminalBuffer.SwitchToMainBuffer;
begin
  if not FUseAlternateBuffer then Exit;
  FSavedCursorAlt := FCursor;
  FSavedScrollTopAlt := FScrollTop;
  FSavedScrollBottomAlt := FScrollBottom;
  FUseAlternateBuffer := False;
  FCursor := FSavedCursorMain;
  FScrollTop := FSavedScrollTopMain;
  FScrollBottom := FSavedScrollBottomMain;
  SetAllDirty;
end;

procedure TTerminalBuffer.WriteChar(Ch: WideChar; Attr: TCharAttributes);
var
  Line: TTerminalLine;
  CurrentLines: TList<TTerminalLine>;
begin
  ResetViewport;
  CurrentLines := GetCurrentLines;
  if (FCursor.Y < 0) or (FCursor.Y >= FHeight) or (FCursor.X < 0) or (FCursor.X > FWidth) then
  begin
    FCursor.X := EnsureRange(FCursor.X, 0, FWidth - 1);
    FCursor.Y := EnsureRange(FCursor.Y, 0, FHeight - 1);
  end;

  case Ch of
    #7: Exit; // Игнорируем звонок (Bell), чтобы не было артефактов
    #10: begin if FCursor.Y = FScrollBottom then ScrollUp(1) else begin Inc(FCursor.Y); if FCursor.Y >= FHeight then FCursor.Y := FHeight - 1; end; Exit; end;
    #13: begin FCursor.X := 0; Exit; end;
    #8: begin if FCursor.X > 0 then Dec(FCursor.X); Exit; end;
    #9: begin FCursor.X := ((FCursor.X div 8) + 1) * 8; if FCursor.X >= FWidth then begin FCursor.X := 0; if FCursor.Y = FScrollBottom then ScrollUp(1) else begin Inc(FCursor.Y); if FCursor.Y >= FHeight then FCursor.Y := FHeight - 1; end; end; Exit; end;
  end;

  if FCursor.X >= FWidth then
  begin
    FCursor.X := 0;
    if FCursor.Y = FScrollBottom then ScrollUp(1) else begin Inc(FCursor.Y); if FCursor.Y >= FHeight then FCursor.Y := FHeight - 1; end;
  end;

  if FCursor.Y > FScrollBottom then FCursor.Y := FScrollBottom;

  EnsureLine(FCursor.Y);
  Line := CurrentLines[FCursor.Y];
  Line[FCursor.X].Char := Ch;
  Line[FCursor.X].Attributes := Attr;
  CurrentLines[FCursor.Y] := Line;
  FLastChar := Ch;
  Inc(FCursor.X);
  SetDirty(FCursor.Y);
end;

procedure TTerminalBuffer.WriteText(const Text: string; Attr: TCharAttributes);
var I: Integer;
begin
  for I := 1 to Length(Text) do WriteChar(Text[I], Attr);
end;

procedure TTerminalBuffer.Resize(NewWidth, NewHeight: Integer);
var
  I: Integer;
  OldMainLines, OldAltLines: TList<TTerminalLine>;

  procedure ResizeBuffer(Lines: TList<TTerminalLine>; OldLines: TList<TTerminalLine>);
  var
    I, J: Integer;
  begin
    Lines.Clear;
    for I := 0 to NewHeight - 1 do
    begin
      Lines.Add(CreateBlankLine);
    end;

    for I := 0 to Min(OldLines.Count - 1, NewHeight - 1) do
    begin
      if OldLines[I] <> nil then
      begin
        var LineToCopy: TTerminalLine := Copy(OldLines[I]);
        if Length(LineToCopy) > NewWidth then
           SetLength(LineToCopy, NewWidth)
        else if Length(LineToCopy) < NewWidth then
        begin
           var OldLen := Length(LineToCopy);
           SetLength(LineToCopy, NewWidth);
           for J := OldLen to NewWidth - 1 do
           begin
             LineToCopy[J].Char := ' ';
             LineToCopy[J].Attributes := TCharAttributes.Default(FTheme);
           end;
        end;

        Lines[I] := LineToCopy;
      end;
    end;
  end;

  procedure ResizeScrollback(Lines: TList<TTerminalLine>);
  var
    I, J: Integer;
  begin
    for I := 0 to Lines.Count - 1 do
    begin
        if Lines[I] <> nil then
        begin
           if Length(Lines[I]) > NewWidth then
             SetLength(Lines.List[I], NewWidth)
           else if Length(Lines[I]) < NewWidth then
           begin
             var OldLen := Length(Lines[I]);
             SetLength(Lines.List[I], NewWidth);
             for J := OldLen to NewWidth - 1 do
             begin
               Lines.List[I][J].Char := ' ';
               Lines.List[I][J].Attributes := TCharAttributes.Default(FTheme);
             end;
           end;
        end;
    end;
  end;

begin
  if (NewWidth = FWidth) and (NewHeight = FHeight) then Exit;

  OldMainLines := TList<TTerminalLine>.Create;
  OldAltLines := TList<TTerminalLine>.Create;
  try
    for I := 0 to FLines.Count - 1 do OldMainLines.Add(Copy(FLines[I]));
    for I := 0 to FAlternateBuffer.Count - 1 do OldAltLines.Add(Copy(FAlternateBuffer[I]));

    FWidth := NewWidth;
    FHeight := NewHeight;

    SetLength(FLinesDirty, FHeight);

    FScrollTop := 0;
    FScrollBottom := FHeight - 1;

    ResizeScrollback(FScrollback);

    ResizeBuffer(FLines, OldMainLines);
    ResizeBuffer(FAlternateBuffer, OldAltLines);

    FCursor.X := EnsureRange(FCursor.X, 0, FWidth - 1);
    FCursor.Y := EnsureRange(FCursor.Y, 0, FHeight - 1);

    FSavedScrollTopMain := 0;
    FSavedScrollBottomMain := FHeight - 1;
    FSavedScrollTopAlt := 0;
    FSavedScrollBottomAlt := FHeight - 1;

    FSavedCursorMain.X := EnsureRange(FSavedCursorMain.X, 0, FWidth - 1);
    FSavedCursorMain.Y := EnsureRange(FSavedCursorMain.Y, 0, FHeight - 1);
    FSavedCursorAlt.X := EnsureRange(FSavedCursorAlt.X, 0, FWidth - 1);
    FSavedCursorAlt.Y := EnsureRange(FSavedCursorAlt.Y, 0, FHeight - 1);

    ClearSelection; // Сбрасываем выделение при ресайзе, чтобы не было багов
    SetAllDirty;

  finally
    OldMainLines.Free;
    OldAltLines.Free;
  end;
end;

// --- РЕАЛИЗАЦИЯ НОВЫХ МЕТОДОВ ВЫДЕЛЕНИЯ ---

function TTerminalBuffer.GetTotalLinesCount: Integer;
begin
  if FUseAlternateBuffer then
    Result := FAlternateBuffer.Count
  else
    Result := FScrollback.Count + FLines.Count;
end;

function TTerminalBuffer.ScreenYToAbsolute(ScreenY: Integer): Integer;
begin
  if FUseAlternateBuffer then
    Result := ScreenY
  else
    Result := (FScrollback.Count + ScreenY) - FViewportOffset;
end;

procedure TTerminalBuffer.NormalizeSelection;
var
  Swap: TPoint;
begin
  if (FSelStart.Y > FSelEnd.Y) or ((FSelStart.Y = FSelEnd.Y) and (FSelStart.X > FSelEnd.X)) then
  begin
    Swap := FSelStart;
    FSelStart := FSelEnd;
    FSelEnd := Swap;
  end;
end;

procedure TTerminalBuffer.SetSelection(StartX, StartY, EndX, EndY: Integer);
begin
  FSelStart := TPoint.Create(StartX, StartY);
  FSelEnd := TPoint.Create(EndX, EndY);
  FHasSelection := True;
  NormalizeSelection;
  SetAllDirty;
end;

procedure TTerminalBuffer.ClearSelection;
begin
  if FHasSelection then
  begin
    FHasSelection := False;
    SetAllDirty;
  end;
end;

function TTerminalBuffer.IsCellSelected(X, ScreenY: Integer): Boolean;
var
  AbsY: Integer;
begin
  if not FHasSelection then Exit(False);

  AbsY := ScreenYToAbsolute(ScreenY);

  if (AbsY > FSelStart.Y) and (AbsY < FSelEnd.Y) then Exit(True);

  if (AbsY = FSelStart.Y) and (AbsY = FSelEnd.Y) then
    Exit((X >= FSelStart.X) and (X <= FSelEnd.X));

  if AbsY = FSelStart.Y then Exit(X >= FSelStart.X);
  if AbsY = FSelEnd.Y then Exit(X <= FSelEnd.X);

  Result := False;
end;

function TTerminalBuffer.GetSelectedText: string;
var
  X, StartX, EndX: Integer;
  Line: TTerminalLine;
  AbsY: Integer;
  ResultStr: TStringBuilder;
  SBCount: Integer;

  function GetLineByAbsIndex(Idx: Integer): TTerminalLine;
  begin
    if FUseAlternateBuffer then
    begin
       if (Idx >= 0) and (Idx < FAlternateBuffer.Count) then Result := FAlternateBuffer[Idx]
       else Result := nil;
    end
    else
    begin
      SBCount := FScrollback.Count;
      if Idx < SBCount then
        Result := FScrollback[Idx]
      else if Idx < SBCount + FLines.Count then
        Result := FLines[Idx - SBCount]
      else
        Result := nil;
    end;
  end;

begin
  if not FHasSelection then Exit('');

  ResultStr := TStringBuilder.Create;
  try
    for AbsY := FSelStart.Y to FSelEnd.Y do
    begin
      Line := GetLineByAbsIndex(AbsY);
      if Line = nil then Continue;

      if AbsY = FSelStart.Y then StartX := FSelStart.X else StartX := 0;
      if AbsY = FSelEnd.Y then EndX := FSelEnd.X else EndX := Length(Line) - 1;

      if EndX >= Length(Line) then EndX := Length(Line) - 1;

      for X := StartX to EndX do
        ResultStr.Append(Line[X].Char);

      if AbsY < FSelEnd.Y then
        ResultStr.Append(sLineBreak);
    end;
    Result := ResultStr.ToString;
  finally
    ResultStr.Free;
  end;
end;

end.
