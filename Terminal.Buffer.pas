unit Terminal.Buffer;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections,
  System.Types, System.Math, System.Character,
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
    FLastChar: string;
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
    FSelStart: TPoint;
    FSelEnd: TPoint;
    FHasSelection: Boolean;
    FBracketedPaste: Boolean;

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
    procedure InternalScrollUp(Top, Bottom, Count: Integer);
    procedure InternalScrollDown(Top, Bottom, Count: Integer);
    procedure NormalizeSelection;
    
    // Очистка "хвоста" wide-символа если перезаписываем
    procedure ClearWideCharTail(Line: TTerminalLine; X: Integer);
    // Очистка "головы" wide-символа если пишем в его хвост
    procedure ClearWideCharHead(Line: TTerminalLine; X: Integer);

  public
    constructor Create(AWidth, AHeight: Integer; ATheme: TTerminalTheme);
    destructor Destroy; override;
    procedure Clear;
    procedure ClearLine(Y: Integer; Mode: Integer = 2);

    // ПРИНИМАЕТ STRING
    procedure WriteChar(Ch: string; Attr: TCharAttributes);
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
    procedure SetSelection(StartX, StartY, EndX, EndY: Integer);
    procedure ClearSelection;
    function IsCellSelected(X, ScreenY: Integer): Boolean;
    function GetSelectedText: string;
    function GetTotalLinesCount: Integer;
    function ScreenYToAbsolute(ScreenY: Integer): Integer;

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
    property IsAlternateBuffer: Boolean read FUseAlternateBuffer;
    property BracketedPaste: Boolean read FBracketedPaste;
  end;

implementation

{ TTerminalBuffer }

constructor TTerminalBuffer.Create(AWidth, AHeight: Integer; ATheme: TTerminalTheme);
var
  I: Integer;
begin
  inherited Create;
  FTheme := ATheme;
  FWidth := AWidth;
  FHeight := AHeight;
  FMaxScrollback := 10000;
  FScrollTop := 0;
  FScrollBottom := FHeight - 1;
  FUseAlternateBuffer := False;
  FAppCursorKeys := False;
  FVisualScrollDelta := 0;
  FViewportOffset := 0;
  FMouseModes := [];
  FLastMouseCol := 1;
  FLastMouseRow := 1;
  FHasSelection := False;
  FBracketedPaste := False;

  FLines := TList<TTerminalLine>.Create;
  FScrollback := TList<TTerminalLine>.Create;
  FAlternateBuffer := TList<TTerminalLine>.Create;

  for I := 0 to FHeight - 1 do
    FLines.Add(CreateBlankLine);

  SetLength(FLinesDirty, FHeight);
  SetAllDirty;

  FCursor.X := 0;
  FCursor.Y := 0;
  FCursor.Visible := True;
  FCurrentAttributes := TCharAttributes.Default(FTheme);
  FLastChar := ' ';
end;

destructor TTerminalBuffer.Destroy;
begin
  // НЕ освобождаем FTheme - он внешний!
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
    Result[J].Width := 1;  // Обычная ширина
  end;
end;

procedure TTerminalBuffer.ClearWideCharTail(Line: TTerminalLine; X: Integer);
begin
  // Если текущая ячейка — wide (Width=2), очищаем следующую ячейку (хвост)
  if (X < Length(Line)) and (Line[X].Width = 2) then
  begin
    if (X + 1 < Length(Line)) and (Line[X + 1].Width = 0) then
    begin
      Line[X + 1].Char := ' ';
      Line[X + 1].Width := 1;
      Line[X + 1].Attributes := TCharAttributes.Default(FTheme);
    end;
  end;
end;

procedure TTerminalBuffer.ClearWideCharHead(Line: TTerminalLine; X: Integer);
begin
  // Если текущая ячейка — хвост wide-символа (Width=0), очищаем голову
  if (X < Length(Line)) and (Line[X].Width = 0) then
  begin
    if (X > 0) and (Line[X - 1].Width = 2) then
    begin
      Line[X - 1].Char := ' ';
      Line[X - 1].Width := 1;
      Line[X - 1].Attributes := TCharAttributes.Default(FTheme);
    end;
  end;
end;

function TTerminalBuffer.GetCurrentLines: TList<TTerminalLine>;
begin
  if FUseAlternateBuffer then
    Result := FAlternateBuffer
  else
    Result := FLines;
end;

function TTerminalBuffer.GetLine(Index: Integer): TTerminalLine;
var
  CurrentLines: TList<TTerminalLine>;
begin
  CurrentLines := GetCurrentLines;
  if (Index >= 0) and (Index < CurrentLines.Count) then
    Result := CurrentLines[Index]
  else
    Result := nil;
end;

procedure TTerminalBuffer.SetLine(Index: Integer; const Value: TTerminalLine);
var
  CurrentLines: TList<TTerminalLine>;
begin
  CurrentLines := GetCurrentLines;
  if (Index >= 0) and (Index < CurrentLines.Count) then
  begin
    CurrentLines[Index] := Value;
    SetDirty(Index);
  end;
end;

procedure TTerminalBuffer.EnsureLine(Index: Integer);
var
  CurrentLines: TList<TTerminalLine>;
begin
  CurrentLines := GetCurrentLines;
  while CurrentLines.Count <= Index do
    CurrentLines.Add(CreateBlankLine);
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
  for I := FromIndex to ToIndex do
    SetDirty(I);
end;

procedure TTerminalBuffer.SetAllDirty;
var
  I: Integer;
begin
  for I := 0 to High(FLinesDirty) do
    FLinesDirty[I] := True;
end;

function TTerminalBuffer.IsLineDirty(Index: Integer): Boolean;
begin
  if (Index >= 0) and (Index < Length(FLinesDirty)) then
    Result := FLinesDirty[Index]
  else
    Result := False;
end;

procedure TTerminalBuffer.CleanLine(Index: Integer);
begin
  if (Index >= 0) and (Index < Length(FLinesDirty)) then
    FLinesDirty[Index] := False;
end;

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
      if FScrollback.Count > FMaxScrollback then
        FScrollback.Delete(0);
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
      Move(FLinesDirty[Lines], FLinesDirty[0], (Length(FLinesDirty) - Lines) *
        SizeOf(Boolean));
      for K := FHeight - Lines to FHeight - 1 do
        FLinesDirty[K] := True;
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
  if (Y < FScrollTop) or (Y > FScrollBottom) then
    Y := FScrollTop;
  Limit := FScrollBottom - Y + 1;
  if Count > Limit then
    Count := Limit;
  if Count > 0 then
    InternalScrollUp(Y, FScrollBottom, Count);
end;

procedure TTerminalBuffer.InsertLine(Y: Integer; Count: Integer);
var
  Limit: Integer;
begin
  if (Y < FScrollTop) or (Y > FScrollBottom) then
    Y := FScrollTop;
  Limit := FScrollBottom - Y + 1;
  if Count > Limit then
    Count := Limit;
  if Count > 0 then
    InternalScrollDown(Y, FScrollBottom, Count);
end;

function TTerminalBuffer.GetAndResetVisualScrollDelta: Integer;
begin
  Result := FVisualScrollDelta;
  FVisualScrollDelta := 0;
end;

procedure TTerminalBuffer.ScrollViewport(Delta: Integer);
begin
  if FUseAlternateBuffer then
    Exit;
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
  TotalHistory, TargetIndex: Integer;
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
  FScrollTop := 0;
  FScrollBottom := FHeight - 1;
  SetAllDirty;
end;

procedure TTerminalBuffer.ClearLine(Y: Integer; Mode: Integer);
var
  CurrentLines: TList<TTerminalLine>;
  Line: TTerminalLine;
  I, StartX, EndX: Integer;
begin
  CurrentLines := GetCurrentLines;
  if (Y < 0) or (Y >= CurrentLines.Count) then Exit;
  
  Line := CurrentLines[Y];
  
  case Mode of
    0: begin StartX := FCursor.X; EndX := FWidth - 1; end;  // От курсора до конца
    1: begin StartX := 0; EndX := FCursor.X; end;           // От начала до курсора
    2: begin StartX := 0; EndX := FWidth - 1; end;          // Вся строка
  else
    Exit;
  end;
  
  for I := StartX to EndX do
  begin
    if I < Length(Line) then
    begin
      Line[I].Char := ' ';
      Line[I].Attributes := FCurrentAttributes;
      Line[I].Width := 1;
    end;
  end;
  
  CurrentLines[Y] := Line;
  SetDirty(Y);
end;

procedure TTerminalBuffer.WriteChar(Ch: string; Attr: TCharAttributes);
var
  CurrentLines: TList<TTerminalLine>;
  Line: TTerminalLine;
  C: Char;
  CharWidth: Integer;
  ShouldMerge: Boolean;
  PrevChar: string;
begin
  ResetViewport;
  CurrentLines := GetCurrentLines;
  
  // Коррекция позиции курсора
  if (FCursor.Y < 0) or (FCursor.Y >= FHeight) or (FCursor.X < 0) or
    (FCursor.X > FWidth) then
  begin
    FCursor.X := EnsureRange(FCursor.X, 0, FWidth - 1);
    FCursor.Y := EnsureRange(FCursor.Y, 0, FHeight - 1);
  end;

  // Обработка управляющих символов
  if Length(Ch) = 1 then
  begin
    C := Ch[1];
    case C of
      #7:  // Bell
        Exit;
      #10: // Line Feed
        begin
          if FCursor.Y = FScrollBottom then
            ScrollUp(1)
          else
          begin
            Inc(FCursor.Y);
            if FCursor.Y >= FHeight then
              FCursor.Y := FHeight - 1;
          end;
          Exit;
        end;
      #13: // Carriage Return
        begin
          FCursor.X := 0;
          Exit;
        end;
      #8:  // Backspace
        begin
          if FCursor.X > 0 then
            Dec(FCursor.X);
          Exit;
        end;
      #9:  // Tab
        begin
          FCursor.X := ((FCursor.X div 8) + 1) * 8;
          if FCursor.X >= FWidth then
          begin
            FCursor.X := 0;
            if FCursor.Y = FScrollBottom then
              ScrollUp(1)
            else
            begin
              Inc(FCursor.Y);
              if FCursor.Y >= FHeight then
                FCursor.Y := FHeight - 1;
            end;
          end;
          Exit;
        end;
    end;
  end;

  // Zero-width символы (ZWJ, variation selectors) — склеиваем с предыдущим
  if IsZeroWidthChar(Ch) then
  begin
    if FCursor.X > 0 then
    begin
      EnsureLine(FCursor.Y);
      Line := CurrentLines[FCursor.Y];
      Line[FCursor.X - 1].Char := Line[FCursor.X - 1].Char + Ch;
      CurrentLines[FCursor.Y] := Line;
      SetDirty(FCursor.Y);
    end;
    Exit; // Не двигаем курсор!
  end;

  // Получаем ширину символа
  CharWidth := GetCharDisplayWidth(Ch);
  if CharWidth = 0 then
    Exit;

  // Автоперенос строки
  if FCursor.X >= FWidth then
  begin
    FCursor.X := 0;
    if FCursor.Y = FScrollBottom then
      ScrollUp(1)
    else
    begin
      Inc(FCursor.Y);
      if FCursor.Y >= FHeight then
        FCursor.Y := FHeight - 1;
    end;
  end;
  
  // Для wide-символов проверяем, влезет ли
  if (CharWidth = 2) and (FCursor.X = FWidth - 1) then
  begin
    // Wide-символ не влезает — переносим на следующую строку
    FCursor.X := 0;
    if FCursor.Y = FScrollBottom then
      ScrollUp(1)
    else
    begin
      Inc(FCursor.Y);
      if FCursor.Y >= FHeight then
        FCursor.Y := FHeight - 1;
    end;
  end;

  if FCursor.Y > FScrollBottom then
    FCursor.Y := FScrollBottom;
    
  EnsureLine(FCursor.Y);
  Line := CurrentLines[FCursor.Y];

  // Проверка на склейку ZWJ-последовательностей (для combining marks)
  ShouldMerge := False;
  if (Length(Ch) > 0) and (FCursor.X > 0) then
  begin
    PrevChar := Line[FCursor.X - 1].Char;
    // Если предыдущий символ заканчивается на ZWJ — склеиваем
    if (Length(PrevChar) > 0) and (PrevChar[Length(PrevChar)] = #$200D) then
      ShouldMerge := True;
  end;

  if ShouldMerge then
  begin
    Line[FCursor.X - 1].Char := Line[FCursor.X - 1].Char + Ch;
    CurrentLines[FCursor.Y] := Line;
    SetDirty(FCursor.Y);
    Exit;
  end;

  // Очищаем старые wide-символы если перезаписываем
  ClearWideCharHead(Line, FCursor.X);
  ClearWideCharTail(Line, FCursor.X);

  // Записываем символ
  Line[FCursor.X].Char := Ch;
  Line[FCursor.X].Attributes := Attr;
  Line[FCursor.X].Width := CharWidth;
  
  // Если wide — помечаем следующую ячейку как продолжение
  if (CharWidth = 2) and (FCursor.X + 1 < FWidth) then
  begin
    ClearWideCharHead(Line, FCursor.X + 1);
    ClearWideCharTail(Line, FCursor.X + 1);
    Line[FCursor.X + 1].Char := '';
    Line[FCursor.X + 1].Width := 0;  // Маркер "продолжение"
    Line[FCursor.X + 1].Attributes := Attr;
  end;

  CurrentLines[FCursor.Y] := Line;
  
  if Length(Ch) > 1 then
    FLastChar := ' '
  else
    FLastChar := Ch;
    
  Inc(FCursor.X, CharWidth);
  SetDirty(FCursor.Y);
end;

procedure TTerminalBuffer.WriteText(const Text: string; Attr: TCharAttributes);
var
  I: Integer;
  S: string;
  Ch: Char;
begin
  I := 1;
  while I <= Length(Text) do
  begin
    S := '';
    Ch := Text[I];
    S := S + Ch;
    Inc(I);
    // Если суррогатная пара, берем второй символ
    if TCharacter.IsHighSurrogate(Ch) and (I <= Length(Text)) and
      TCharacter.IsLowSurrogate(Text[I]) then
    begin
      S := S + Text[I];
      Inc(I);
    end;
    WriteChar(S, Attr);
  end;
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

procedure TTerminalBuffer.AdvanceCursor;
begin
  Inc(FCursor.X);
  if FCursor.X >= FWidth then
  begin
    FCursor.X := 0;
    Inc(FCursor.Y);
    if FCursor.Y >= FHeight then
      FCursor.Y := FHeight - 1;
  end;
end;

procedure TTerminalBuffer.InsertChar(X, Y: Integer; Count: Integer);
var
  CurrentLines: TList<TTerminalLine>;
  Line: TTerminalLine;
  I: Integer;
begin
  CurrentLines := GetCurrentLines;
  if (Y < 0) or (Y >= CurrentLines.Count) then Exit;
  Line := CurrentLines[Y];
  
  for I := FWidth - 1 downto X + Count do
  begin
    if I < Length(Line) then
      Line[I] := Line[I - Count];
  end;
  
  for I := X to Min(X + Count - 1, FWidth - 1) do
  begin
    if I < Length(Line) then
    begin
      Line[I].Char := ' ';
      Line[I].Attributes := FCurrentAttributes;
      Line[I].Width := 1;
    end;
  end;
  
  CurrentLines[Y] := Line;
  SetDirty(Y);
end;

procedure TTerminalBuffer.DeleteChar(X, Y: Integer; Count: Integer);
var
  CurrentLines: TList<TTerminalLine>;
  Line: TTerminalLine;
  I: Integer;
begin
  CurrentLines := GetCurrentLines;
  if (Y < 0) or (Y >= CurrentLines.Count) then Exit;
  Line := CurrentLines[Y];
  
  for I := X to FWidth - 1 - Count do
  begin
    if (I < Length(Line)) and (I + Count < Length(Line)) then
      Line[I] := Line[I + Count];
  end;
  
  for I := Max(X, FWidth - Count) to FWidth - 1 do
  begin
    if I < Length(Line) then
    begin
      Line[I].Char := ' ';
      Line[I].Attributes := FCurrentAttributes;
      Line[I].Width := 1;
    end;
  end;
  
  CurrentLines[Y] := Line;
  SetDirty(Y);
end;

procedure TTerminalBuffer.EraseChar(X, Y: Integer; Count: Integer);
var
  CurrentLines: TList<TTerminalLine>;
  Line: TTerminalLine;
  I: Integer;
begin
  CurrentLines := GetCurrentLines;
  if (Y < 0) or (Y >= CurrentLines.Count) then Exit;
  Line := CurrentLines[Y];
  
  for I := X to Min(X + Count - 1, FWidth - 1) do
  begin
    if I < Length(Line) then
    begin
      Line[I].Char := ' ';
      Line[I].Attributes := FCurrentAttributes;
      Line[I].Width := 1;
    end;
  end;
  
  CurrentLines[Y] := Line;
  SetDirty(Y);
end;

procedure TTerminalBuffer.SwitchToAlternateBuffer;
var
  I: Integer;
begin
  if FUseAlternateBuffer then Exit;
  
  // Сохраняем состояние main buffer
  FSavedCursorMain := FCursor;
  FSavedScrollTopMain := FScrollTop;
  FSavedScrollBottomMain := FScrollBottom;
  
  // Инициализируем alternate buffer
  FAlternateBuffer.Clear;
  for I := 0 to FHeight - 1 do
    FAlternateBuffer.Add(CreateBlankLine);
  
  FUseAlternateBuffer := True;
  FCursor := FSavedCursorAlt;
  FScrollTop := FSavedScrollTopAlt;
  FScrollBottom := FSavedScrollBottomAlt;
  
  if FScrollBottom = 0 then
    FScrollBottom := FHeight - 1;
  
  SetAllDirty;
end;

procedure TTerminalBuffer.SwitchToMainBuffer;
begin
  if not FUseAlternateBuffer then Exit;
  
  // Сохраняем состояние alternate buffer
  FSavedCursorAlt := FCursor;
  FSavedScrollTopAlt := FScrollTop;
  FSavedScrollBottomAlt := FScrollBottom;
  
  FUseAlternateBuffer := False;
  FCursor := FSavedCursorMain;
  FScrollTop := FSavedScrollTopMain;
  FScrollBottom := FSavedScrollBottomMain;
  
  if FScrollBottom = 0 then
    FScrollBottom := FHeight - 1;
  
  SetAllDirty;
end;

procedure TTerminalBuffer.SetTheme(ATheme: TTerminalTheme);
var
  OldTheme: TTerminalTheme;
begin
  if FTheme = ATheme then Exit;
  
  OldTheme := TTerminalTheme.Create;
  try
    OldTheme.Assign(FTheme);
    FTheme.Assign(ATheme);
    
    RemapBufferColors(FLines, OldTheme, FTheme);
    RemapBufferColors(FAlternateBuffer, OldTheme, FTheme);
    RemapBufferColors(FScrollback, OldTheme, FTheme);
    
    FCurrentAttributes.Reset(FTheme);
  finally
    OldTheme.Free;
  end;
  
  SetAllDirty;
end;

procedure TTerminalBuffer.RemapBufferColors(Buffer: TList<TTerminalLine>;
  OldTheme, NewTheme: TTerminalTheme);
var
  I, J, K: Integer;
  Line: TTerminalLine;
begin
  for I := 0 to Buffer.Count - 1 do
  begin
    Line := Buffer[I];
    for J := 0 to High(Line) do
    begin
      // Remap foreground
      if Line[J].Attributes.ForegroundColor = OldTheme.DefaultFG then
        Line[J].Attributes.ForegroundColor := NewTheme.DefaultFG
      else
        for K := 0 to 15 do
          if Line[J].Attributes.ForegroundColor = OldTheme.AnsiColors[K] then
          begin
            Line[J].Attributes.ForegroundColor := NewTheme.AnsiColors[K];
            Break;
          end;
      
      // Remap background
      if Line[J].Attributes.BackgroundColor = OldTheme.DefaultBG then
        Line[J].Attributes.BackgroundColor := NewTheme.DefaultBG
      else
        for K := 0 to 15 do
          if Line[J].Attributes.BackgroundColor = OldTheme.AnsiColors[K] then
          begin
            Line[J].Attributes.BackgroundColor := NewTheme.AnsiColors[K];
            Break;
          end;
    end;
    Buffer[I] := Line;
  end;
end;

procedure TTerminalBuffer.Resize(NewWidth, NewHeight: Integer);
var
  I, J: Integer;
  NewLine: TTerminalLine;
  LinesDiff: Integer;
  LineToMove: TTerminalLine;
begin
  if (NewWidth <= 0) or (NewHeight <= 0) then Exit;
  if (NewWidth = FWidth) and (NewHeight = FHeight) then Exit;

  // 1. ИЗМЕНЕНИЕ ВЫСОТЫ (Умный скроллинг)
  LinesDiff := NewHeight - FHeight;

  if LinesDiff < 0 then
  begin
    // === ОКНО УМЕНЬШАЕТСЯ (СУЖЕНИЕ) ===
    // Мы должны убрать ВЕРХНИЕ строки экрана в историю,
    // чтобы НИЖНИЕ (где курсор) остались на экране.

    for I := 1 to Abs(LinesDiff) do
    begin
      // Если это не Alt-буфер, сохраняем верхнюю строку в историю
      if not FUseAlternateBuffer then
      begin
        FScrollback.Add(FLines[0]);
        if FScrollback.Count > FMaxScrollback then
          FScrollback.Delete(0);
      end;

      // Удаляем верхнюю строку (все остальные сдвигаются вверх)
      FLines.Delete(0);

      // КУРСОР: Так как все строки сдвинулись вверх (индекс уменьшился),
      // курсор тоже должен уменьшить свой Y.
      if FCursor.Y > 0 then
        Dec(FCursor.Y);
    end;
  end
  else if LinesDiff > 0 then
  begin
    // === ОКНО УВЕЛИЧИВАЕТСЯ (РАСШИРЕНИЕ) ===
    // Мы должны вернуть строки из истории НАВЕРХ экрана,
    // чтобы заполнить пустоту сверху, а не снизу.

    for I := 1 to LinesDiff do
    begin
      // Пытаемся достать строку из истории
      if (not FUseAlternateBuffer) and (FScrollback.Count > 0) then
      begin
        // Берем последнюю строку из истории
        LineToMove := FScrollback[FScrollback.Count - 1];
        FScrollback.Delete(FScrollback.Count - 1);

        // Вставляем её В НАЧАЛО экрана (индекс 0)
        // Это сдвигает весь текущий текст ВНИЗ.
        FLines.Insert(0, LineToMove);

        // КУРСОР: Текст уехал вниз, курсор тоже должен поехать вниз.
        Inc(FCursor.Y);
      end
      else
      begin
        // Если истории нет (или это Alt-буфер), добавляем пустую строку В КОНЕЦ
        FLines.Add(CreateBlankLine);
      end;
    end;
  end;

  // На всякий случай подгоняем размер под точный NewHeight (страховка)
  while FLines.Count > NewHeight do FLines.Delete(FLines.Count - 1);
  while FLines.Count < NewHeight do FLines.Add(CreateBlankLine);

  // Обработка альтернативного буфера (там истории нет, просто ресайз)
  while FAlternateBuffer.Count < NewHeight do FAlternateBuffer.Add(CreateBlankLine);
  while FAlternateBuffer.Count > NewHeight do FAlternateBuffer.Delete(FAlternateBuffer.Count - 1);


  // 2. ИЗМЕНЕНИЕ ШИРИНЫ
  if NewWidth <> FWidth then
  begin
    // Основной буфер
    for I := 0 to FLines.Count - 1 do
    begin
      NewLine := FLines[I];
      if NewWidth > Length(NewLine) then
      begin
        var OldLen := Length(NewLine);
        SetLength(NewLine, NewWidth);
        for J := OldLen to NewWidth - 1 do
        begin
          NewLine[J].Char := ' ';
          NewLine[J].Attributes := TCharAttributes.Default(FTheme);
          NewLine[J].Width := 1;
        end;
      end
      else
        SetLength(NewLine, NewWidth);
      FLines[I] := NewLine;
    end;

    // Альтернативный буфер
    for I := 0 to FAlternateBuffer.Count - 1 do
    begin
      NewLine := FAlternateBuffer[I];
      if NewWidth > Length(NewLine) then
      begin
        var OldLen := Length(NewLine);
        SetLength(NewLine, NewWidth);
        for J := OldLen to NewWidth - 1 do
        begin
          NewLine[J].Char := ' ';
          NewLine[J].Attributes := TCharAttributes.Default(FTheme);
          NewLine[J].Width := 1;
        end;
      end
      else
        SetLength(NewLine, NewWidth);
      FAlternateBuffer[I] := NewLine;
    end;
  end;

  // 3. ФИНАЛИЗАЦИЯ
  FWidth := NewWidth;
  FHeight := NewHeight;

  // Важно: при ресайзе сбрасываем регион скроллинга на полный экран,
  // иначе `top` и `bottom` могут указывать за пределы массива.
  FScrollTop := 0;
  FScrollBottom := FHeight - 1;

  SetLength(FLinesDirty, FHeight);

  // Удерживаем курсор в границах
  FCursor.X := EnsureRange(FCursor.X, 0, FWidth - 1);
  FCursor.Y := EnsureRange(FCursor.Y, 0, FHeight - 1);

  SetAllDirty;
end;

procedure TTerminalBuffer.ProcessCommand(const Cmd: TAnsiCommand);
var
  N, M: Integer;
begin
  case Cmd.Command of
    apcPrintChar:
      WriteChar(Cmd.Char, Cmd.Attributes);
    
    apcCursorUp:
      begin
        N := 1;
        if Length(Cmd.Params) > 0 then N := Max(1, Cmd.Params[0]);
        MoveCursorRelative(0, -N);
      end;
    
    apcCursorDown:
      begin
        N := 1;
        if Length(Cmd.Params) > 0 then N := Max(1, Cmd.Params[0]);
        MoveCursorRelative(0, N);
      end;
    
    apcCursorForward:
      begin
        N := 1;
        if Length(Cmd.Params) > 0 then N := Max(1, Cmd.Params[0]);
        MoveCursorRelative(N, 0);
      end;
    
    apcCursorBack:
      begin
        N := 1;
        if Length(Cmd.Params) > 0 then N := Max(1, Cmd.Params[0]);
        MoveCursorRelative(-N, 0);
      end;
    
    apcCursorNextLine:
      begin
        N := 1;
        if Length(Cmd.Params) > 0 then N := Max(1, Cmd.Params[0]);
        FCursor.X := 0;
        MoveCursorRelative(0, N);
      end;
    
    apcCursorPrevLine:
      begin
        N := 1;
        if Length(Cmd.Params) > 0 then N := Max(1, Cmd.Params[0]);
        FCursor.X := 0;
        MoveCursorRelative(0, -N);
      end;
    
    apcCursorHorizontalAbs:
      begin
        N := 1;
        if Length(Cmd.Params) > 0 then N := Cmd.Params[0];
        FCursor.X := EnsureRange(N - 1, 0, FWidth - 1);
      end;
    
    apcCursorPosition:
      begin
        N := 1; M := 1;
        if Length(Cmd.Params) > 0 then N := Cmd.Params[0];
        if Length(Cmd.Params) > 1 then M := Cmd.Params[1];
        MoveCursor(M - 1, N - 1);
      end;
    
    apcVerticalPositionAbs:
      begin
        N := 1;
        if Length(Cmd.Params) > 0 then N := Cmd.Params[0];
        FCursor.Y := EnsureRange(N - 1, 0, FHeight - 1);
      end;
    
    apcEraseDisplay:
      begin
        N := 0;
        if Length(Cmd.Params) > 0 then N := Cmd.Params[0];
        case N of
          0: begin
               ClearLine(FCursor.Y, 0);
               for M := FCursor.Y + 1 to FHeight - 1 do
                 ClearLine(M, 2);
             end;
          1: begin
               for M := 0 to FCursor.Y - 1 do
                 ClearLine(M, 2);
               ClearLine(FCursor.Y, 1);
             end;
          2, 3: Clear;
        end;
      end;
    
    apcEraseLine:
      begin
        N := 0;
        if Length(Cmd.Params) > 0 then N := Cmd.Params[0];
        ClearLine(FCursor.Y, N);
      end;
    
    apcEraseChar:
      begin
        N := 1;
        if Length(Cmd.Params) > 0 then N := Max(1, Cmd.Params[0]);
        EraseChar(FCursor.X, FCursor.Y, N);
      end;
    
    apcScrollUp:
      begin
        N := 1;
        if Length(Cmd.Params) > 0 then N := Max(1, Cmd.Params[0]);
        InternalScrollUp(FScrollTop, FScrollBottom, N);
      end;
    
    apcScrollDown:
      begin
        N := 1;
        if Length(Cmd.Params) > 0 then N := Max(1, Cmd.Params[0]);
        InternalScrollDown(FScrollTop, FScrollBottom, N);
      end;
    
    apcInsertLine:
      begin
        N := 1;
        if Length(Cmd.Params) > 0 then N := Max(1, Cmd.Params[0]);
        InsertLine(FCursor.Y, N);
      end;
    
    apcDeleteLine:
      begin
        N := 1;
        if Length(Cmd.Params) > 0 then N := Max(1, Cmd.Params[0]);
        DeleteLine(FCursor.Y, N);
      end;
    
    apcInsertChar:
      begin
        N := 1;
        if Length(Cmd.Params) > 0 then N := Max(1, Cmd.Params[0]);
        InsertChar(FCursor.X, FCursor.Y, N);
      end;
    
    apcDeleteChar:
      begin
        N := 1;
        if Length(Cmd.Params) > 0 then N := Max(1, Cmd.Params[0]);
        DeleteChar(FCursor.X, FCursor.Y, N);
      end;
    
    apcRepeatChar:
      begin
        N := 1;
        if Length(Cmd.Params) > 0 then N := Max(1, Cmd.Params[0]);
        for M := 1 to N do
          WriteChar(FLastChar, Cmd.Attributes);
      end;
    
    apcSetScrollingRegion:
      begin
        N := 1; M := FHeight;
        if Length(Cmd.Params) > 0 then N := Max(1, Cmd.Params[0]);
        if Length(Cmd.Params) > 1 then M := Min(Cmd.Params[1], FHeight);
        FScrollTop := N - 1;
        FScrollBottom := M - 1;
        MoveCursor(0, 0);
      end;
    
    apcSoftTerminalReset:
      begin
        FScrollTop := 0;
        FScrollBottom := FHeight - 1;
        FCurrentAttributes.Reset(FTheme);
        FCursor.X := 0;
        FCursor.Y := 0;
        FCursor.Visible := True;
        FAppCursorKeys := False;
        FMouseModes := [];
        FBracketedPaste := False;
      end;
    
    apcSaveCursorPosition:
      FSavedCursor := FCursor;
    
    apcRestoreCursorPosition:
      FCursor := FSavedCursor;
    
    apcSetPrivateMode:
      begin
        if Length(Cmd.Params) > 0 then
        begin
          N := Cmd.Params[0];
          case N of
            1: FAppCursorKeys := True;
            25: FCursor.Visible := True;
            1000: Include(FMouseModes, mtm1000_Click);
            1002: Include(FMouseModes, mtm1002_Wheel);
            1003: Include(FMouseModes, mtm1003_Any);
            1006: Include(FMouseModes, mtm1006_SGR);
            1049, 47, 1047: SwitchToAlternateBuffer;
            2004: FBracketedPaste := True;
          end;
        end;
      end;
    
    apcResetPrivateMode:
      begin
        if Length(Cmd.Params) > 0 then
        begin
          N := Cmd.Params[0];
          case N of
            1: FAppCursorKeys := False;
            25: FCursor.Visible := False;
            1000: Exclude(FMouseModes, mtm1000_Click);
            1002: Exclude(FMouseModes, mtm1002_Wheel);
            1003: Exclude(FMouseModes, mtm1003_Any);
            1006: Exclude(FMouseModes, mtm1006_SGR);
            1049, 47, 1047: SwitchToMainBuffer;
            2004: FBracketedPaste := False;
          end;
        end;
      end;
    
    apcReverseIndex:
      begin
        if FCursor.Y = FScrollTop then
          ScrollDown(1)
        else if FCursor.Y > 0 then
          Dec(FCursor.Y);
      end;
    
    apcSetGraphicsMode:
      FCurrentAttributes := Cmd.Attributes;
  end;
end;

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
  if (FSelStart.Y > FSelEnd.Y) or
    ((FSelStart.Y = FSelEnd.Y) and (FSelStart.X > FSelEnd.X)) then
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
  if not FHasSelection then
    Exit(False);
  AbsY := ScreenYToAbsolute(ScreenY);
  if (AbsY > FSelStart.Y) and (AbsY < FSelEnd.Y) then
    Exit(True);
  if (AbsY = FSelStart.Y) and (AbsY = FSelEnd.Y) then
    Exit((X >= FSelStart.X) and (X <= FSelEnd.X));
  if AbsY = FSelStart.Y then
    Exit(X >= FSelStart.X);
  if AbsY = FSelEnd.Y then
    Exit(X <= FSelEnd.X);
  Result := False;
end;

function TTerminalBuffer.GetSelectedText: string;
var
  Y, X, StartX, EndX, AbsY, SBCount: Integer;
  Line: TTerminalLine;
  ResultStr: TStringBuilder;
  
  function GetLineByAbsIndex(Idx: Integer): TTerminalLine;
  begin
    if FUseAlternateBuffer then
    begin
      if (Idx >= 0) and (Idx < FAlternateBuffer.Count) then
        Result := FAlternateBuffer[Idx]
      else
        Result := nil;
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
  if not FHasSelection then
    Exit('');
    
  ResultStr := TStringBuilder.Create;
  try
    for AbsY := FSelStart.Y to FSelEnd.Y do
    begin
      Line := GetLineByAbsIndex(AbsY);
      if Line = nil then
        Continue;
      if AbsY = FSelStart.Y then
        StartX := FSelStart.X
      else
        StartX := 0;
      if AbsY = FSelEnd.Y then
        EndX := FSelEnd.X
      else
        EndX := Length(Line) - 1;
      if EndX >= Length(Line) then
        EndX := Length(Line) - 1;
      for X := StartX to EndX do
      begin
        // Пропускаем "хвосты" wide-символов
        if (Line[X].Width > 0) then
          ResultStr.Append(Line[X].Char);
      end;
      if AbsY < FSelEnd.Y then
        ResultStr.Append(sLineBreak);
    end;
    Result := ResultStr.ToString;
  finally
    ResultStr.Free;
  end;
end;

end.
