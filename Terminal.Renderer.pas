unit Terminal.Renderer;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Math,
  FMX.Types, FMX.Graphics, FMX.TextLayout,
  FMX.Skia, Skia, System.Skia,
  Terminal.Types, Terminal.Buffer, System.UIConsts,
  Terminal.Theme;

type
  TTerminalRenderer = class
  private
    FBuffer: TTerminalBuffer;
    FCharWidth: Single;
    FCharHeight: Single;
    FAscentOffset: Single;
    FFontFamily: string;
    FFontSize: Single;
    FShowCursor: Boolean;
    FCursorBlinkState: Boolean;
    FTheme: TTerminalTheme;

    FCachedPaint: ISkPaint;
    FCachedFontNormal: ISkFont;
    FCachedFontBold: ISkFont;
    FCachedFontItalic: ISkFont;
    FCachedFontBoldItalic: ISkFont;
    FResourcesValid: Boolean;

    FBackBuffer: ISkSurface;
    FBackBufferWidth: Integer;
    FBackBufferHeight: Integer;

    function GetEffectiveForeground(const Attr: TCharAttributes): TAlphaColor;
    procedure UpdateResources;
    function GetFontForStyle(Bold, Italic: Boolean): ISkFont;
    procedure CheckBackBuffer(Width, Height: Integer);
    function GetCursorRect: TRectF;
  public
    constructor Create(ABuffer: TTerminalBuffer; ATheme: TTerminalTheme);

    procedure Render(Canvas: ISkCanvas; const Bounds: TRectF);
    procedure RenderLine(Canvas: ISkCanvas; LineIndex: Integer; const Bounds: TRectF; OffsetY: Single; DefaultBG: TAlphaColor);
    procedure RenderCursor(Canvas: ISkCanvas; const Bounds: TRectF);

    procedure MeasureChar;
    procedure ToggleCursorBlink;
    procedure RenderDebugInfo(Canvas: ISkCanvas; const Bounds: TRectF);
    procedure SetTheme(ATheme: TTerminalTheme);
    procedure InvalidateResources;

    property CharWidth: Single read FCharWidth;
    property CharHeight: Single read FCharHeight;
    property FontFamily: string read FFontFamily write FFontFamily;
    property FontSize: Single read FFontSize write FFontSize;
    property ShowCursor: Boolean read FShowCursor write FShowCursor;
  end;

implementation


{ TTerminalRenderer }

constructor TTerminalRenderer.Create(ABuffer: TTerminalBuffer; ATheme: TTerminalTheme);
begin
  inherited Create;
  FBuffer := ABuffer;
  FTheme := ATheme;

  {$IFDEF LINUX}
    FFontFamily := 'Monospace';
  {$ELSEIF DEFINED(MACOS)}
    FFontFamily := 'Menlo';
  {$ELSE}
    FFontFamily := 'Consolas';
  {$ENDIF}

  FFontSize := 13;
  FShowCursor := True;
  FCursorBlinkState := True;
  FAscentOffset := 0;
  FResourcesValid := False;

  FCachedPaint := TSkPaint.Create;
  FBackBuffer := nil;
  FBackBufferWidth := 0;
  FBackBufferHeight := 0;

  MeasureChar;
end;

procedure TTerminalRenderer.SetTheme(ATheme: TTerminalTheme);
begin
  FTheme := ATheme;
  FBuffer.SetAllDirty;
end;

procedure TTerminalRenderer.InvalidateResources;
begin
  FResourcesValid := False;
end;

procedure TTerminalRenderer.UpdateResources;
var
  Typeface: ISkTypeface;
begin
  if FResourcesValid then Exit;

  Typeface := TSkTypeface.MakeFromName(FFontFamily, TSkFontStyle.Normal);
  FCachedFontNormal := TSkFont.Create(Typeface, FFontSize);

  Typeface := TSkTypeface.MakeFromName(FFontFamily, TSkFontStyle.Bold);
  FCachedFontBold := TSkFont.Create(Typeface, FFontSize);

  Typeface := TSkTypeface.MakeFromName(FFontFamily, TSkFontStyle.Italic);
  FCachedFontItalic := TSkFont.Create(Typeface, FFontSize);

  Typeface := TSkTypeface.MakeFromName(FFontFamily, TSkFontStyle.BoldItalic);
  FCachedFontBoldItalic := TSkFont.Create(Typeface, FFontSize);

  FResourcesValid := True;
end;

function TTerminalRenderer.GetFontForStyle(Bold, Italic: Boolean): ISkFont;
begin
  if Bold and Italic then
    Result := FCachedFontBoldItalic
  else if Bold then
    Result := FCachedFontBold
  else if Italic then
    Result := FCachedFontItalic
  else
    Result := FCachedFontNormal;
end;

procedure TTerminalRenderer.MeasureChar;
var
  Metrics: TSkFontMetrics;
begin
  InvalidateResources;
  UpdateResources;

  FCachedFontNormal.GetMetrics(Metrics);
  FCharWidth := FCachedFontNormal.MeasureText('W');
  if FCharWidth < 1 then FCharWidth := 8;
  FCharHeight := Abs(Metrics.Ascent) + Metrics.Descent;
  if FCharHeight < 1 then FCharHeight := 12;
  FAscentOffset := Abs(Metrics.Ascent);
end;

function TTerminalRenderer.GetEffectiveForeground(const Attr: TCharAttributes): TAlphaColor;
begin
  if Attr.Inverse then
    Result := Attr.BackgroundColor
  else
    Result := Attr.ForegroundColor;

  if Attr.Faint then
    Result := MakeColor(Result, 0.6);
end;

procedure TTerminalRenderer.CheckBackBuffer(Width, Height: Integer);
begin
  if (FBackBuffer = nil) or (FBackBufferWidth <> Width) or (FBackBufferHeight <> Height) then
  begin
    FBackBufferWidth := Width;
    FBackBufferHeight := Height;
    FBackBuffer := TSkSurface.MakeRaster(FBackBufferWidth, FBackBufferHeight);

    FBuffer.GetAndResetVisualScrollDelta;
    FBuffer.SetAllDirty;

    if FBackBuffer <> nil then
      FBackBuffer.Canvas.Clear(FTheme.DefaultBG);
  end;
end;

procedure TTerminalRenderer.RenderLine(Canvas: ISkCanvas; LineIndex: Integer;
  const Bounds: TRectF; OffsetY: Single; DefaultBG: TAlphaColor);
var
  Line: TTerminalLine;
  Width, I, J, RunStart, RunLen: Integer;
  Y, RunX: Single;
  RunAttr: TCharAttributes;
  RunText: string;
  CurrentFont: ISkFont;
  BgColor, FgColor: TAlphaColor;
  RunSelected: Boolean;

  function AttrsEqual(const A, B: TCharAttributes): Boolean; inline;
  begin
    Result := (A.ForegroundColor = B.ForegroundColor) and
              (A.BackgroundColor = B.BackgroundColor) and
              (A.Bold = B.Bold) and
              (A.Italic = B.Italic) and
              (A.Underline = B.Underline) and
              (A.Inverse = B.Inverse);
  end;

begin
  if (LineIndex < 0) or (LineIndex >= FBuffer.Height) then Exit;

  // Берем линию с учетом скролла истории
  Line := FBuffer.GetRenderLine(LineIndex);

  Y := Bounds.Top + OffsetY + (LineIndex * FCharHeight);

  // Очищаем фон всей линии
  FCachedPaint.Style := TSkPaintStyle.Fill;
  FCachedPaint.Color := DefaultBG;
  Canvas.DrawRect(TRectF.Create(Bounds.Left, Y, Bounds.Right, Y + FCharHeight), FCachedPaint);

  if Line = nil then Exit;

  Width := FBuffer.Width;
  I := 0;
  while I < Width do
  begin
    if I >= Length(Line) then Break;

    RunStart := I;
    RunAttr := Line[I].Attributes;

    // --- ПРОВЕРЯЕМ ВЫДЕЛЕНИЕ ---
    RunSelected := FBuffer.IsCellSelected(I, LineIndex); // LineIndex - это экранный Y (0..Height-1)

    Inc(I);
    // Ищем конец группы символов (Run)
    while (I < Width) and (I < Length(Line)) do
    begin
      // 1. Атрибуты должны совпадать
      if not AttrsEqual(Line[I].Attributes, RunAttr) then Break;
      // 2. Статус выделения должен совпадать (иначе разрываем группу)
      if FBuffer.IsCellSelected(I, LineIndex) <> RunSelected then Break;

      Inc(I);
    end;

    RunLen := I - RunStart;
    RunX := Bounds.Left + (RunStart * FCharWidth);

    // --- ОПРЕДЕЛЯЕМ ЦВЕТА С УЧЕТОМ ВЫДЕЛЕНИЯ ---
    if RunSelected then
    begin
      // Стиль выделения: Инверсия или фиксированный цвет (как в VS Code / Putty)
      // Используем светло-серый фон и черный текст для контраста
      BgColor := $FFCCCCCC;
      FgColor := $FF000000;
    end
    else
    begin
      // Обычные цвета
      if RunAttr.Inverse then
        BgColor := RunAttr.ForegroundColor
      else
        BgColor := RunAttr.BackgroundColor;

      FgColor := GetEffectiveForeground(RunAttr);
    end;

    // Рисуем фон сегмента
    if (BgColor <> DefaultBG) or RunSelected then
    begin
      FCachedPaint.Style := TSkPaintStyle.Fill;
      FCachedPaint.Color := BgColor;
      Canvas.DrawRect(
        TRectF.Create(RunX, Y, RunX + (RunLen * FCharWidth), Y + FCharHeight),
        FCachedPaint
      );
    end;

    if not RunAttr.Hidden then
    begin
      SetLength(RunText, RunLen);
      for J := 0 to RunLen - 1 do
        RunText[J+1] := Line[RunStart + J].Char;

      if (RunText.Trim <> '') or RunAttr.Underline or RunAttr.Strikethrough then
      begin
        CurrentFont := GetFontForStyle(RunAttr.Bold, RunAttr.Italic);

        FCachedPaint.Style := TSkPaintStyle.Fill;
        FCachedPaint.Color := FgColor;

        Canvas.DrawSimpleText(RunText, RunX, Y + FAscentOffset, CurrentFont, FCachedPaint);

        if RunAttr.Underline or RunAttr.Strikethrough then
        begin
          FCachedPaint.Style := TSkPaintStyle.Stroke;
          FCachedPaint.StrokeWidth := 1;
          FCachedPaint.Color := FgColor; // Линии тоже красятся цветом текста

          if RunAttr.Underline then
            Canvas.DrawLine(RunX, Y + FAscentOffset + 2, RunX + (RunLen * FCharWidth), Y + FAscentOffset + 2, FCachedPaint);

          if RunAttr.Strikethrough then
            Canvas.DrawLine(RunX, Y + FCharHeight / 2, RunX + (RunLen * FCharWidth), Y + FCharHeight / 2, FCachedPaint);
        end;
      end;
    end;
  end;
end;

function TTerminalRenderer.GetCursorRect: TRectF;
var
  X, Y: Single;
begin
  X := (FBuffer.Cursor.X * FCharWidth);
  Y := (FBuffer.Cursor.Y * FCharHeight);
  Result := TRectF.Create(X, Y, X + FCharWidth, Y + FCharHeight);
end;

procedure TTerminalRenderer.RenderCursor(Canvas: ISkCanvas; const Bounds: TRectF);
var
  CursorRect: TRectF;
begin
  if (FBuffer.ViewportOffset > 0) then Exit;

  if not FShowCursor or not FCursorBlinkState or not FBuffer.Cursor.Visible then
    Exit;

  CursorRect := GetCursorRect;
  CursorRect.Offset(Bounds.Left, Bounds.Top);

  FCachedPaint.Style := TSkPaintStyle.Stroke;
  FCachedPaint.Color := $FFFFFFFF;
  FCachedPaint.StrokeWidth := 1;

  Canvas.DrawRect(CursorRect, FCachedPaint);

  FCachedPaint.Style := TSkPaintStyle.Fill;
  FCachedPaint.Color := $55FFFFFF;
  Canvas.DrawRect(CursorRect, FCachedPaint);
end;

procedure TTerminalRenderer.RenderDebugInfo(Canvas: ISkCanvas; const Bounds: TRectF);
begin
end;

procedure TTerminalRenderer.Render(Canvas: ISkCanvas; const Bounds: TRectF);
var
  I: Integer;
  LDefaultBG: TAlphaColor;
  BackCanvas: ISkCanvas;
  W, H: Integer;
  ImageSnapshot: ISkImage;
  ScrollDelta: Integer;
  ScrollPx: Single;
begin
  UpdateResources;
  LDefaultBG := FTheme.DefaultBG;

  W := Ceil(Bounds.Width);
  H := Ceil(Bounds.Height);

  CheckBackBuffer(W, H);
  if FBackBuffer = nil then Exit;
  BackCanvas := FBackBuffer.Canvas;

  ScrollDelta := FBuffer.GetAndResetVisualScrollDelta;

  // --- Hardware Scroll (Сдвиг картинки) ---
  // Если есть выделение (HasSelection), то лучше перерисовать весь экран,
  // иначе сдвиг картинки может "размазать" выделение.
  // Поэтому добавляем проверку "not FBuffer.HasSelection"
  if (not FBuffer.HasSelection) and (FBuffer.ViewportOffset = 0) and (ScrollDelta > 0) and (ScrollDelta * FCharHeight < H) then
  begin
    ImageSnapshot := FBackBuffer.MakeImageSnapshot;
    if ImageSnapshot <> nil then
    begin
       ScrollPx := ScrollDelta * FCharHeight;
       BackCanvas.DrawImage(ImageSnapshot, 0, -ScrollPx);
    end;
  end
  else if (FBuffer.ViewportOffset = 0) and (ScrollDelta > 0) then
  begin
    BackCanvas.Clear(LDefaultBG);
  end;

  // --- Отрисовка "грязных" строк ---
  for I := 0 to FBuffer.Height - 1 do
  begin
    if FBuffer.IsLineDirty(I) then
    begin
      RenderLine(BackCanvas, I, TRectF.Create(0, 0, Bounds.Width, Bounds.Height), 0, LDefaultBG);
      FBuffer.CleanLine(I);
    end;
  end;

  // --- ФИНАЛЬНЫЙ ВЫВОД ---
  ImageSnapshot := FBackBuffer.MakeImageSnapshot;
  if ImageSnapshot <> nil then
    Canvas.DrawImage(ImageSnapshot, Bounds.Left, Bounds.Top);

  RenderCursor(Canvas, Bounds);
end;

procedure TTerminalRenderer.ToggleCursorBlink;
begin
  FCursorBlinkState := not FCursorBlinkState;
end;

end.
