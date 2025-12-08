unit Terminal.Renderer;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Math,
  FMX.Types, FMX.Graphics, FMX.TextLayout,
  FMX.Skia, System.Skia,
  Terminal.Types, Terminal.Buffer, System.UIConsts,
  Terminal.Theme;

type
  TTerminalRenderer = class
  private
    FBuffer: TTerminalBuffer;
    FCharWidth: Single;
    FCharHeight: Single;
    FScaleX: Single;
    FScaleY: Single;
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

    // Для гибридного рендеринга
    FTextLayout: TTextLayout;
    FEmojiBitmap: TBitmap;

    FResourcesValid: Boolean;
    FBackBuffer: ISkSurface;
    FBackBufferWidth: Integer;
    FBackBufferHeight: Integer;

    function GetEffectiveForeground(const Attr: TCharAttributes): TAlphaColor;
    procedure UpdateResources;
    function GetFontForStyle(Bold, Italic: Boolean): ISkFont;
    procedure CheckBackBuffer(Width, Height: Integer);
    function GetCursorRect: TRectF;

    procedure RenderBoxDrawingChar(Canvas: ISkCanvas; Ch: string;
      X, Y, W, H: Single; Color: TAlphaColor);
    procedure RenderComplexChar(Canvas: ISkCanvas; const Ch: string;
      X, Y, W: Single; Color: TAlphaColor);

  public
    constructor Create(ABuffer: TTerminalBuffer; ATheme: TTerminalTheme);
    destructor Destroy; override;

    procedure Render(Canvas: ISkCanvas; const Bounds: TRectF);
    procedure RenderLine(Canvas: ISkCanvas; LineIndex: Integer;
      const Bounds: TRectF; OffsetY: Single; DefaultBG: TAlphaColor);
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

constructor TTerminalRenderer.Create(ABuffer: TTerminalBuffer;
  ATheme: TTerminalTheme);
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
  FScaleX := 1.0;
  FScaleY := 1.0;
  FCachedPaint := TSkPaint.Create;
  FBackBuffer := nil;
  FBackBufferWidth := 0;
  FBackBufferHeight := 0;

  FTextLayout := TTextLayoutManager.DefaultTextLayout.Create;
  FEmojiBitmap := TBitmap.Create;

  MeasureChar;
end;

destructor TTerminalRenderer.Destroy;
begin
  FTextLayout.Free;
  FEmojiBitmap.Free;
  inherited;
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
  
  procedure ConfigureFont(Font: ISkFont);
  begin
    Font.Edging := TSkFontEdging.SubpixelAntiAlias;
    Font.Hinting := TSkFontHinting.Slight;
    Font.Subpixel := True;
  end;

begin
  if FResourcesValid then
    Exit;
  Typeface := TSkTypeface.MakeFromName(FFontFamily, TSkFontStyle.Normal);
  FCachedFontNormal := TSkFont.Create(Typeface, FFontSize);
  ConfigureFont(FCachedFontNormal);
  Typeface := TSkTypeface.MakeFromName(FFontFamily, TSkFontStyle.Bold);
  FCachedFontBold := TSkFont.Create(Typeface, FFontSize);
  ConfigureFont(FCachedFontBold);
  Typeface := TSkTypeface.MakeFromName(FFontFamily, TSkFontStyle.Italic);
  FCachedFontItalic := TSkFont.Create(Typeface, FFontSize);
  ConfigureFont(FCachedFontItalic);
  Typeface := TSkTypeface.MakeFromName(FFontFamily, TSkFontStyle.BoldItalic);
  FCachedFontBoldItalic := TSkFont.Create(Typeface, FFontSize);
  ConfigureFont(FCachedFontBoldItalic);
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
  RealWidth, RealHeight: Single;
begin
  InvalidateResources;
  UpdateResources;
  FCachedFontNormal.GetMetrics(Metrics);
  RealWidth := FCachedFontNormal.MeasureText('W');
  if RealWidth < 1 then
    RealWidth := 8;
  FCharWidth := Round(RealWidth);
  if FCharWidth < 1 then
    FCharWidth := 8;
  FScaleX := FCharWidth / RealWidth;
  RealHeight := Abs(Metrics.Ascent) + Metrics.Descent;
  FCharHeight := Round(RealHeight);
  if FCharHeight < 1 then
    FCharHeight := 12;
  if RealHeight > 0 then
    FScaleY := FCharHeight / RealHeight
  else
    FScaleY := 1.0;
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
  if (FBackBuffer = nil) or (FBackBufferWidth <> Width) or
    (FBackBufferHeight <> Height) then
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

procedure TTerminalRenderer.RenderBoxDrawingChar(Canvas: ISkCanvas; Ch: string;
  X, Y, W, H: Single; Color: TAlphaColor);
var
  Cx, Cy: Single;
  Code: Integer;
  DrawUp, DrawDown, DrawLeft, DrawRight: Boolean;
begin
  FCachedPaint.Style := TSkPaintStyle.Stroke;
  FCachedPaint.Color := Color;
  FCachedPaint.StrokeWidth := 1;
  Cx := Floor(X + (W / 2)) + 0.5;
  Cy := Floor(Y + (H / 2)) + 0.5;
  if Length(Ch) <> 1 then
    Exit;
  Code := Ord(Ch[1]);
  DrawUp := False;
  DrawDown := False;
  DrawLeft := False;
  DrawRight := False;
  
  case Code of
    $2500, $2501, $2504, $2505, $2508, $2509, $254C, $254D, $2550:
      begin
        DrawLeft := True;
        DrawRight := True;
      end;
    $2502, $2503, $2506, $2507, $250A, $250B, $2551:
      begin
        DrawUp := True;
        DrawDown := True;
      end;
    $250C..$250F, $2552..$2554:
      begin
        DrawDown := True;
        DrawRight := True;
      end;
    $2510..$2513, $2555..$2557:
      begin
        DrawDown := True;
        DrawLeft := True;
      end;
    $2514..$2517, $2558..$255A:
      begin
        DrawUp := True;
        DrawRight := True;
      end;
    $2518..$251B, $255B..$255D:
      begin
        DrawUp := True;
        DrawLeft := True;
      end;
    $251C..$2523, $255E..$2560:
      begin
        DrawUp := True;
        DrawDown := True;
        DrawRight := True;
      end;
    $2524..$252B, $2561..$2563:
      begin
        DrawUp := True;
        DrawDown := True;
        DrawLeft := True;
      end;
    $252C..$2533, $2564..$2566:
      begin
        DrawDown := True;
        DrawLeft := True;
        DrawRight := True;
      end;
    $2534..$253B, $2567..$2569:
      begin
        DrawUp := True;
        DrawLeft := True;
        DrawRight := True;
      end;
    $253C..$254B, $256A..$256C:
      begin
        DrawUp := True;
        DrawDown := True;
        DrawLeft := True;
        DrawRight := True;
      end;
  end;
  
  if DrawUp then
    Canvas.DrawLine(Cx, Cy, Cx, Y, FCachedPaint);
  if DrawDown then
    Canvas.DrawLine(Cx, Cy, Cx, Y + H, FCachedPaint);
  if DrawLeft then
    Canvas.DrawLine(Cx, Cy, X, Cy, FCachedPaint);
  if DrawRight then
    Canvas.DrawLine(Cx, Cy, X + W, Cy, FCachedPaint);
end;

procedure TTerminalRenderer.RenderComplexChar(Canvas: ISkCanvas;
  const Ch: string; X, Y, W: Single; Color: TAlphaColor);
var
  BmpW, BmpH: Integer;
  Image: ISkImage;
begin
  FTextLayout.Text := Ch;
  {$IFDEF MSWINDOWS}
  FTextLayout.Font.Family := 'Segoe UI Emoji';
  {$ELSE}
  FTextLayout.Font.Family := 'Apple Color Emoji';
  {$ENDIF}

  FTextLayout.Font.Size := FFontSize;
  FTextLayout.Color := Color;
  FTextLayout.WordWrap := False;
  FTextLayout.HorizontalAlign := TTextAlign.Leading;
  FTextLayout.VerticalAlign := TTextAlign.Leading;

  BmpW := Round(W);
  BmpH := Round(FCharHeight * 1.5);
  if (FEmojiBitmap.Width < BmpW) or (FEmojiBitmap.Height < BmpH) then
    FEmojiBitmap.SetSize(BmpW, BmpH);
  FEmojiBitmap.Clear(0);

  if FEmojiBitmap.Canvas.BeginScene then
    try
      FTextLayout.RenderLayout(FEmojiBitmap.Canvas);
    finally
      FEmojiBitmap.Canvas.EndScene;
    end;

  Image := FEmojiBitmap.ToSkImage;
  if Image <> nil then
    Canvas.DrawImage(Image, X, Y);
end;

procedure TTerminalRenderer.RenderLine(Canvas: ISkCanvas; LineIndex: Integer;
  const Bounds: TRectF; OffsetY: Single; DefaultBG: TAlphaColor);
var
  Line: TTerminalLine;
  Width, I, RunStart, RunLen: Integer;
  Y, RunX: Single;
  RunAttr: TCharAttributes;
  CurrentFont: ISkFont;
  BgColor, FgColor: TAlphaColor;
  RunSelected: Boolean;
  BgRect: TRectF;
  CharToDraw: string;
  CharIdx: Integer;
  CharX: Single;
  CharDisplayWidth: Integer;
  RenderWidth: Single;
begin
  if (LineIndex < 0) or (LineIndex >= FBuffer.Height) then
    Exit;
    
  Line := FBuffer.GetRenderLine(LineIndex);
  Y := Floor(Bounds.Top + OffsetY + (LineIndex * FCharHeight));
  
  // Очистка строки фоном
  FCachedPaint.Style := TSkPaintStyle.Fill;
  FCachedPaint.Color := DefaultBG;
  Canvas.DrawRect(TRectF.Create(Bounds.Left, Y, Bounds.Right, Y + FCharHeight), FCachedPaint);
  
  if Line = nil then
    Exit;
    
  Width := FBuffer.Width;
  I := 0;

  while I < Width do
  begin
    // Пропускаем "хвосты" wide-символов (Width = 0)
    if (I < Length(Line)) and (Line[I].Width = 0) then
    begin
      Inc(I);
      Continue;
    end;
    
    RunStart := I;
    if I < Length(Line) then
      RunAttr := Line[I].Attributes
    else
      RunAttr := TCharAttributes.Default(FTheme);
    RunSelected := FBuffer.IsCellSelected(I, LineIndex);
    Inc(I);
    
    // Собираем run символов с одинаковыми атрибутами
    while I < Width do
    begin
      // Пропускаем хвосты в подсчёте run
      if (I < Length(Line)) and (Line[I].Width = 0) then
      begin
        Inc(I);
        Continue;
      end;
      
      var CurrentAttr: TCharAttributes;
      if I < Length(Line) then
        CurrentAttr := Line[I].Attributes
      else
        CurrentAttr := TCharAttributes.Default(FTheme);
        
      if (CurrentAttr.ForegroundColor <> RunAttr.ForegroundColor) or
        (CurrentAttr.BackgroundColor <> RunAttr.BackgroundColor) or
        (CurrentAttr.Bold <> RunAttr.Bold) or
        (CurrentAttr.Italic <> RunAttr.Italic) or
        (CurrentAttr.Underline <> RunAttr.Underline) or
        (CurrentAttr.Inverse <> RunAttr.Inverse) then
        Break;
      if FBuffer.IsCellSelected(I, LineIndex) <> RunSelected then
        Break;
      Inc(I);
    end;
    
    RunLen := I - RunStart;
    RunX := Bounds.Left + (RunStart * FCharWidth);
    
    // Определяем цвета
    if RunSelected then
    begin
      BgColor := $FFCCCCCC;
      FgColor := $FF000000;
    end
    else
    begin
      if RunAttr.Inverse then
        BgColor := RunAttr.ForegroundColor
      else
        BgColor := RunAttr.BackgroundColor;
      FgColor := GetEffectiveForeground(RunAttr);
    end;
    
    // Рисуем фон если отличается от default
    if (BgColor <> DefaultBG) or RunSelected then
    begin
      FCachedPaint.Style := TSkPaintStyle.Fill;
      FCachedPaint.Color := BgColor;
      BgRect := TRectF.Create(RunX, Y, RunX + (RunLen * FCharWidth), Y + FCharHeight);
      BgRect.Inflate(0.5, 0.0);
      Canvas.DrawRect(BgRect, FCachedPaint);
    end;

    // Рисуем символы
    if not RunAttr.Hidden then
    begin
      CurrentFont := GetFontForStyle(RunAttr.Bold, RunAttr.Italic);
      CharIdx := RunStart;
      CharX := RunX;
      
      while CharIdx < I do
      begin
        if CharIdx >= Length(Line) then
          Break;
          
        // Пропускаем хвосты
        if Line[CharIdx].Width = 0 then
        begin
          Inc(CharIdx);
          Continue;
        end;
        
        CharToDraw := Line[CharIdx].Char;
        CharDisplayWidth := Line[CharIdx].Width;
        RenderWidth := CharDisplayWidth * FCharWidth;

        if IsBoxDrawingChar(CharToDraw) then
          RenderBoxDrawingChar(Canvas, CharToDraw, CharX, Y, FCharWidth, FCharHeight, FgColor)
        else if CharToDraw <> '' then
        begin
          // Гибридный рендер: сложные символы через FMX, простые через Skia
          if (Length(CharToDraw) > 1) or (Ord(CharToDraw[1]) > $2E80) then
            RenderComplexChar(Canvas, CharToDraw, CharX, Y, RenderWidth, FgColor)
          else
          begin
            Canvas.Save;
            try
              Canvas.Translate(CharX, Y + FAscentOffset);
              Canvas.Scale(FScaleX, FScaleY);
              Canvas.Translate(-CharX, -(Y + FAscentOffset));
              FCachedPaint.Style := TSkPaintStyle.Fill;
              FCachedPaint.Color := FgColor;
              Canvas.DrawSimpleText(CharToDraw, CharX, Y + FAscentOffset, CurrentFont, FCachedPaint);
            finally
              Canvas.Restore;
            end;
          end;

          // Underline и Strikethrough
          if RunAttr.Underline or RunAttr.Strikethrough then
          begin
            FCachedPaint.Style := TSkPaintStyle.Stroke;
            FCachedPaint.StrokeWidth := 1;
            FCachedPaint.Color := FgColor;
            if RunAttr.Underline then
              Canvas.DrawLine(CharX, Y + FAscentOffset + 2, CharX + RenderWidth, Y + FAscentOffset + 2, FCachedPaint);
            if RunAttr.Strikethrough then
              Canvas.DrawLine(CharX, Y + FCharHeight / 2, CharX + RenderWidth, Y + FCharHeight / 2, FCachedPaint);
          end;
        end;
        
        CharX := CharX + RenderWidth;
        Inc(CharIdx);
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
  if (FBuffer.ViewportOffset > 0) then
    Exit;
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
  // Debug info placeholder
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
  if FBackBuffer = nil then
    Exit;
  BackCanvas := FBackBuffer.Canvas;
  ScrollDelta := FBuffer.GetAndResetVisualScrollDelta;
  
  if (not FBuffer.HasSelection) and (FBuffer.ViewportOffset = 0) and
    (ScrollDelta > 0) and (ScrollDelta * FCharHeight < H) then
  begin
    ImageSnapshot := FBackBuffer.MakeImageSnapshot;
    if ImageSnapshot <> nil then
    begin
      ScrollPx := ScrollDelta * FCharHeight;
      BackCanvas.DrawImage(ImageSnapshot, 0, -ScrollPx);
    end;
  end
  else if (FBuffer.ViewportOffset = 0) and (ScrollDelta > 0) then
    BackCanvas.Clear(LDefaultBG);
    
  for I := 0 to FBuffer.Height - 1 do
    if FBuffer.IsLineDirty(I) then
    begin
      RenderLine(BackCanvas, I, TRectF.Create(0, 0, Bounds.Width, Bounds.Height), 0, LDefaultBG);
      FBuffer.CleanLine(I);
    end;
    
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
