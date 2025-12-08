unit Terminal.Control;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  System.Math, System.Generics.Collections,
  FMX.Types, FMX.Controls, FMX.Graphics, FMX.Dialogs,
  FMX.Skia, Skia, FMX.Consts, FMX.Platform,
  Terminal.Types, Terminal.Buffer, Terminal.AnsiParser, Terminal.Renderer,
  Terminal.Theme;

type
  TTerminalDataEvent = procedure(const S: string) of object;

  TMouseButtonState = (mbsDown, mbsUp, mbsMove);

  TSyntaxRule = record
    Keyword: string;
    AnsiColor: string;
    IgnoreCase: Boolean;
  end;

  TTerminalControl = class(TSkPaintBox)
  private
    FBuffer: TTerminalBuffer;
    FParser: TAnsiParser;
    FRenderer: TTerminalRenderer;
    FCursorTimer: TTimer;
    FOnData: TTerminalDataEvent;
    FTheme: TTerminalTheme;

    FRenderTimer: TTimer;
    FNeedRedraw: Boolean;

    FSyntaxRules: TList<TSyntaxRule>;
    FEnableSyntaxHighlighting: Boolean;

    // Для выделения
    FIsSelecting: Boolean;
    FSelectionStartAbs: TPoint;
    FAutoCopySelection: Boolean;
    FPasteOnRightClick: Boolean;

    procedure CursorTimerProc(Sender: TObject);
    procedure RenderTimerProc(Sender: TObject);

    function GetCols: Integer;
    function GetRows: Integer;
    function GetFontSize: Single;
    procedure SetFontSize(const Value: Single);
    function GetFontFamily: string;
    procedure SetFontFamily(const Value: string);
    function GetTheme: TTerminalTheme;
    procedure SetTheme(const Value: TTerminalTheme);

    function TranslateKey(Key: Word; KeyChar: WideChar; Shift: TShiftState): string;

    procedure SendMouseReport(AButton, ACol, ARow: Integer; AShift: TShiftState;
      AState: TMouseButtonState);

    function ApplyHighlighting(const Input: string): string;

    // Буфер обмена
    procedure CopyToClipboard;
    procedure PasteFromClipboard;

  protected
    procedure Draw(const Canvas: ISkCanvas; const Dest: TRectF; const Opacity: Single); override;
    procedure Resize; override;
    procedure KeyDown(var Key: Word; var KeyChar: WideChar; Shift: TShiftState); override;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
    procedure MouseWheel(Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean); override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure WriteText(const Text: string);
    procedure Clear;

    procedure AddSyntaxRule(const Keyword: string; const AnsiColor: string; IgnoreCase: Boolean = True);
    procedure ClearSyntaxRules;

    property Buffer: TTerminalBuffer read FBuffer;
    property Parser: TAnsiParser read FParser;
    property Renderer: TTerminalRenderer read FRenderer;
    property OnData: TTerminalDataEvent read FOnData write FOnData;
    property Cols: Integer read GetCols;
    property Rows: Integer read GetRows;

    property EnableSyntaxHighlighting: Boolean read FEnableSyntaxHighlighting write FEnableSyntaxHighlighting;
    property AutoCopySelection: Boolean read FAutoCopySelection write FAutoCopySelection;
    property PasteOnRightClick: Boolean read FPasteOnRightClick write FPasteOnRightClick;

  published
    property FontSize: Single read GetFontSize write SetFontSize;
    property FontFamily: string read GetFontFamily write SetFontFamily;
    property Theme: TTerminalTheme read GetTheme write SetTheme;
  end;

procedure Register;

implementation

uses
  System.Rtti;

procedure Register;
begin
  RegisterComponents('Terminal', [TTerminalControl]);
end;

{ TTerminalControl }

constructor TTerminalControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FTheme := TTerminalTheme.Create;

  FBuffer := TTerminalBuffer.Create(80, 24, FTheme);
  FParser := TAnsiParser.Create(FTheme);
  FRenderer := TTerminalRenderer.Create(FBuffer, FTheme);

  FCursorTimer := TTimer.Create(Self);
  FCursorTimer.Interval := 500;
  FCursorTimer.OnTimer := CursorTimerProc;
  FCursorTimer.Enabled := True;

  FRenderTimer := TTimer.Create(Self);
  FRenderTimer.Interval := 16;
  FRenderTimer.OnTimer := RenderTimerProc;
  FRenderTimer.Enabled := True;
  FNeedRedraw := True;

  FSyntaxRules := TList<TSyntaxRule>.Create;
  FEnableSyntaxHighlighting := False;

  FIsSelecting := False;
  FAutoCopySelection := True;
  FPasteOnRightClick := True;

  TabStop := False;
  CanFocus := True;
  HitTest := True;

  DrawCacheKind := TSkDrawCacheKind.Raster;
end;

destructor TTerminalControl.Destroy;
begin
  FSyntaxRules.Free;
  FRenderTimer.Free;
  FTheme.Free;
  FCursorTimer.Free;
  FRenderer.Free;
  FParser.Free;
  FBuffer.Free;
  inherited;
end;

function TTerminalControl.GetTheme: TTerminalTheme;
begin
  Result := FTheme;
end;

procedure TTerminalControl.SetTheme(const Value: TTerminalTheme);
begin
  FTheme.Assign(Value);
  FParser.SetTheme(FTheme);
  FRenderer.SetTheme(FTheme);
  FBuffer.SetTheme(FTheme);
  FNeedRedraw := True;
  redraw;
end;

function TTerminalControl.GetCols: Integer;
begin
  Result := FBuffer.Width;
end;

function TTerminalControl.GetRows: Integer;
begin
  Result := FBuffer.Height;
end;

function TTerminalControl.GetFontSize: Single;
begin
  Result := FRenderer.FontSize;
end;

procedure TTerminalControl.SetFontSize(const Value: Single);
begin
  if FRenderer.FontSize <> Value then
  begin
    FRenderer.FontSize := Value;
    FRenderer.MeasureChar;
    FNeedRedraw := True;
  end;
end;

function TTerminalControl.GetFontFamily: string;
begin
  Result := FRenderer.FontFamily;
end;

procedure TTerminalControl.SetFontFamily(const Value: string);
begin
  if FRenderer.FontFamily <> Value then
  begin
    FRenderer.FontFamily := Value;
    FRenderer.MeasureChar;
    FNeedRedraw := True;
  end;
end;

procedure TTerminalControl.CursorTimerProc(Sender: TObject);
begin
  FRenderer.ToggleCursorBlink;
  FNeedRedraw := True;
end;

procedure TTerminalControl.RenderTimerProc(Sender: TObject);
begin
  if FNeedRedraw then
  begin
    FNeedRedraw := False;
    Redraw;
  end;
end;

procedure TTerminalControl.Draw(const Canvas: ISkCanvas; const Dest: TRectF; const Opacity: Single);
begin
  inherited;
  FRenderer.Render(Canvas, Dest);
end;

procedure TTerminalControl.Resize;
var
  NewCols, NewRows: Integer;
begin
  inherited;

  FRenderer.MeasureChar;

  if (FRenderer.CharWidth = 0) or (FRenderer.CharHeight = 0) then Exit;

  NewCols := Trunc(Width / FRenderer.CharWidth);
  NewRows := Trunc(Height / FRenderer.CharHeight);

  if (NewCols > 0) and (NewRows > 0) then
  begin
    if (NewCols <> FBuffer.Width) or (NewRows <> FBuffer.Height) then
    begin
      FBuffer.Resize(NewCols, NewRows);
      FNeedRedraw := True;
    end;
  end;

  Redraw;
  FNeedRedraw := False;
end;

procedure TTerminalControl.AddSyntaxRule(const Keyword, AnsiColor: string; IgnoreCase: Boolean);
var
  Rule: TSyntaxRule;
begin
  Rule.Keyword := Keyword;
  Rule.AnsiColor := AnsiColor;
  Rule.IgnoreCase := IgnoreCase;
  FSyntaxRules.Add(Rule);
end;

procedure TTerminalControl.ClearSyntaxRules;
begin
  FSyntaxRules.Clear;
end;

function TTerminalControl.ApplyHighlighting(const Input: string): string;
var
  I: Integer;
  Rule: TSyntaxRule;
  Flags: TReplaceFlags;
  Replacement: string;
begin
  Result := Input;

  for I := 0 to FSyntaxRules.Count - 1 do
  begin
    Rule := FSyntaxRules[I];
    if Rule.Keyword = '' then Continue;

    Flags := [rfReplaceAll];
    if Rule.IgnoreCase then Include(Flags, rfIgnoreCase);

    Replacement := Rule.AnsiColor + Rule.Keyword + #27'[0m';

    Result := StringReplace(Result, Rule.Keyword, Replacement, Flags);
  end;
end;

procedure TTerminalControl.WriteText(const Text: string);
var
  Commands: TArray<TAnsiCommand>;
  I: Integer;
  ProcessedText: string;
begin
  if FEnableSyntaxHighlighting and (FSyntaxRules.Count > 0) and (not FBuffer.IsAlternateBuffer) then
    ProcessedText := ApplyHighlighting(Text)
  else
    ProcessedText := Text;

  if FParser.Parse(ProcessedText, Commands) then
  begin
    for I := 0 to High(Commands) do
      FBuffer.ProcessCommand(Commands[I]);

    FNeedRedraw := True;
  end;
end;

procedure TTerminalControl.Clear;
begin
  FBuffer.Clear;
  FNeedRedraw := True;
end;

function TTerminalControl.TranslateKey(Key: Word; KeyChar: WideChar;
  Shift: TShiftState): string;
begin
  Result := '';

  // Ctrl + буква -> Control code
  if (ssCtrl in Shift) and (Key >= ord('A')) and (Key <= ord('Z')) then
  begin
    Result := string(Char(Key - ord('A') + 1));
    Exit;
  end;

  // Alt + буква -> Escape + буква
  if (ssAlt in Shift) and (KeyChar <> #0) then
  begin
    Result := #27 + string(KeyChar);
    Exit;
  end;

  case Key of
    vkReturn: Result := #13;
    vkBack: Result := #127;
    vkTab: Result := #9;
    vkEscape: Result := #27;

    vkUp:
      if FBuffer.AppCursorKeys then Result := #27 + 'OA' else Result := #27 + '[A';
    vkDown:
      if FBuffer.AppCursorKeys then Result := #27 + 'OB' else Result := #27 + '[B';
    vkRight:
      if FBuffer.AppCursorKeys then Result := #27 + 'OC' else Result := #27 + '[C';
    vkLeft:
      if FBuffer.AppCursorKeys then Result := #27 + 'OD' else Result := #27 + '[D';

    vkHome: Result := #27 + '[H';
    vkEnd: Result := #27 + '[F';
    vkInsert: Result := #27 + '[2~';
    vkDelete: Result := #27 + '[3~';
    vkPrior: Result := #27 + '[5~';
    vkNext: Result := #27 + '[6~';

    vkF1: Result := #27 + 'OP';
    vkF2: Result := #27 + 'OQ';
    vkF3: Result := #27 + 'OR';
    vkF4: Result := #27 + 'OS';
    vkF5: Result := #27 + '[15~';
    vkF6: Result := #27 + '[17~';
    vkF7: Result := #27 + '[18~';
    vkF8: Result := #27 + '[19~';
    vkF9: Result := #27 + '[20~';
    vkF10: Result := #27 + '[21~';
    vkF11: Result := #27 + '[23~';
    vkF12: Result := #27 + '[24~';

  else
    if (KeyChar <> #0) and (Ord(KeyChar) >= 32) then
    begin
      Result := string(KeyChar);
    end;
  end;
end;

procedure TTerminalControl.CopyToClipboard;
var
  ClipboardService: IFMXClipboardService;
  Text: string;
begin
  if not FBuffer.HasSelection then Exit;

  if TPlatformServices.Current.SupportsPlatformService(IFMXClipboardService, ClipboardService) then
  begin
    Text := FBuffer.GetSelectedText;
    if Text <> '' then
      ClipboardService.SetClipboard(Text);
  end;
end;

procedure TTerminalControl.PasteFromClipboard;
var
  ClipboardService: IFMXClipboardService;
  Value: TValue;
  Text: string;
begin
  if FBuffer.HasSelection then
  begin
    FBuffer.ClearSelection;
    FNeedRedraw := True;
  end;

  if TPlatformServices.Current.SupportsPlatformService(IFMXClipboardService, ClipboardService) then
  begin
    Value := ClipboardService.GetClipboard;
    if not Value.IsEmpty then
    begin
      Text := Value.ToString;
      if (Text <> '') and Assigned(FOnData) then
      begin
        // Bracketed Paste Mode
        if FBuffer.BracketedPaste then
          Text := #27'[200~' + Text + #27'[201~';
        FOnData(Text);
      end;
    end;
  end;
end;

procedure TTerminalControl.KeyDown(var Key: Word;
  var KeyChar: WideChar; Shift: TShiftState);
var
  S: string;
begin
  // Обработка Copy/Paste
  // Ctrl + Shift + C или Ctrl + Insert -> Копировать
  if ((ssCtrl in Shift) and (ssShift in Shift) and (Key = vkC)) or
     ((ssCtrl in Shift) and (Key = vkInsert)) then
  begin
    CopyToClipboard;
    Key := 0;
    KeyChar := #0;
    Exit;
  end;

  // Ctrl + Shift + V или Shift + Insert -> Вставить
  if ((ssCtrl in Shift) and (ssShift in Shift) and (Key = vkV)) or
     ((ssShift in Shift) and (Key = vkInsert)) then
  begin
    PasteFromClipboard;
    Key := 0;
    KeyChar := #0;
    Exit;
  end;

  // Сбрасываем viewport при любом вводе
  FBuffer.ResetViewport;

  S := TranslateKey(Key, KeyChar, Shift);
  if (S <> '') and Assigned(FOnData) then
  begin
    FOnData(S);
    Key := 0;
    KeyChar := #0;
    FNeedRedraw := True;
  end;
end;

procedure TTerminalControl.SendMouseReport(AButton, ACol, ARow: Integer;
  AShift: TShiftState; AState: TMouseButtonState);
var
  S: string;
  Cb, Cx, Cy, ShiftMod: Integer;
begin
  S := '';
  ShiftMod := 0;
  if ssShift in AShift then Inc(ShiftMod, 4);
  if ssAlt in AShift then Inc(ShiftMod, 8);
  if ssCtrl in AShift then Inc(ShiftMod, 16);

  if mtm1006_SGR in FBuffer.MouseModes then
  begin
    Cb := AButton + ShiftMod;
    case AState of
      mbsDown:
        S := Format(#27'[<%d;%d;%dM', [Cb, ACol, ARow]);
      mbsUp:
        S := Format(#27'[<%d;%d;%dm', [Cb, ACol, ARow]);
      mbsMove:
        if mtm1003_Any in FBuffer.MouseModes then
          S := Format(#27'[<%d;%d;%dM', [Cb, ACol, ARow]);
    end;
  end
  else if (mtm1000_Click in FBuffer.MouseModes) or
     (mtm1002_Wheel in FBuffer.MouseModes) or
     (mtm1003_Any in FBuffer.MouseModes) then
  begin
    if (AButton = 64) and (mtm1002_Wheel in FBuffer.MouseModes) then
      Cb := 64
    else if (AButton = 65) and (mtm1002_Wheel in FBuffer.MouseModes) then
      Cb := 65
    else if (AState = mbsMove) and (mtm1003_Any in FBuffer.MouseModes) then
      Cb := AButton + 32
    else if AState = mbsUp then
      Cb := 3
    else if AState = mbsDown then
      Cb := AButton
    else
      Exit;

    Cb := Cb + ShiftMod;
    Cx := Min(Max(1, ACol), 255 - 32) + 32;
    Cy := Min(Max(1, ARow), 255 - 32) + 32;

    S := #27'[' + 'M' + Char(Cb + 32) + Char(Cx) + Char(Cy);
  end;

  if (S <> '') and Assigned(FOnData) then
  begin
    FOnData(S);
  end;
end;

procedure TTerminalControl.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Single);
var
  Col, Row, Cb, AbsY: Integer;
  IsMouseReporting: Boolean;
  OverrideSelection: Boolean;
begin
  SetFocus;
  if (FRenderer.CharWidth = 0) or (FRenderer.CharHeight = 0) then Exit;

  Col := Trunc(X / FRenderer.CharWidth);
  Row := Trunc(Y / FRenderer.CharHeight);

  var RepCol := Col + 1;
  var RepRow := Row + 1;

  IsMouseReporting := FBuffer.MouseModes <> [];
  OverrideSelection := (ssShift in Shift);

  // Логика выделения и вставки
  if (not IsMouseReporting) or OverrideSelection then
  begin
    if Button = TMouseButton.mbLeft then
    begin
      AbsY := FBuffer.ScreenYToAbsolute(Row);
      FSelectionStartAbs := TPoint.Create(Col, AbsY);
      FBuffer.SetSelection(Col, AbsY, Col, AbsY);
      FIsSelecting := True;
      FNeedRedraw := True;
    end
    else if (Button = TMouseButton.mbRight) and FPasteOnRightClick then
    begin
      PasteFromClipboard;
    end;
    Exit;
  end;

  case Button of
    TMouseButton.mbLeft: Cb := 0;
    TMouseButton.mbMiddle: Cb := 1;
    TMouseButton.mbRight: Cb := 2;
  else
    Exit;
  end;

  SendMouseReport(Cb, RepCol, RepRow, Shift, mbsDown);
  FBuffer.LastMouseCol := RepCol;
  FBuffer.LastMouseRow := RepRow;
end;

procedure TTerminalControl.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Single);
var
  Col, Row, Cb: Integer;
  IsMouseReporting: Boolean;
  OverrideSelection: Boolean;
begin
  // Завершение выделения
  if FIsSelecting then
  begin
    FIsSelecting := False;
    if FAutoCopySelection and FBuffer.HasSelection then
      CopyToClipboard;
    Exit;
  end;

  IsMouseReporting := FBuffer.MouseModes <> [];
  OverrideSelection := (ssShift in Shift);

  if OverrideSelection then Exit;

  if FBuffer.MouseModes = [] then
    Exit;

  if not (mtm1006_SGR in FBuffer.MouseModes) and
     not (mtm1003_Any in FBuffer.MouseModes) then
       Exit;

  if (FRenderer.CharWidth = 0) or (FRenderer.CharHeight = 0) then Exit;

  Col := Trunc(X / FRenderer.CharWidth) + 1;
  Row := Trunc(Y / FRenderer.CharHeight) + 1;

  case Button of
    TMouseButton.mbLeft: Cb := 0;
    TMouseButton.mbMiddle: Cb := 1;
    TMouseButton.mbRight: Cb := 2;
  else
    Exit;
  end;

  SendMouseReport(Cb, Col, Row, Shift, mbsUp);
  FBuffer.LastMouseCol := Col;
  FBuffer.LastMouseRow := Row;
end;

procedure TTerminalControl.MouseMove(Shift: TShiftState; X, Y: Single);
var
  Col, Row, Cb, AbsY: Integer;
  IsMouseReporting: Boolean;
  OverrideSelection: Boolean;
begin
  if (FRenderer.CharWidth = 0) or (FRenderer.CharHeight = 0) then Exit;

  Col := Trunc(X / FRenderer.CharWidth);
  Row := Trunc(Y / FRenderer.CharHeight);

  // Обновление выделения
  if FIsSelecting then
  begin
    Col := Max(0, Min(Col, FBuffer.Width - 1));
    Row := Max(0, Min(Row, FBuffer.Height - 1));

    AbsY := FBuffer.ScreenYToAbsolute(Row);

    FBuffer.SetSelection(FSelectionStartAbs.X, FSelectionStartAbs.Y, Col, AbsY);
    FNeedRedraw := True;
    Exit;
  end;

  IsMouseReporting := FBuffer.MouseModes <> [];
  OverrideSelection := (ssShift in Shift);

  if OverrideSelection then
  begin
     Cursor := crIBeam;
     Exit;
  end;

  var RepCol := Col + 1;
  var RepRow := Row + 1;

  if not ((mtm1003_Any in FBuffer.MouseModes) or
          ((mtm1006_SGR in FBuffer.MouseModes) and (Shift * [ssLeft, ssRight, ssMiddle] <> []))) then
  begin
    if FBuffer.MouseModes <> [] then
      Cursor := crHandPoint
    else
      Cursor := crIBeam;
    Exit;
  end;

  Cursor := crHandPoint;

  if (RepCol = FBuffer.LastMouseCol) and (RepRow = FBuffer.LastMouseRow) then
    Exit;

  FBuffer.LastMouseCol := RepCol;
  FBuffer.LastMouseRow := RepRow;

  if ssLeft in Shift then
    Cb := 0
  else if ssMiddle in Shift then
    Cb := 1
  else if ssRight in Shift then
    Cb := 2
  else
    Cb := 3;

  SendMouseReport(Cb, RepCol, RepRow, Shift, mbsMove);
end;

procedure TTerminalControl.MouseWheel(Shift: TShiftState; WheelDelta: Integer;
  var Handled: Boolean);
var
  Col, Row, Cb: Integer;
  LocalPos: TPointF;
  MouseService: IFMXMouseService;
  MousePos: TPointF;
  I: Integer;
  LinesToScroll: Integer;
begin
  LinesToScroll := 3;

  // Случай 1: Мышь НЕ отслеживается
  if not (mtm1002_Wheel in FBuffer.MouseModes) and
     not (mtm1006_SGR in FBuffer.MouseModes) then
  begin
    if FBuffer.IsAlternateBuffer then
    begin
      if Assigned(FOnData) then
      begin
        if FBuffer.HasSelection then
        begin
          FBuffer.ClearSelection;
          FNeedRedraw := True;
        end;

        for I := 1 to LinesToScroll do
        begin
          if WheelDelta > 0 then
            FOnData(#27'[A')
          else
            FOnData(#27'[B');
        end;
      end;
    end
    else
    begin
      if WheelDelta > 0 then
         FBuffer.ScrollViewport(LinesToScroll)
      else
         FBuffer.ScrollViewport(-LinesToScroll);

      FNeedRedraw := True;
    end;

    Handled := True;
    Exit;
  end;

  // Случай 2: Мышь отслеживается
  if (FRenderer.CharWidth = 0) or (FRenderer.CharHeight = 0) then Exit;

  if not TPlatformServices.Current.SupportsPlatformService(IFMXMouseService, MouseService) then
  begin
     Handled := False;
     Exit;
  end;

  MousePos := MouseService.GetMousePos;
  LocalPos := AbsoluteToLocal(MousePos);

  Col := Trunc(LocalPos.X / FRenderer.CharWidth) + 1;
  Row := Trunc(LocalPos.Y / FRenderer.CharHeight) + 1;

  if WheelDelta > 0 then
    Cb := 64
  else
    Cb := 65;

  SendMouseReport(Cb, Col, Row, Shift, mbsDown);
  Handled := True;
end;

end.
