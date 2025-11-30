unit MainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics,
  FMX.StdCtrls, FMX.Layouts, FMX.Edit, FMX.Memo,
  Terminal.Control, System.Skia, FMX.Memo.Types, FMX.ScrollBox, FMX.Skia,
  FMX.Controls.Presentation, ScSSHClient, ScBridge, ScSSHChannel, FMX.ListBox,Terminal.Types,
  Terminal.Theme, ScUtils, FMX.Objects, FMX.Dialogs;

type
  TFormMain = class(TForm)
    LayoutTop: TLayout;
    ButtonTest1: TButton;
    ButtonTest2: TButton;
    ButtonTest3: TButton;
    btDisconnect: TButton;
    LayoutBottom: TLayout;
    EditInput: TEdit;
    ButtonSend: TButton;
    Terminal: TTerminalControl;
    MemoCommands: TMemo;
    Splitter1: TSplitter;
    ButtonTest4: TButton;
    ButtonTest5: TButton;
    sshShell: TScSSHShell;
    ScSSHClient1: TScSSHClient;
    SFS: TScFileStorage;
    edHostName: TEdit;
    edUser: TEdit;
    Layout1: TLayout;
    Samples: TLabel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    edPassword: TEdit;
    btConnect: TButton;
    Layout2: TLayout;
    Label5: TLabel;
    btClear: TButton;
    Rectangle1: TRectangle;
    odTheme: TOpenDialog;
    btLoadTheme: TButton;
    btThemeDefault: TButton;
    procedure Button1Click(Sender: TObject);
    procedure btConnectClick(Sender: TObject);
    procedure ButtonTest1Click(Sender: TObject);
    procedure ButtonTest2Click(Sender: TObject);
    procedure ButtonTest3Click(Sender: TObject);
    procedure btDisconnectClick(Sender: TObject);
    procedure ButtonSendClick(Sender: TObject);
    procedure EditInputKeyDown(Sender: TObject; var Key: Word;
      var KeyChar: WideChar; Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
    procedure ButtonTest4Click(Sender: TObject);
    procedure ButtonTest5Click(Sender: TObject);
    procedure sshShellAsyncReceive(Sender: TObject);
    procedure TerminalResized(Sender: TObject);
    procedure sshShellConnect(Sender: TObject);
    procedure ScSSHClient1AfterDisconnect(Sender: TObject);
    procedure ScSSHClient1ServerKeyValidate(Sender: TObject; NewServerKey: TScKey;
        var Accept: Boolean);
    procedure btClearClick(Sender: TObject);
    procedure btLoadThemeClick(Sender: TObject);
  private
    procedure LoadExampleCommands;
    procedure TerminalDataHandler(const S: string);
    procedure KeyDown(var Key: Word; var KeyChar: System.WideChar; Shift: TShiftState); override;
  public
  end;

var
  FormMain: TFormMain;

implementation

{$R *.fmx}

  function EscapeString(const Input: string): string;
  var
    I: Integer;
    Ch: Char;
  begin
    Result := '';
    for I := 1 to Length(Input) do
    begin
      Ch := Input[I];
      if Ord(Ch) < 32 then
        Result := Result + '#' + IntToStr(Ord(Ch))
      else
        Result := Result + Ch;
    end;
  end;

procedure TFormMain.Button1Click(Sender: TObject);
var
  LTheme: TTerminalTheme;
begin
end;

procedure TFormMain.btClearClick(Sender: TObject);
begin
MemoCommands.Lines.Clear;
end;

procedure TFormMain.btConnectClick(Sender: TObject);
begin
  ScSSHClient1.HostName:=edHostName.Text;
  ScSSHClient1.User:=edUser.Text;
  ScSSHClient1.Password:=edPassword.Text;

  sshShell.TerminalInfo.Cols := Terminal.Cols;
  sshShell.TerminalInfo.Rows := Terminal.Rows;
  ScSSHClient1.Connect;
  sshShell.Connect;

  Terminal.Clear;
  Terminal.SetFocus;
end;

procedure TFormMain.ButtonTest4Click(Sender: TObject);
begin
  Terminal.WriteText('=== RGB Color Test ==='#13#10);
  Terminal.WriteText(#27'[38;2;255;100;50mOrange RGB'#27'[0m'#13#10);
  Terminal.WriteText(#27'[38;2;255;0;255mMagenta RGB'#27'[0m'#13#10);
  Terminal.WriteText(#27'[48;2;0;128;128mDark Cyan BG'#27'[0m'#13#10);
  Terminal.WriteText
    (#27'[38;2;255;215;0m'#27'[48;2;128;0;128mGold on Purple'#27'[0m'#13#10);
  Terminal.WriteText(#13#10);
end;

procedure TFormMain.ButtonTest5Click(Sender: TObject);
var
  I: Integer;
begin
  Terminal.WriteText('=== Complex Test ==='#13#10);
  Terminal.WriteText('Progress: '#27'[42m          '#27'[0m 100%'#13#10);
  Terminal.WriteText(#27'[1m+--------+--------+--------+'#27'[0m'#13#10);
  Terminal.WriteText
    (#27'[1m| '#27'[31mRed    '#27'[37m| '#27'[32mGreen  '#27'[37m| '#27'[34mBlue   '#27'[37m|'#27'[0m'#13#10);
  Terminal.WriteText(#27'[1m+--------+--------+--------+'#27'[0m'#13#10);
  Terminal.WriteText('Gradient: ');
  for I := 0 to 10 do
    Terminal.WriteText(#27'[48;5;' + IntToStr(16 + I * 20) + 'm '#27'[0m');
  Terminal.WriteText(#13#10#13#10);
end;

procedure TFormMain.ButtonTest1Click(Sender: TObject);
begin
  Terminal.WriteText('=== Color Test ==='#13#10);
  Terminal.WriteText
    (#27'[31mRed '#27'[32mGreen '#27'[33mYellow '#27'[34mBlue'#27'[0m'#13#10);
  Terminal.WriteText
    (#27'[35mMagenta '#27'[36mCyan '#27'[37mWhite'#27'[0m'#13#10);
  Terminal.WriteText(#27'[91mBright Red '#27'[92mBright Green'#27'[0m'#13#10);
  Terminal.WriteText(#13#10);
end;

procedure TFormMain.ButtonTest2Click(Sender: TObject);
begin
  Terminal.WriteText('=== Style Test ==='#13#10);
  Terminal.WriteText(#27'[1mBold '#27'[22mNormal'#27'[0m'#13#10);
  Terminal.WriteText(#27'[3mItalic '#27'[23mNormal'#27'[0m'#13#10);
  Terminal.WriteText(#27'[4mUnderline '#27'[24mNormal'#27'[0m'#13#10);
  Terminal.WriteText(#27'[1;3;4mBold Italic Underline'#27'[0m'#13#10);
  Terminal.WriteText(#13#10);
end;

procedure TFormMain.ButtonTest3Click(Sender: TObject);
begin
  Terminal.WriteText('=== Background Test ==='#13#10);
  Terminal.WriteText
    (#27'[40mBlack BG '#27'[41mRed BG '#27'[42mGreen BG'#27'[0m'#13#10);
  Terminal.WriteText(#27'[43mYellow BG '#27'[44mBlue BG'#27'[0m'#13#10);
  Terminal.WriteText(#27'[31;43m'#27'[1mRed on Yellow'#27'[0m'#13#10);
  Terminal.WriteText(#13#10);
end;

procedure TFormMain.btDisconnectClick(Sender: TObject);
begin
  ScSSHClient1.Disconnect;
  Terminal.Clear;
end;

procedure TFormMain.btLoadThemeClick(Sender: TObject);
var
  LTheme: TTerminalTheme;
begin
  if not odTheme.Execute then exit;

  LTheme := TTerminalTheme.Create;
  Ltheme.LoadThemeFromFile(odTheme.FileName);
  try
    Terminal.Theme := LTheme;
  finally
    LTheme.Free;
  end;
  Terminal.SetFocus;
end;

procedure TFormMain.ButtonSendClick(Sender: TObject);
var
  Text: string;
begin
  sshShell.TerminalInfo.Cols := Terminal.Cols;
  sshShell.TerminalInfo.Rows := Terminal.Rows;
  sshShell.Resize;
  Text := EditInput.Text;
  if Text <> '' then
  begin
    Text := StringReplace(Text, '#27', #27, [rfReplaceAll]);
    sshShell.WriteString(Text + #13#10);
    EditInput.Text := '';
    EditInput.SetFocus;
  end;
end;

procedure TFormMain.EditInputKeyDown(Sender: TObject; var Key: Word;
  var KeyChar: WideChar; Shift: TShiftState);
begin
  if Key = vkReturn then
  begin
    ButtonSendClick(Sender);
    Key := 0;
  end;
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  LoadExampleCommands;
  Terminal.WriteText('Terminal ready.'#13#10);
  Terminal.WriteText('Type commands or click test buttons.'#13#10#13#10);

  Terminal.OnData := TerminalDataHandler;
  Terminal.SetFocus;

  // --- *** ВКЛЮЧАЕМ ПОДСВЕТКУ (Client-Side) *** ---
  Terminal.EnableSyntaxHighlighting := True;

  // 1. Правила для логов (Ошибки красным, предупреждения желтым)
  Terminal.AddSyntaxRule('Error', #27'[1;31m');   // Жирный Красный
  Terminal.AddSyntaxRule('Fail', #27'[1;31m');
  Terminal.AddSyntaxRule('Warning', #27'[1;33m'); // Жирный Желтый
  Terminal.AddSyntaxRule('Success', #27'[1;32m'); // Жирный Зеленый
  Terminal.AddSyntaxRule('OK', #27'[1;32m');

  // 2. Правила для SQL (пример)
  Terminal.AddSyntaxRule('SELECT', #27'[1;36m');  // Жирный Циан
  Terminal.AddSyntaxRule('FROM', #27'[1;36m');
  Terminal.AddSyntaxRule('WHERE', #27'[1;36m');

  // 3. Правила для кода (пример)
  Terminal.AddSyntaxRule('function', #27'[35m');  // Пурпурный
  Terminal.AddSyntaxRule('begin', #27'[1m');      // Жирный белый
  Terminal.AddSyntaxRule('end', #27'[1m');
end;

procedure TFormMain.KeyDown(var Key: Word; var KeyChar: System.WideChar; Shift: TShiftState);
begin
if (Key = vkTab) and Assigned(Focused) and (Focused is TTerminalControl) then
begin
 sshShell.WriteString(#9);
end else
  inherited;
end;

procedure TFormMain.LoadExampleCommands;
begin
  MemoCommands.Lines.Clear;
  MemoCommands.Lines.Add('// Примеры ANSI команд:');
  MemoCommands.Lines.Add('');
  MemoCommands.Lines.Add('// Цветной текст');
  MemoCommands.Lines.Add('#27[31mRed text#27[0m');
  MemoCommands.Lines.Add('#27[32mGreen text#27[0m');
  MemoCommands.Lines.Add('#27[33mYellow text#27[0m');
  MemoCommands.Lines.Add('');
  MemoCommands.Lines.Add('// Стили');
  MemoCommands.Lines.Add('#27[1mBold#27[0m');
  MemoCommands.Lines.Add('#27[3mItalic#27[0m');
  MemoCommands.Lines.Add('#27[4mUnderline#27[0m');
  MemoCommands.Lines.Add('');
  MemoCommands.Lines.Add('// Комбинации');
  MemoCommands.Lines.Add('#27[1;4;31mBold Underline Red#27[0m');
  MemoCommands.Lines.Add('#27[7mInverse#27[0m');
  MemoCommands.Lines.Add('');
  MemoCommands.Lines.Add('// Фон');
  MemoCommands.Lines.Add('#27[43mYellow background#27[0m');
  MemoCommands.Lines.Add('#27[31;44mRed on Blue#27[0m');
  MemoCommands.Lines.Add('');
  MemoCommands.Lines.Add('// RGB цвета');
  MemoCommands.Lines.Add('#27[38;2;255;100;50mOrange RGB#27[0m');
  MemoCommands.Lines.Add('#27[48;2;128;0;128mPurple BG#27[0m');
end;

// procedure TFormMain.Panel1KeyDown(Sender: TObject; var Key: Word; var KeyChar: WideChar; Shift: TShiftState);
// begin
//   EditInput.Text:=KeyChar;
// end;

procedure TFormMain.ScSSHClient1AfterDisconnect(Sender: TObject);
begin
  Terminal.Clear;
end;

procedure TFormMain.ScSSHClient1ServerKeyValidate(Sender: TObject;
    NewServerKey: TScKey; var Accept: Boolean);
var
  Key: TScKey;
  fp, msg: string;
  CurHostKeyName: string;
begin
  if ScSSHClient1.HostKeyName = '' then
    CurHostKeyName := ScSSHClient1.HostName
  else
    CurHostKeyName := ScSSHClient1.HostKeyName;
  Key := SFS.Keys.FindKey(CurHostKeyName);
  if (Key = nil) or not Key.Ready then begin
    NewServerKey.GetFingerPrint(haMD5, fp);
    msg := 'The authenticity of server can not be verified.'#13#10 +
           'Fingerprint for the key received from server: ' + fp + '.'#13#10 +
           'Key length: ' + IntToStr(NewServerKey.BitCount) + ' bits.'#13#10 +
           'Are you sure you want to continue connecting?';

    if MessageDlg(msg, TMsgDlgType.mtConfirmation, mbOkCancel, 0) = mrOk then begin
      Key := TScKey.Create(nil);
      try
        Key.Assign(NewServerKey);
        Key.KeyName := CurHostKeyName;
        SFS.Keys.Add(Key);
      except
        Key.Free;
        raise;
      end;

      Accept := True;
    end;
  end;
end;

procedure TFormMain.sshShellAsyncReceive(Sender: TObject);
var
  s: string;
begin
  s := sshShell.Readstring;
  MemoCommands.Lines.Add('SSH: '+EscapeString(s));
  Terminal.WriteText(s);
end;

procedure TFormMain.sshShellConnect(Sender: TObject);
begin
  Terminal.SetFocus;
end;

procedure TFormMain.TerminalDataHandler(const S: string);
begin
  if not sshShell.Connected then exit;
  MemoCommands.Lines.Add('Term: '+EscapeString(s));
  sshShell.WriteString(S);
end;

procedure TFormMain.TerminalResized(Sender: TObject);
begin
  sshShell.TerminalInfo.Cols := Terminal.Cols;
  sshShell.TerminalInfo.Rows := Terminal.Rows;
  sshShell.Resize;
end;

end.
