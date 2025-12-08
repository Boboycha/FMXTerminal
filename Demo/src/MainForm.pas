unit MainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics,
  FMX.StdCtrls, FMX.Layouts, FMX.Edit, FMX.Memo,
  Terminal.Control, System.Skia, FMX.Memo.Types, FMX.ScrollBox, FMX.Skia,
  FMX.Controls.Presentation, ScSSHClient, ScBridge, ScSSHChannel, FMX.ListBox,
  Terminal.Types,
  Terminal.Theme, ScUtils, FMX.Objects, FMX.Dialogs,
  System.NetEncoding, ScSSHUtils, FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf,
  FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys, FireDAC.FMXUI.Wait, Data.DB, FireDAC.Comp.Client,
  FireDAC.Phys.SQLite, FireDAC.Phys.SQLiteDef, FireDAC.Stan.ExprFuncs, FireDAC.Phys.SQLiteWrapper.Stat, FireDAC.Stan.Param, FireDAC.DatS,
  FireDAC.DApt.Intf, FireDAC.DApt, FireDAC.Comp.DataSet, FMX.TabControl, FMX.ListView.Types, FMX.ListView.Appearances,
  FMX.ListView.Adapters.Base, Data.Bind.EngExt, Fmx.Bind.DBEngExt, System.Rtti, System.Bindings.Outputs, Fmx.Bind.Editors,
  Data.Bind.Components, Data.Bind.DBScope, FMX.ListView;

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
    Memo1: TMemo;
    Splitter2: TSplitter;
    Button1: TButton;
    SMS: TScMemoryStorage;
    StyleBook1: TStyleBook;
    db: TFDConnection;
    qKeys: TFDQuery;
    TabControl1: TTabControl;
    TabItem1: TTabItem;
    TabItem2: TTabItem;
    TabControl2: TTabControl;
    TabItem3: TTabItem;
    TabItem4: TTabItem;
    TabItem5: TTabItem;
    ListView1: TListView;
    qHosts: TFDQuery;
    BindingsList1: TBindingsList;
    BindSourceDB1: TBindSourceDB;
    LinkListControlToField1: TLinkListControlToField;
    Edit1: TEdit;
    LinkControlToField1: TLinkControlToField;
    qHostsid: TFDAutoIncField;
    qHostsname: TWideStringField;
    qHostsgroup_id: TLargeintField;
    qHostsuser: TWideStringField;
    qHostspassword: TWideStringField;
    qHostskey_id: TIntegerField;
    qHostsaddress: TWideStringField;
    Edit2: TEdit;
    LinkControlToField2: TLinkControlToField;
    ListBox1: TListBox;
    LinkListControlToField2: TLinkListControlToField;
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
    procedure ScSSHClient1ServerKeyValidate(Sender: TObject;
      NewServerKey: TScKey; var Accept: Boolean);
    procedure btClearClick(Sender: TObject);
    procedure btLoadThemeClick(Sender: TObject);
    procedure ButtonTestEmojiClick(Sender: TObject);
    procedure SMSCheckUserKey(Sender: TObject; ClientInfo: TScSSHClientInfo;
      Key: TScKey; var Accept: Boolean);
    procedure Button1Click(Sender: TObject);
  private
    FPendingData: TBytes;
    ButtonTestEmoji: TButton;

    procedure LoadExampleCommands;
    procedure TerminalDataHandler(const S: string);
    procedure KeyDown(var Key: Word; var KeyChar: System.WideChar;
      Shift: TShiftState); override;

    // Ручной потоковый декодер
    function ProcessUTF8Data(const NewData: TBytes): string;
  public
  end;

var
  FormMain: TFormMain;

implementation

{$R *.fmx}

uses System.Math;

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

procedure TFormMain.btClearClick(Sender: TObject);
begin
  MemoCommands.Lines.Clear;
end;

procedure TFormMain.btConnectClick(Sender: TObject);
begin
  // ScSSHClient1.HostName:=edHostName.Text;
  // ScSSHClient1.User:=edUser.Text;
  // ScSSHClient1.Password:=edPassword.Text;
  //
  // // Отключаем встроенное декодирование, читаем RAW байты
  // sshShell.UseUnicode := False;
  //
  // sshShell.TerminalInfo.Cols := Terminal.Cols;
  // sshShell.TerminalInfo.Rows := Terminal.Rows;
  if ScSSHClient1.Connected then exit;
  ScSSHClient1.Connect;
  sshShell.Connect;

  Terminal.Clear;
  Terminal.SetFocus;
  SetLength(FPendingData, 0);
end;

procedure TFormMain.ButtonTest1Click(Sender: TObject);
begin
  Terminal.WriteText(#27'[31mRed'#27'[0m'#13#10);
end;

procedure TFormMain.ButtonTest2Click(Sender: TObject);
begin
  Terminal.WriteText(#27'[1mBold'#27'[0m'#13#10);
end;

procedure TFormMain.ButtonTest3Click(Sender: TObject);
begin
  Terminal.WriteText(#27'[41mRedBG'#27'[0m'#13#10);
end;

procedure TFormMain.ButtonTest4Click(Sender: TObject);
begin
  Terminal.WriteText('RGB Test'#13#10);
end;

procedure TFormMain.ButtonTest5Click(Sender: TObject);
begin
  Terminal.WriteText('Complex Test'#13#10);
end;

procedure TFormMain.ButtonTestEmojiClick(Sender: TObject);
var
  S: string;
begin
  S := '';
  S := S + #27'[1;32m' + '--- DIRECT LOCAL TEST ---' + #27'[0m' + #13#10;
  S := S + 'Rocket: ' + #$D83D#$DE80 + #13#10;
  S := S + 'Fire:   ' + #$D83D#$DD25 + #13#10;
  S := S + 'Family: ' + #$D83D#$DC68 + #$200D + #$D83D#$DC69 + #$200D +
    #$D83D#$DC67 + #$200D + #$D83D#$DC66 + #13#10;
  Terminal.WriteText(S);
end;

procedure TFormMain.btDisconnectClick(Sender: TObject);
begin
  ScSSHClient1.Disconnect;
  Terminal.Clear;
  SetLength(FPendingData, 0);
end;

procedure TFormMain.btLoadThemeClick(Sender: TObject);
var
  LTheme: TTerminalTheme;
begin
  if not odTheme.Execute then
    exit;
  LTheme := TTerminalTheme.Create;
  LTheme.LoadThemeFromFile(odTheme.FileName);
  try
    Terminal.Theme := LTheme;
  finally
    LTheme.Free;
  end;
  Terminal.SetFocus;
  sshShell.TerminalInfo.Rows:=sshShell.TerminalInfo.Rows+1;
  sshShell.Resize;
  sshShell.TerminalInfo.Rows := sshShell.TerminalInfo.Rows-1;
  sshShell.Resize;

end;

procedure TFormMain.Button1Click(Sender: TObject);
var
  Key,StorageKey: TScKey;
  msg, fp, Comment, KeyName: string;
begin
  var
  ms := TMemoryStream.Create;
  Memo1.Lines.SaveToStream(ms);
  ms.Position := 0;
  Key := TScKey.Create(nil);
  try
    Key.ImportFrom(ms, '', Comment);
    Key.GetFingerPrint(haMD5, fp);
    msg := 'Fingerprint for the key imported from server: ' + fp + '.'#13#10 +
      'Key length: ' + IntToStr(Key.BitCount) + ' bits.'#13#10 +
      'Do you want to save the server key?';

    if MessageDlg(msg, TmsgDlgType.mtConfirmation,
      [tMsgDlgBtn.mbOK, tMsgDlgBtn.mbCancel], 0) = mrOk then
    begin
      if Trim(KeyName) = '' then
        KeyName := 'Rocky';

      StorageKey := SMS.Keys.FindKey(KeyName);
      if StorageKey <> nil then
        StorageKey.Assign(Key)
      else
      begin
        Key.KeyName := KeyName;
        SMS.Keys.Add(Key);
        Key := nil;
      end;
      ScSSHClient1.PrivateKeyName := KeyName;
    end;
  finally
    Key.Free;
  end;

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
    // Отправляем UTF-8 байты
    var
      Bytes: TBytes := TEncoding.UTF8.GetBytes(Text + #13#10);
    if Length(Bytes) > 0 then
      sshShell.WriteBuffer(Bytes[0], Length(Bytes));

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
  Terminal.OnData := TerminalDataHandler;
  Terminal.EnableSyntaxHighlighting := True;

  ButtonTestEmoji := TButton.Create(Self);
  ButtonTestEmoji.Parent := Rectangle1;
  ButtonTestEmoji.Position.X := 84;
  ButtonTestEmoji.Position.Y := 55;
  ButtonTestEmoji.Width := 80;
  ButtonTestEmoji.Height := 32;
  ButtonTestEmoji.Text := 'Emoji Test';
  ButtonTestEmoji.OnClick := ButtonTestEmojiClick;
end;

procedure TFormMain.KeyDown(var Key: Word; var KeyChar: System.WideChar;
  Shift: TShiftState);
begin
  if (Key = vkTab) and Assigned(Focused) and (Focused is TTerminalControl) then
  begin
    var
      TabByte: Byte := 9;
    sshShell.WriteBuffer(TabByte, 1);
  end
  else
    inherited;
end;

procedure TFormMain.LoadExampleCommands;
begin
  MemoCommands.Lines.Clear;
  MemoCommands.Lines.Add('// Examples...');
end;

procedure TFormMain.ScSSHClient1AfterDisconnect(Sender: TObject);
begin
  Terminal.Clear;
  SetLength(FPendingData, 0);
end;

procedure TFormMain.ScSSHClient1ServerKeyValidate(Sender: TObject;
  NewServerKey: TScKey; var Accept: Boolean);
begin
  Accept := True;
end;

procedure TFormMain.SMSCheckUserKey(Sender: TObject;
  ClientInfo: TScSSHClientInfo; Key: TScKey; var Accept: Boolean);
begin

end;

// --- РУЧНОЙ ПОТОКОВЫЙ ДЕКОДЕР UTF-8 ---
// Не падает при ошибках, корректно склеивает пакеты
function TFormMain.ProcessUTF8Data(const NewData: TBytes): string;
var
  TotalData: TBytes;
  I, Len, SeqLen: Integer;
  B: Byte;
  CodePoint: UInt32;

  // Вспомогательная проверка на байт продолжения (10xxxxxx)
  function IsContinuation(Idx: Integer): Boolean;
  begin
    Result := (Idx < Len) and ((TotalData[Idx] and $C0) = $80);
  end;

begin
  // 1. Склеиваем хвост и новые данные
  Len := Length(FPendingData) + Length(NewData);
  SetLength(TotalData, Len);
  if Length(FPendingData) > 0 then
    Move(FPendingData[0], TotalData[0], Length(FPendingData));
  if Length(NewData) > 0 then
    Move(NewData[0], TotalData[Length(FPendingData)], Length(NewData));

  SetLength(FPendingData, 0); // Очищаем хвост, будем набирать новый
  Result := '';

  I := 0;
  while I < Len do
  begin
    B := TotalData[I];

    // ASCII (0xxxxxxx) - 1 байт
    if B < 128 then
    begin
      Result := Result + Char(B);
      Inc(I);
      Continue;
    end;

    // Определяем длину последовательности по первому байту
    SeqLen := 0;
    if (B and $E0) = $C0 then
      SeqLen := 2 // 110xxxxx
    else if (B and $F0) = $E0 then
      SeqLen := 3 // 1110xxxx
    else if (B and $F8) = $F0 then
      SeqLen := 4; // 11110xxx

    // Если байт старта невалидный (10xxxxxx или > F4) -> заменяем на ? и идем дальше
    if SeqLen = 0 then
    begin
      Result := Result + '?';
      Inc(I);
      Continue;
    end;

    // Проверяем, хватает ли байтов в буфере
    if (I + SeqLen) > Len then
    begin
      // Байтов НЕ хватает. Это разрыв пакета.
      // Сохраняем весь остаток (от I до конца) в FPendingData и выходим.
      SetLength(FPendingData, Len - I);
      Move(TotalData[I], FPendingData[0], Len - I);
      Break;
    end;

    // Декодируем CodePoint
    CodePoint := 0;
    case SeqLen of
      2:
        begin
          if not IsContinuation(I + 1) then
          begin
            Result := Result + '?';
            Inc(I);
            Continue;
          end;
          CodePoint := ((B and $1F) shl 6) or (TotalData[I + 1] and $3F);
        end;
      3:
        begin
          if (not IsContinuation(I + 1)) or (not IsContinuation(I + 2)) then
          begin
            Result := Result + '?';
            Inc(I);
            Continue;
          end;
          CodePoint := ((B and $0F) shl 12) or
            ((TotalData[I + 1] and $3F) shl 6) or (TotalData[I + 2] and $3F);
        end;
      4:
        begin
          if (not IsContinuation(I + 1)) or (not IsContinuation(I + 2)) or
            (not IsContinuation(I + 3)) then
          begin
            Result := Result + '?';
            Inc(I);
            Continue;
          end;
          CodePoint := ((B and $07) shl 18) or
            ((TotalData[I + 1] and $3F) shl 12) or
            ((TotalData[I + 2] and $3F) shl 6) or (TotalData[I + 3] and $3F);
        end;
    end;

    // Конвертируем CodePoint в строку (UTF-16)
    if CodePoint <= $FFFF then
      Result := Result + Char(CodePoint)
    else
    begin
      // Суррогатная пара для Emoji (> U+FFFF)
      CodePoint := CodePoint - $10000;
      Result := Result + Char($D800 + (CodePoint shr 10));
      Result := Result + Char($DC00 + (CodePoint and $3FF));
    end;

    Inc(I, SeqLen);
  end;
end;

procedure TFormMain.sshShellAsyncReceive(Sender: TObject);
var
  S: string;
  InCount: Integer;
  Buffer: TBytes;
begin
  InCount := sshShell.InCount;
  if InCount > 0 then
  begin
    SetLength(Buffer, InCount);
    // Читаем в первый байт массива (Buffer[0])
    sshShell.ReadBuffer(Buffer[0], InCount);

    S := ProcessUTF8Data(Buffer);

    if S <> '' then
    begin
      MemoCommands.Lines.Add('SSH: ' + EscapeString(S));
      Terminal.WriteText(S);
    end;
  end;
end;

procedure TFormMain.sshShellConnect(Sender: TObject);
begin
  Terminal.SetFocus;
end;

procedure TFormMain.TerminalDataHandler(const S: string);
var
  Bytes: TBytes;
begin
  if not sshShell.Connected then
    exit;
  Bytes := TEncoding.UTF8.GetBytes(S);
  if Length(Bytes) > 0 then
    sshShell.WriteBuffer(Bytes[0], Length(Bytes));

  // MemoCommands.Lines.Add('Term: '+EscapeString(s));
end;

procedure TFormMain.TerminalResized(Sender: TObject);
begin
  sshShell.TerminalInfo.Cols := Terminal.Cols;
  sshShell.TerminalInfo.Rows := Terminal.Rows;
  sshShell.Resize;
end;

end.
