program TerminalDemo;

uses
  System.StartUpCopy,
  FMX.Forms,
  FMX.Skia,
  MainForm in 'MainForm.pas' {FormMain},
  Terminal.AnsiParser in '..\..\Terminal.AnsiParser.pas',
  Terminal.Buffer in '..\..\Terminal.Buffer.pas',
  Terminal.Control in '..\..\Terminal.Control.pas',
  Terminal.Renderer in '..\..\Terminal.Renderer.pas',
  Terminal.Theme in '..\..\Terminal.Theme.pas',
  Terminal.Types in '..\..\Terminal.Types.pas';

{$R *.res}

begin
  GlobalUseSkia := True;
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
