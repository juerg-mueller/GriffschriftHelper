program GriffschriftHelper;

uses
  Vcl.Forms,
  UGriffHelper in 'UGriffHelper.pas' {Form1},
  UXmlNode in 'UXmlNode.pas',
  UXmlParser in 'UXmlParser.pas',
  UMyMidiStream in 'UMyMidiStream.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
