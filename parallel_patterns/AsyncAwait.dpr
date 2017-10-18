program AsyncAwait;

uses
  Vcl.Forms,
  AsyncAwait1 in 'AsyncAwait1.pas' {frmAsyncAwait};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmAsyncAwait, frmAsyncAwait);
  Application.Run;
end.
