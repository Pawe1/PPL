program ParallelFor;

uses
  Vcl.Forms,
  For1 in 'For1.pas' {frmFor};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmFor, frmFor);
  Application.Run;
end.
