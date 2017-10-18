program Future;

uses
  System.StartUpCopy,
  FMX.Forms,
  Future1 in 'Future1.pas' {frmFuture};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmFuture, frmFuture);
  Application.Run;
end.
