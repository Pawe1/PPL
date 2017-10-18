program Map;

uses
  Vcl.Forms,
  Map1 in 'Map1.pas' {frmMap};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMap, frmMap);
  Application.Run;
end.
