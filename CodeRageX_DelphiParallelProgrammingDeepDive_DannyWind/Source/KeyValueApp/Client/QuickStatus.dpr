program QuickStatus;

uses
  System.StartUpCopy,
  FMX.Forms,
  UnitGlobalStatus in '..\Shared\UnitGlobalStatus.pas',
  UnitFormMain in 'UnitFormMain.pas' {FormMain},
  ClientClassesUnit1 in 'ClientClassesUnit1.pas',
  ClientModuleUnit1 in 'ClientModuleUnit1.pas' {ClientModule1: TDataModule},
  UnitDataMain in 'UnitDataMain.pas' {DataModuleMain: TDataModule};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TClientModule1, ClientModule1);
  Application.CreateForm(TDataModuleMain, DataModuleMain);
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
