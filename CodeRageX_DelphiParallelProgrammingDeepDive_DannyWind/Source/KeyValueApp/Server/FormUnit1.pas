unit FormUnit1;

{===============================================================================
 Demo for using the parallel programming library in a FMX client of a REST based
 server with a Key/Value store.

 Author: Danny Wind
 License: Creative Commons CC-BY
===============================================================================}

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Edit, IdHTTPWebBrokerBridge, Web.HTTPApp, FMX.Controls.Presentation;

type
  TForm1 = class(TForm)
    ButtonStart: TButton;
    ButtonStop: TButton;
    EditPort: TEdit;
    Label1: TLabel;
    ButtonOpenBrowser: TButton;
    LabelLastUpdate: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure ButtonStartClick(Sender: TObject);
    procedure ButtonStopClick(Sender: TObject);
    procedure ButtonOpenBrowserClick(Sender: TObject);
  private
    FServer: TIdHTTPWebBrokerBridge;
    procedure StartServer;
    procedure ApplicationIdle(Sender: TObject; var Done: Boolean);
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses
  WinApi.Windows, Winapi.ShellApi, Datasnap.DSSession, System.SyncObjs,
  UnitGlobalStatus;

procedure TForm1.ApplicationIdle(Sender: TObject; var Done: Boolean);
var
  lDateTime: TDateTime;
begin
  ButtonStart.Enabled := not FServer.Active;
  ButtonStop.Enabled := FServer.Active;
  EditPort.Enabled := not FServer.Active;
  {Refresh last update from one of the clients in the user interface}
  lDateTime := TInterlocked.CompareExchange(Double(UnitGlobalStatus.ServerLastUpdate), 0.0, 0.0);
  LabelLastUpdate.Text := DateTimeToStr(lDateTime);
end;

procedure TForm1.ButtonOpenBrowserClick(Sender: TObject);
var
  LURL: string;
begin
  StartServer;
  LURL := Format('http://localhost:%s', [EditPort.Text]);
  ShellExecute(0,
        nil,
        PChar(LURL), nil, nil, SW_SHOWNOACTIVATE);
end;

procedure TForm1.ButtonStartClick(Sender: TObject);
begin
  StartServer;
end;

procedure TerminateThreads;
begin
  if TDSSessionManager.Instance <> nil then
    TDSSessionManager.Instance.TerminateAllSessions;
end;

procedure TForm1.ButtonStopClick(Sender: TObject);
begin
  TerminateThreads;
  FServer.Active := False;
  FServer.Bindings.Clear;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FServer := TIdHTTPWebBrokerBridge.Create(Self);
  Application.OnIdle := ApplicationIdle;
end;

procedure TForm1.StartServer;
begin
  if not FServer.Active then
  begin
    FServer.Bindings.Clear;
    FServer.DefaultPort := StrToInt(EditPort.Text);
    FServer.Active := True;
  end;
end;

end.
