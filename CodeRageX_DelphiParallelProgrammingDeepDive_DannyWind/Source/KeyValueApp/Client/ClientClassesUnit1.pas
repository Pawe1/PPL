//
// Created by the DataSnap proxy generator.
// 10/10/2015 10:51:17 PM
//

unit ClientClassesUnit1;

interface

uses System.JSON, Datasnap.DSProxyRest, Datasnap.DSClientRest, Data.DBXCommon, Data.DBXClient, Data.DBXDataSnap, Data.DBXJSON, Datasnap.DSProxy, System.Classes, System.SysUtils, Data.DB, Data.SqlExpr, Data.DBXDBReaders, Data.DBXCDSReaders, Data.DBXJSONReflect;

type
  TServerMethods1Client = class(TDSAdminRestClient)
  private
    FEchoStringCommand: TDSRestCommand;
    FReverseStringCommand: TDSRestCommand;
    FSyncValueCommand: TDSRestCommand;
    FLastUpdateCommand: TDSRestCommand;
  public
    constructor Create(ARestConnection: TDSRestConnection); overload;
    constructor Create(ARestConnection: TDSRestConnection; AInstanceOwner: Boolean); overload;
    destructor Destroy; override;
    function EchoString(Value: string; const ARequestFilter: string = ''): string;
    function ReverseString(Value: string; const ARequestFilter: string = ''): string;
    procedure SyncValue(aKey: string; var aValue: string; var aDateTime: TDateTime; const ARequestFilter: string = '');
    function LastUpdate(const ARequestFilter: string = ''): TDateTime;
  end;

const
  TServerMethods1_EchoString: array [0..1] of TDSRestParameterMetaData =
  (
    (Name: 'Value'; Direction: 1; DBXType: 26; TypeName: 'string'),
    (Name: ''; Direction: 4; DBXType: 26; TypeName: 'string')
  );

  TServerMethods1_ReverseString: array [0..1] of TDSRestParameterMetaData =
  (
    (Name: 'Value'; Direction: 1; DBXType: 26; TypeName: 'string'),
    (Name: ''; Direction: 4; DBXType: 26; TypeName: 'string')
  );

  TServerMethods1_SyncValue: array [0..2] of TDSRestParameterMetaData =
  (
    (Name: 'aKey'; Direction: 1; DBXType: 26; TypeName: 'string'),
    (Name: 'aValue'; Direction: 3; DBXType: 26; TypeName: 'string'),
    (Name: 'aDateTime'; Direction: 3; DBXType: 11; TypeName: 'TDateTime')
  );

  TServerMethods1_LastUpdate: array [0..0] of TDSRestParameterMetaData =
  (
    (Name: ''; Direction: 4; DBXType: 11; TypeName: 'TDateTime')
  );

implementation

function TServerMethods1Client.EchoString(Value: string; const ARequestFilter: string): string;
begin
  if FEchoStringCommand = nil then
  begin
    FEchoStringCommand := FConnection.CreateCommand;
    FEchoStringCommand.RequestType := 'GET';
    FEchoStringCommand.Text := 'TServerMethods1.EchoString';
    FEchoStringCommand.Prepare(TServerMethods1_EchoString);
  end;
  FEchoStringCommand.Parameters[0].Value.SetWideString(Value);
  FEchoStringCommand.Execute(ARequestFilter);
  Result := FEchoStringCommand.Parameters[1].Value.GetWideString;
end;

function TServerMethods1Client.ReverseString(Value: string; const ARequestFilter: string): string;
begin
  if FReverseStringCommand = nil then
  begin
    FReverseStringCommand := FConnection.CreateCommand;
    FReverseStringCommand.RequestType := 'GET';
    FReverseStringCommand.Text := 'TServerMethods1.ReverseString';
    FReverseStringCommand.Prepare(TServerMethods1_ReverseString);
  end;
  FReverseStringCommand.Parameters[0].Value.SetWideString(Value);
  FReverseStringCommand.Execute(ARequestFilter);
  Result := FReverseStringCommand.Parameters[1].Value.GetWideString;
end;

procedure TServerMethods1Client.SyncValue(aKey: string; var aValue: string; var aDateTime: TDateTime; const ARequestFilter: string);
begin
  if FSyncValueCommand = nil then
  begin
    FSyncValueCommand := FConnection.CreateCommand;
    FSyncValueCommand.RequestType := 'GET';
    FSyncValueCommand.Text := 'TServerMethods1.SyncValue';
    FSyncValueCommand.Prepare(TServerMethods1_SyncValue);
  end;
  FSyncValueCommand.Parameters[0].Value.SetWideString(aKey);
  FSyncValueCommand.Parameters[1].Value.SetWideString(aValue);
  FSyncValueCommand.Parameters[2].Value.AsDateTime := aDateTime;
  FSyncValueCommand.Execute(ARequestFilter);
  aValue := FSyncValueCommand.Parameters[1].Value.GetWideString;
  aDateTime := FSyncValueCommand.Parameters[2].Value.AsDateTime;
end;

function TServerMethods1Client.LastUpdate(const ARequestFilter: string): TDateTime;
begin
  if FLastUpdateCommand = nil then
  begin
    FLastUpdateCommand := FConnection.CreateCommand;
    FLastUpdateCommand.RequestType := 'GET';
    FLastUpdateCommand.Text := 'TServerMethods1.LastUpdate';
    FLastUpdateCommand.Prepare(TServerMethods1_LastUpdate);
  end;
  FLastUpdateCommand.Execute(ARequestFilter);
  Result := FLastUpdateCommand.Parameters[0].Value.AsDateTime;
end;

constructor TServerMethods1Client.Create(ARestConnection: TDSRestConnection);
begin
  inherited Create(ARestConnection);
end;

constructor TServerMethods1Client.Create(ARestConnection: TDSRestConnection; AInstanceOwner: Boolean);
begin
  inherited Create(ARestConnection, AInstanceOwner);
end;

destructor TServerMethods1Client.Destroy;
begin
  FEchoStringCommand.DisposeOf;
  FReverseStringCommand.DisposeOf;
  FSyncValueCommand.DisposeOf;
  FLastUpdateCommand.DisposeOf;
  inherited;
end;

end.

