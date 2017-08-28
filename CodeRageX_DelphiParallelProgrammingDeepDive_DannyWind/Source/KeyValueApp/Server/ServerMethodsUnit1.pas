unit ServerMethodsUnit1;

interface

uses System.SysUtils, System.Classes, System.Json,
    Datasnap.DSServer, Datasnap.DSAuth, DataSnap.DSProviderDataModuleAdapter,
  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Error, FireDAC.UI.Intf,
  FireDAC.Phys.Intf, FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Stan.Async,
  FireDAC.Phys, FireDAC.FMXUI.Wait, FireDAC.Stan.Param, FireDAC.DatS,
  FireDAC.DApt.Intf, FireDAC.DApt, Data.DB, FireDAC.Comp.DataSet,
  FireDAC.Comp.Client, FireDAC.Phys.IB, FireDAC.Phys.IBDef;

type
{$METHODINFO ON}
  TServerMethods1 = class(TDataModule)
    FDConnection1: TFDConnection;
    FDQuerySelect: TFDQuery;
    FDQueryInsert: TFDQuery;
    FDQueryUpdate: TFDQuery;
  private
    { Private declarations }
  public
    { Public declarations }
    function EchoString(Value: string): string;
    function ReverseString(Value: string): string;
    procedure SyncValue(aKey: string;var aValue: string;var aDateTime: TDateTime);
    function LastUpdate: TDateTime;
  end;
{$METHODINFO OFF}

implementation

{%CLASSGROUP 'FMX.Controls.TControl'}

{$R *.dfm}


uses System.Types, System.StrUtils, System.DateUtils, System.SyncObjs, UnitGlobalStatus;

function TServerMethods1.EchoString(Value: string): string;
begin
  Result := Value;
end;

function TServerMethods1.LastUpdate: TDateTime;
begin
  Result := TInterlocked.CompareExchange(Double(UnitGlobalStatus.ServerLastUpdate),0.0,0.0);
end;

function TServerMethods1.ReverseString(Value: string): string;
begin
  Result := System.StrUtils.ReverseString(Value);
end;

procedure TServerMethods1.SyncValue(aKey: string; var aValue: string; var aDateTime: TDateTime);
var
  lIsEmpty: Boolean;
  lDBValue: string;
  lDBDateTime: TDateTime;
begin
  FDQuerySelect.ParamByName('AKEY').AsString := aKey;
  FDQuerySelect.Open;
  if FDQuerySelect.IsEmpty then
  begin
    lIsEmpty := True;
    lDBValue := '';
    lDBDateTime := EncodeDateTime(2000,1,1, 0,0,0,0);
  end
  else
  begin
    lIsEmpty := False;
    lDBValue := FDQuerySelect.FieldByName('AVALUE').AsString;
    lDBDateTime := FDQuerySelect.FieldByName('ADATETIME').AsDateTime;
  end;
  FDQuerySelect.Close;

  {Possible race conditions here:
   another client/thread may have inserted after we selected it as empty
   another client may have updated this one with a newer ADATETIME than we just selected}
  if (CompareDateTime(aDateTime, lDBDateTime) = GreaterThanValue) then
  begin {aDateTime Value is newer, update in database}
    if lIsEmpty then
    begin
      FDQueryInsert.ParamByName('AKEY').AsString := aKey;
      FDQueryInsert.ParamByName('AVALUE').AsString := aValue;
      FDQueryInsert.ParamByName('ADATETIME').AsDateTime := aDateTime;
      FDQueryInsert.ExecSQL;
    end
    else
    begin
      FDQueryUpdate.ParamByName('AKEY').AsString := aKey;
      FDQueryUpdate.ParamByName('AVALUE').AsString := aValue;
      FDQueryUpdate.ParamByName('ADATETIME').AsDateTime := aDateTime;
      FDQueryUpdate.ExecSQL;
    end;

    {Set datetime in Global Status to Now}
    TInterlocked.Exchange(Double(UnitGlobalStatus.ServerLastUpdate), Now);
  end
  else
  begin {Value is older, just return the DB Values}
    aValue := lDBValue;
    aDateTime := lDBDateTime;
  end;
end;

end.

