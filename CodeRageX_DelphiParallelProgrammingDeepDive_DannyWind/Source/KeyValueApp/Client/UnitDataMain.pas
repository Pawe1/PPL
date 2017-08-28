unit UnitDataMain;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections,
  System.Threading, FMX.Types;

type
  TValueDateTime = record
    Value: string;
    DateTime: TDateTime;
    Stored: Boolean;
  end;
  TTaskStatusSet = set of TTaskStatus;
  TDisplayEvent = procedure(aKey, aValue: string) of object;
  TKeyStore = class(TDictionary<string, TValueDateTime>)
  private
    FOnDisplay: TDisplayEvent;
  protected
    procedure DoDisplay(aKey, aValue: string);
  public
    property OnDisplay: TDisplayEvent read FOnDisplay write FOnDisplay;
  end;
  TDataModuleMain = class(TDataModule)
    TimerRefresh: TTimer;
    procedure DataModuleCreate(Sender: TObject);
    procedure TimerRefreshTimer(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
  private
    { Private declarations }
    FTask: TDictionary<string, ITask>;
    procedure GetLastUpdate;
    procedure SyncValues;
  public
    { Public declarations }
    FKeyStore: TKeyStore;
    function GetTaskCount(aStatusSet: TTaskStatusSet): Integer;
    procedure SyncValue(aKey: string);
  end;

const
  TTaskStatusPending : TTaskStatusSet = [TTaskStatus.Created, TTaskStatus.Running,
                                       TTaskStatus.WaitingToRun, TTaskStatus.WaitingForChildren];
  TTaskStatusFailed : TTaskStatusSet = [TTaskStatus.Canceled, TTaskStatus.Exception];
  TTaskStatusSucceeded : TTaskStatusSet = [TTaskStatus.Completed];

var
  DataModuleMain: TDataModuleMain;

implementation

{%CLASSGROUP 'FMX.Controls.TControl'}

uses System.Types, System.DateUtils, System.SyncObjs, ClientModuleUnit1,
  UnitGlobalStatus;

{$R *.dfm}

{ TDataModuleMain }

function TDataModuleMain.GetTaskCount(aStatusSet: TTaskStatusSet): Integer;
var
  lArray: TArray<TPair<string,ITask>>;
  i: Integer;
  lCount: Integer;
begin
  {Get a local copy to work with}
  TMonitor.Enter(FTask);
  try
    lArray := FTask.ToArray;
  finally
    TMonitor.Exit(FTask);
  end;

  lCount := 0;
  {Count all tasks that are in this set ot TTaskStatus}
  for i := Low(lArray) to High(lArray) do
  begin
    if (lArray[i].Value <> nil)
       and (lArray[i].Value.Status in aStatusSet) then
      Inc(lCount);
  end;
  Result := lCount;
end;

procedure TDataModuleMain.DataModuleCreate(Sender: TObject);
begin
  FTask := TDictionary<string, ITask>.Create;
  FKeyStore := TKeyStore.Create;
end;

procedure TDataModuleMain.SyncValue(aKey: string);
var
  lClientModule: TClientModule1;
  lKey: string;
  lTask: ITask;
  localValueDateTime: TValueDateTime;
  serverValueDateTime: TValueDateTime;
begin
  lClientModule := TClientModule1.Create(Self);
  lKey := aKey;

  TMonitor.Enter(FTask);
  try
    if not( FTask.TryGetValue(lKey, lTask)
            and (lTask.Status in [TTaskStatus.Created, TTaskStatus.Running,
                                  TTaskStatus.WaitingToRun, TTaskStatus.WaitingForChildren])) then
    begin {There is no pending Task for this Key, so we can start a new one}
      {Log previous failure}
      if (lTask <> nil) and (lTask.Status = TTaskStatus.Exception) then
      begin {We know the Task is finished, calling Wait gets us the exceptions}
        try
          lTask.Wait;
        except
          on E : EAggregateException do
            Log.TimeStamp(E.ToString);
        end;
      end;
      lTask := TTask.Run(
        procedure
        begin
          {Get latest local value from Dictionary}
          localValueDateTime.Value := '';
          localValueDateTime.DateTime := EncodeDateTime(2000,1,1, 0,0,0,0);
          localValueDateTime.Stored := True;
          TMonitor.Enter(FKeyStore);
          try
            FKeyStore.TryGetValue(lKey, localValueDateTime);
          finally
            TMonitor.Exit(FKeyStore);
          end;

          {Sync value with server}
          serverValueDateTime := localValueDateTime;
          try
            lClientModule.SyncValue(lKey, serverValueDateTime.Value, serverValueDateTime.DateTime);
          finally
            lClientModule.Free;
          end;

          {Refresh value in Dictionary if newer}
          if (CompareDateTime(serverValueDateTime.DateTime, localValueDateTime.DateTime) = GreaterThanValue) then
          begin {server DateTime Value is newer}
            {update in FKeyStore}
            TMonitor.Enter(FKeyStore);
            try
              FKeyStore.AddOrSetValue(lKey, serverValueDateTime);
            finally
              TMonitor.Exit(FKeyStore);
            end;
            {Feedback to user interface}
            TThread.Queue(TThread.Current,
              procedure
              begin
                FKeyStore.DoDisplay(lKey, serverValueDateTime.Value);
              end);
          end;
        end);
      FTask.AddOrSetValue(lKey, lTask);
    end;
  finally
    TMonitor.Exit(FTask);
  end;
end;

procedure TDataModuleMain.TimerRefreshTimer(Sender: TObject);
var
  lServerLastUpdate: TDateTime;
  lClientLastUpdate: TDateTime;
begin
  lServerLastUpdate := TInterlocked.CompareExchange(Double(UnitGlobalStatus.ServerLastUpdate), 0.0, 0.0);
  lClientLastUpdate := TInterlocked.CompareExchange(Double(UnitGlobalStatus.ClientLastUpdate), 0.0, 0.0);

  if CompareDateTime(lServerLastUpdate, lClientLastUpdate) = GreaterThanValue then
  begin {ServerLastUpdate is newer}
    {Although it is possible that the next SyncValues may fail, the system is
     busy enough for this to be fixed by a subsequent update to the server.
     So let's just update the ClientLastUpdate.}
    TInterlocked.CompareExchange(Double(UnitGlobalStatus.ClientLastUpdate), Double(lServerLastUpdate), Double(lClientLastUpdate));

    SyncValues;
  end
  else
  begin {Request new ServerLastUpdate from server}
    GetLastUpdate;
  end;
end;

procedure TDataModuleMain.SyncValues;
var
  lArray: TArray<TPair<string,TValueDateTime>>;
  i: Integer;
begin
  {Get a local copy of the Keys}
  TMonitor.Enter(FKeyStore);
  try
    lArray := FKeyStore.ToArray;
  finally
    TMonitor.Exit(FKeyStore);
  end;

  {Use local copy to asynchronously request latest values}
  for i := Low(lArray) to High(lArray) do
  begin
    SyncValue(lArray[i].Key);
  end;
end;

procedure TDataModuleMain.DataModuleDestroy(Sender: TObject);
begin
  {Disable update Timer}
  TimerRefresh.Enabled := False;

  {Needs some Task canceling and clean-up code here}
end;

procedure TDataModuleMain.GetLastUpdate;
var
  lClientModule: TClientModule1;
  lKey: string;
  lTask: ITask;
begin
  lClientModule := TClientModule1.Create(Self);
  lKey := 'LastUpdate';

  TMonitor.Enter(FTask);
  try
    if not( FTask.TryGetValue(lKey, lTask)
            and (lTask.Status in [TTaskStatus.Created, TTaskStatus.Running,
                                  TTaskStatus.WaitingToRun, TTaskStatus.WaitingForChildren])) then
    begin {There is no pending Task for this Key, so we can start one}
      {Log previous failure}
      if (lTask <> nil) and (lTask.Status = TTaskStatus.Exception) then
      begin {We know the Task is finished, calling Wait gets us the exceptions}
        try
          lTask.Wait;
        except
          on E : EAggregateException do
            Log.TimeStamp(E.ToString);
        end;
      end;
      lTask := TTask.Run(
        procedure
        var
          lLocalDateTime: TDateTime;
          lServerDateTime: TDateTime;
        begin
          try
            lServerDateTime := lClientModule.LastUpdate;
          finally
            lClientModule.Free;
          end;

          {Only update if newer}
          lLocalDateTime := TInterlocked.CompareExchange(Double(UnitGlobalStatus.ServerLastUpdate), 0.0, 0.0);
          if CompareDateTime(lServerDateTime, lLocalDateTime) = GreaterThanValue then
          begin {Last Server update is newer than local}
            {Store new LastUpdate in local LastUpdate}
            TInterlocked.CompareExchange(Double(UnitGlobalStatus.ServerLastUpdate), Double(lServerDateTime), Double(lLocalDateTime));
          end;
        end);
      FTask.AddOrSetValue(lKey, lTask);
    end;
  finally
    TMonitor.Exit(FTask);
  end;
end;


{ TKeyStore }

procedure TKeyStore.DoDisplay(aKey, aValue: string);
begin
  if Assigned(FOnDisplay) then
    FOnDisplay(aKey, aValue);
end;

end.
