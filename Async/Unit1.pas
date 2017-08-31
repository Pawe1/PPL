unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  System.Threading,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.StdCtrls,
  Vcl.WinXCtrls, Vcl.ExtCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    ProgressBar1: TProgressBar;
    Button2: TButton;
    ActivityIndicator1: TActivityIndicator;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    tmrUpdateProgress: TTimer;
    Button7: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button7Click(Sender: TObject);
  private
    { Private declarations }
    FExtender: TProc;
    FTask: ITask;
    FProgress: Integer;
    procedure AddLabel;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  System.SyncObjs,
  AA;

type
  Callback = TProc;

const
  Recurrences = 10;

function SlowFunction: Boolean;
begin
  Result := False;
  Sleep(5000);
  Result := True;
end;

procedure SlowEvent2(const AProgressCallback: TProc<Integer>);
var
  LC: Integer;
  Progress: Integer;
begin
  for LC := 0 to Recurrences - 1 do
  begin
    Sleep(5000);
    Progress := (100 * LC) div (Recurrences - 1);
    TThread.Queue(nil,
      procedure
      begin
        AProgressCallback(Progress)
      end
    );
  end;
end;

procedure SlowEvent;
var
  LC: Integer;
  Progress: Integer;
begin
  for LC := 0 to Recurrences - 1 do
  begin
    Sleep(1000);

    Progress := (100 * LC) div (Recurrences - 1);

    //system.Classes.TThreadMethod

  //  TThread.Synchronize();


      TThread.Queue{Synchronize}(nil,
                          procedure
                          begin
                            Form1.ProgressBar1.Position := Progress;
                            ShowMessage (Format('Progress: %d%%', [Progress]));
                          end);
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  ActivityIndicator1.Animate := True;
  FTask := TTask.Create(SlowEvent);
  FTask.Start;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  if Assigned(FTask) then
    FTask.Cancel;
end;

procedure TForm1.Button3Click(Sender: TObject);
var
  OK: IFuture<Boolean>;
  FutureObject: IFuture<Integer>;
begin
  OK := TTask.Future<Boolean>(SlowFunction);

  if OK.Value then
    Caption := Format('%s - OK', [TimeToStr(Now)]);
end;

procedure TForm1.Button4Click(Sender: TObject);
begin


//  ActivityIndicator1.Animate := True;
//  FTask := TTask.Create(
//    SlowEvent2(
//      procedure (A: Integer)
//      begin
//        ProgressBar1.Position := A;
//      end
//    )
//  );
//  FTask.Start;
end;

procedure TForm1.Button5Click(Sender: TObject);
begin
  ActivityIndicator1.Animate := True;
  Button5.Enabled := False;
  AsyncAwait(
    procedure
    begin
      Sleep(5000);
    end,
    procedure
    begin
      ActivityIndicator1.Animate := False;
      Button5.Enabled := True;
    end);
end;

procedure TForm1.Button6Click(Sender: TObject);
begin
//  BeforeTest;
  FProgress := 0;
  tmrUpdateProgress.Enabled := true;

  // While the parallel code is running, main form must process messages, so we
  // have to push a TParallel.For into background by wrapping it inside a task.

  FTask := TTask.Run(
    procedure
    var
      processed: integer; // shared counter
      total    : integer; // total number of items to be processed
    begin
      processed := 0;
      total := 1000;
      TParallel.For(1, 1000,
        procedure (i: integer)
        var
          new: integer;
        begin
          Sleep(10); //do the work
          new := TInterlocked.Increment(processed); // increment the shared counter
          if (new mod 10) = 0 then // update the progress bar every 10 processed items
            // Even with one reader and one writer accessing an 'integer'
            // is not atomic!
            TInterlocked.Exchange(FProgress, Round(new / total * 100));
        end
      ); // TParallel.For

      // Disable the timer when everything is processed.
      TThread.Queue(nil,
        procedure
        begin
          // We have no idea if timer proc 'saw' FProgress = 100 so let's force
          // the last update.
          ProgressBar1{pbParallel}.Position := 100;
          tmrUpdateProgress.Enabled := false;
          //AfterTest;
        end
      ); //TThread.Queue
    end
  ); // TTask.Run
end;

procedure TForm1.Button7Click(Sender: TObject);
begin
  FExtender;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  AddLabel;
end;

procedure TForm1.AddLabel;
var
  L: TLabel;
begin
  L := TLabel.Create(Self);
  with L do
  begin
    Name := 'Test';
    Caption := 'XXXXXXXXXXXXX';
    Left := 100;
    Top := 100;
    Parent := Self;
  end;

  FExtender := procedure
       begin
         Caption := L.Name;
         L.Caption := TimeToStr(Now);
       end;

  AsyncAwait(
    procedure
    begin
      L.Free;
      Sleep(3000);
    end,
    procedure
    begin
    end

//    procedure
//    begin
//      L.Caption := TimeToStr(Now);
//    end
  );
end;

end.
