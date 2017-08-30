unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  System.Threading,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    ProgressBar1: TProgressBar;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
    FTask: ITask;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

type
  Callback = TProc;

const
  Recurrences = 10;

procedure SlowEvent;
var
  LC: Integer;
begin
  for LC := 0 to Recurrences - 1 do
  begin
    Sleep(1000);


    //system.Classes.TThreadMethod

  //  TThread.Synchronize();


                          TThread.Queue{Synchronize}(nil,
                                              procedure
                                              begin
                                                ShowMessage ('Hello');
                                              end);
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  FTask := TTask.Create(SlowEvent);
  FTask.Start;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  if Assigned(FTask) then
    FTask.Cancel;
end;

end.
