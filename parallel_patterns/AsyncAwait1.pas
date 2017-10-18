unit AsyncAwait1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TfrmAsyncAwait = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    procedure Button1Click(Sender: TObject);
  private
    procedure Work;
  public
  end;

var
  frmAsyncAwait: TfrmAsyncAwait;

implementation

{$R *.dfm}

uses
  OtlParallel;

procedure TfrmAsyncAwait.Button1Click(Sender: TObject);
var
  button: TButton;
begin
  button := (Sender as TButton);

  button.Caption := 'Working ...';
  button.Enabled := false;

  Async(Work)
    .Await(
      procedure
      begin
        button.Caption := 'Do more work';
        button.Enabled := true;
      end);
end;

procedure TfrmAsyncAwait.Work;
begin
  Sleep(5000);
end;

end.
