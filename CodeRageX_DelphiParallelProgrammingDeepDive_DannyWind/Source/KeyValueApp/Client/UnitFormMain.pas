unit UnitFormMain;

{===============================================================================
 Demo for using the parallel programming library in a FMX client of a REST based
 server with a Key/Value store.

 We monitor TTasks in a dictionary, mainly to prevent Tasks from piling up
 when we are disconnected from the server. We also limit each Key/Value to
 one Task, if you have many Key/Value pairs you should reduce this
 even further.

 We use a local cache of Key/Value pairs in a Dictionary. This means we can
 get at Key/Value pairs in O(1) "one-step" due to the internal hashing of
 the Dictionary. This speed is beneficial for resource sharing.

 Author: Danny Wind
 License: Creative Commons CC-BY
===============================================================================}

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.Edit, FMX.Layouts, FMX.Objects;

type
  TFormMain = class(TForm)
    ToolBar1: TToolBar;
    LabelTitle: TLabel;
    StatusBarTasks: TStatusBar;
    LabelHint: TLabel;
    Layout2: TLayout;
    LabelKetchup: TLabel;
    RadioButtonKetchupYes: TRadioButton;
    RadioButtonKetchupNo: TRadioButton;
    Layout1: TLayout;
    LabelMustard: TLabel;
    RadioButtonMustardNo: TRadioButton;
    RadioButtonMustardYes: TRadioButton;
    Layout3: TLayout;
    LabelPepper: TLabel;
    RadioButtonPepperNo: TRadioButton;
    RadioButtonPepperYes: TRadioButton;
    Layout4: TLayout;
    LabelSalt: TLabel;
    RadioButtonSaltNo: TRadioButton;
    RadioButtonSaltYes: TRadioButton;
    procedure OnApplicationHint(Sender: TObject);
    procedure RadioButtonChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    FUpdating: Integer;
    procedure InitKeys;
    procedure InitOnChange;
    procedure DisplayValue(aKey,aValue:string);
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.fmx}

uses System.DateUtils, System.Generics.Collections, ClientModuleUnit1, UnitDataMain;

procedure TFormMain.OnApplicationHint(Sender: TObject);
var
  lSucceeded, lPending, lFailed: Integer;
begin
  if SameStr(Application.Hint, StatusBarTasks.Hint) then
  begin
    lSucceeded := DataModuleMain.GetTaskCount(TTaskStatusSucceeded);
    lPending := DataModuleMain.GetTaskCount(TTaskStatusPending);
    lFailed := DataModuleMain.GetTaskCount(TTaskStatusFailed);
    LabelHint.Text := ' Done: ' + lSucceeded.ToString +
                      ' Pending: ' + lPending.ToString +
                      ' Failed: ' + lFailed.ToString;
  end;
end;

procedure TFormMain.InitKeys;
var
  lKey: string;
  lValueDateTime: TValueDateTime;
begin
  {Set identifying keys for each RadioButton}
  lKey := 'Ketchup';
  lValueDateTime.Value := 'None';
  lValueDateTime.DateTime := EncodeDateTime(2000,1,1, 0,0,0,0);
  RadioButtonKetchupYes.TagString := lKey;
  RadioButtonKetchupNo.TagString := lKey;
  TMonitor.Enter(DataModuleMain.FKeyStore);
  try
    DataModuleMain.FKeyStore.AddOrSetValue(lKey, lValueDateTime);
  finally
    TMonitor.Exit(DataModuleMain.FKeyStore);
  end;

  lKey := 'Salt';
  lValueDateTime.Value := 'None';
  lValueDateTime.DateTime := EncodeDateTime(2000,1,1, 0,0,0,0);
  RadioButtonSaltYes.TagString := lKey;
  RadioButtonSaltNo.TagString := lKey;
  TMonitor.Enter(DataModuleMain.FKeyStore);
  try
    DataModuleMain.FKeyStore.AddOrSetValue(lKey, lValueDateTime);
  finally
    TMonitor.Exit(DataModuleMain.FKeyStore);
  end;

  lKey := 'Pepper';
  lValueDateTime.Value := 'None';
  lValueDateTime.DateTime := EncodeDateTime(2000,1,1, 0,0,0,0);
  RadioButtonPepperYes.TagString := lKey;
  RadioButtonPepperNo.TagString := lKey;
  TMonitor.Enter(DataModuleMain.FKeyStore);
  try
    DataModuleMain.FKeyStore.AddOrSetValue(lKey, lValueDateTime);
  finally
    TMonitor.Exit(DataModuleMain.FKeyStore);
  end;

  lKey := 'Mustard';
  lValueDateTime.Value := 'None';
  lValueDateTime.DateTime := EncodeDateTime(2000,1,1, 0,0,0,0);
  RadioButtonMustardYes.TagString := lKey;
  RadioButtonMustardNo.TagString := lKey;
  TMonitor.Enter(DataModuleMain.FKeyStore);
  try
    DataModuleMain.FKeyStore.AddOrSetValue(lKey, lValueDateTime);
  finally
    TMonitor.Exit(DataModuleMain.FKeyStore);
  end;
end;

procedure TFormMain.InitOnChange;
var
  i: Integer;
  lComponent: TComponent;
  lKey: string;
begin
  {Connect RadioButton OnChange for feedback to KeyStore}
  for i := 0  to ComponentCount -1 do
  begin
    lComponent := Components[i];
    if (lComponent is TRadioButton) then
    begin
      lKey := (lComponent as TRadioButton).TagString;
      if not(lKey.IsEmpty) then
      begin
        (lComponent as TRadioButton).OnChange := RadioButtonChange;
      end;
    end;
  end;
  {Connect DisplayValue for feedback from KeyStore}
  DataModuleMain.FKeyStore.OnDisplay := DisplayValue;
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  InitKeys;
  InitOnChange;
  Application.OnHint := OnApplicationHint;
end;

procedure TFormMain.DisplayValue(aKey,aValue:string);
var
  i: Integer;
  lComponent: TComponent;
  lKey, lValue: string;
begin
  for i := 0  to ComponentCount -1 do
  begin
    lComponent := Components[i];
    if (lComponent is TRadioButton) then
    begin
      lKey := (lComponent as TRadioButton).TagString;
      if not(lKey.IsEmpty)
         and SameStr(aKey, lKey) then
      begin
        lValue := (lComponent as TRadioButton).Text;
        if SameStr(aValue, lValue) then
        begin
          Inc(FUpdating); {Prevent Display from Server from Syncing back to Server}
          try
            (lComponent as TRadioButton).IsChecked := True;
          finally
            Dec(FUpdating);
          end;
        end;
      end;
    end;
  end;
end;

procedure TFormMain.RadioButtonChange(Sender: TObject);
var
  lKey: string;
  lValueDateTime: TValueDateTime;
begin
  if (Sender as TRadioButton).IsChecked then
  begin
    lKey := (Sender as TRadioButton).TagString;
    lValueDateTime.Value := (Sender as TRadioButton).Text;
    lValueDateTime.DateTime := Now;
    lValueDateTime.Stored := False;
    {Store new values in local Dictionary}
    if not(lKey.IsEmpty) then
    begin
      TMonitor.Enter(DataModuleMain.FKeyStore);
      try
        DataModuleMain.FKeyStore.AddOrSetValue(lKey, lValueDateTime);
      finally
        TMonitor.Exit(DataModuleMain.FKeyStore);
      end;
    end;
    if (FUpdating = 0) then
    begin {Sync value with server}
      DataModuleMain.SyncValue(lKey);
    end;
  end;
end;

end.
