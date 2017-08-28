unit UnitGlobalStatus;

interface

var
  ServerLastUpdate: TDateTime;
  ClientLastUpdate: TDateTime;

implementation

uses
  System.DateUtils, System.SyncObjs;

initialization

  {Newer basevalue for server makes sure client loads values when first run}
  TInterlocked.Exchange(Double(ServerLastUpdate), EncodeDateTime(2001,1,1, 0,0,0,0));
  TInterlocked.Exchange(Double(ClientLastUpdate), EncodeDateTime(2000,1,1, 0,0,0,0));

end.
