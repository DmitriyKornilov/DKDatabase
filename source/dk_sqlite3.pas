unit DK_SQLite3;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, SQLite3Conn, SQLDB, DK_SQLUtils, DK_Vector,
  DK_Dialogs;

type

  { TSQLite3 }

  TSQLite3 = class
  private
    FConnection: TSQLite3Connection;
    FTransaction: TSQLTransaction;
    FQuery: TSQLQuery;

    function EscStr(const AString: String): String;

  public
    constructor Create;
    destructor  Destroy; override;

    procedure Connect(const AFileName: String);
    procedure ExecuteScript(const AFileName: String);

    function LastWritedInt32ID(const ATableName: String): Integer;
    function LastWritedInt64ID(const ATableName: String): Int64;

    {KeyPickList получает из таблицы ATable значения из полей AKeyField и APickField,
  отсортированные по AOrderField (если AOrderField=EmptyStr, то AOrderField=APickField),
  и записывает их в AKeyList, APickList
  Если AKeyValueNotZero=True - отбор значений AKeyField<>0}
    procedure KeyPickList(const ATable, AKeyField, APickField: String;
                          out AKeyVector: TIntVector;
                          out APickVector: TStrVector;
                          const AKeyValueNotZero: Boolean = False;
                          const AOrderField: String = '');
    procedure KeyPickList(const ATable, AKeyField, APickField: String;
                          out AKeyList, APickList: TStringList;
                          const AKeyValueNotZero: Boolean = False;
                          const AOrderField: String = '');

  end;

implementation

{ TSQLite3 }

function TSQLite3.EscStr(const AString: String): String;
begin
  Result:= StringReplace(AString, ' ', '', [rfReplaceAll]);
  Result:= ' [' + Result + '] ';
end;

constructor TSQLite3.Create;
begin
  FConnection:= TSQLite3Connection.Create(nil);
  FTransaction:= TSQLTransaction.Create(nil);
  FQuery:= TSQLQuery.Create(nil);

  FConnection.CharSet:= 'UTF8';
  FConnection.Transaction:= FTransaction;
  FConnection.OpenFlags:= FConnection.OpenFlags + [sofCreate, sofReadWrite];
  FQuery.SQLConnection:= FConnection;
  FQuery.Transaction:= FTransaction;
end;

destructor TSQLite3.Destroy;
begin
  if Assigned(FQuery) then FreeAndNil(FQuery);
  if Assigned(FTransaction) then FreeAndNil(FTransaction);
  if Assigned(FConnection) then FreeAndNil(FConnection);
  inherited Destroy;
end;

procedure TSQLite3.Connect(const AFileName: String);
begin
  FConnection.DatabaseName:= AFileName;
  FConnection.Open;
end;

procedure TSQLite3.ExecuteScript(const AFileName: String);
var
  SQLScript: TSQLScript;
begin
  SQLScript:= TSQLScript.Create(nil);
  try
    SQLScript.DataBase:= FConnection;
    SQLScript.Transaction:= FTransaction;
    SQLScript.Script.LoadFromFile(AFileName);
    try
      SQLScript.Execute;
      FTransaction.Commit;
    except
      FTransaction.Rollback;
    end;

  finally
    FreeAndNil(SQLScript);
  end;
end;

function TSQLite3.LastWritedInt32ID(const ATableName: String): Integer;
begin
  Result:= 0;
  QSetQuery(FQuery);
  QSetSQL(
    'SELECT last_insert_rowid() AS LastID ' +
    'FROM' + EscStr(ATableName) +
    'LIMIT 1'
    );
  QOpen;
  if not QIsEmpty then
    Result:= QFieldInt('LastID');
  QClose;
end;

function TSQLite3.LastWritedInt64ID(const ATableName: String): Int64;
begin
  Result:= 0;
  QSetQuery(FQuery);
  QSetSQL(
    'SELECT last_insert_rowid() AS LastID ' +
    'FROM' + EscStr(ATableName) +
    'LIMIT 1'
    );
  QOpen;
  if not QIsEmpty then
    Result:= QFieldInt64('LastID');
  QClose;
end;

procedure TSQLite3.KeyPickList(const ATable, AKeyField, APickField: String;
                          out AKeyVector: TIntVector;
                          out APickVector: TStrVector;
                          const AKeyValueNotZero: Boolean = False;
                          const AOrderField: String = '');
var
  QueryStr, KeyField, PickField, OrderField: String;
begin
  AKeyVector:= nil;
  APickVector:= nil;

  KeyField:= EscStr(AKeyField);
  PickField:= EscStr(APickField);

  if AOrderField=EmptyStr then
    OrderField:=  PickField
  else
    OrderField:= EscStr(AOrderField);

  QueryStr:=
    'SELECT' + KeyField + ',' + PickField +
    'FROM'   + EscStr(ATable);
  if AKeyValueNotZero then
    QueryStr:= QueryStr +
      'WHERE' + KeyField + '<> 0 ';
  QueryStr:= QueryStr +
    'ORDER BY' + OrderField;

  QSetQuery(FQuery);
  QSetSQL(QueryStr);
  QOpen;
  if not QIsEmpty then
  begin
    QFirst;
    while not QEOF do
    begin
      VAppend(AKeyVector, QFieldInt(AKeyField));
      VAppend(APickVector, QFieldStr(APickField));
      QNext;
    end;
  end;
  QClose;
end;

procedure TSQLite3.KeyPickList(const ATable, AKeyField, APickField: String;
                          out AKeyList, APickList: TStringList;
                          const AKeyValueNotZero: Boolean = False;
                          const AOrderField: String = '');
var
  VKey: TIntVector;
  VPick: TStrVector;
  i: Integer;
begin
  {%H-}APickList.Clear;
  {%H-}AKeyList.Clear;
  KeyPickList(ATable, AKeyField, APickField,
                 VKey, VPick, AKeyValueNotZero, AOrderField);
  for i:= 0 to High(VKey) do
  begin
    AKeyList.Add(IntToStr(VKey[i]));
    APickList.Add(VPick[i]);
  end;
end;

end.

