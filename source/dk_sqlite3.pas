unit DK_SQLite3;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, SQLite3Conn, SQLDB, DK_SQLUtils, DK_Vector, DK_StrUtils,
  DK_Dialogs;

type

  { TSQLite3 }

  TSQLite3 = class
  private
    FConnection: TSQLite3Connection;
    FTransaction: TSQLTransaction;
    FQuery: TSQLQuery;


  public
    constructor Create;
    destructor  Destroy; override;

    procedure Connect(const AFileName: String);
    procedure ExecuteScript(const AFileName: String);

    procedure List(const ACaption: String;
                   const ATableName, AIDFieldName, AFieldName: String;
                   const AOrderByName: Boolean = False;
                   const AIDNotZero: Boolean = False;
                   const AColorFieldName: String = '');


    procedure Delete(const ATableName, AIDFieldName: String;
                     const AIDValue: Integer);
    procedure Delete(const ATableName, AIDFieldName: String;
                     const AIDValue: Int64);
    procedure Delete(const ATableName, AIDFieldName: String;
                     const AIDValue: TDateTime);
    procedure Delete(const ATableName, AIDFieldName: String;
                     const AIDValue: String;
                     const ACaseSensitivity: Boolean = True);

    procedure UpdateInt32ID(const ATableName, AFieldName, AIDFieldName: String;
                     const AIDValue: Integer;
                     const ANewValue: Integer);
    procedure UpdateInt32ID(const ATableName, AFieldName, AIDFieldName: String;
                     const AIDValue: Integer;
                     const ANewValue: Int64);
    procedure UpdateInt32ID(const ATableName, AFieldName, AIDFieldName: String;
                     const AIDValue: Integer;
                     const ANewValue: TDateTime);
    procedure UpdateInt32ID(const ATableName, AFieldName, AIDFieldName: String;
                     const AIDValue: Integer;
                     const ANewValue: String);

    procedure UpdateInt64ID(const ATableName, AFieldName, AIDFieldName: String;
                     const AIDValue: Int64;
                     const ANewValue: Integer);
    procedure UpdateInt64ID(const ATableName, AFieldName, AIDFieldName: String;
                     const AIDValue: Int64;
                     const ANewValue: Int64);
    procedure UpdateInt64ID(const ATableName, AFieldName, AIDFieldName: String;
                     const AIDValue: Int64;
                     const ANewValue: TDateTime);
    procedure UpdateInt64ID(const ATableName, AFieldName, AIDFieldName: String;
                     const AIDValue: Int64;
                     const ANewValue: String);

    function IsValueInTable(const ATableName, AFieldName: String;
                            const AValue: Integer): Boolean;
    function IsValueInTable(const ATableName, AFieldName: String;
                            const AValue: Int64): Boolean;
    function IsValueInTable(const ATableName, AFieldName: String;
                            const AValue: TDateTime): Boolean;
    function IsValueInTable(const ATableName, AFieldName: String;
                            const AValue: String;
                            const ACaseSensitivity: Boolean = True): Boolean;


    function IsValueInTableNotMatchInt32ID(const ATableName, AFieldName: String;
                            const AValue: Integer;
                            const AIDFieldName: String;
                            const AIDValue: Integer = 0): Boolean;
    function IsValueInTableNotMatchInt32ID(const ATableName, AFieldName: String;
                            const AValue: Int64;
                            const AIDFieldName: String;
                            const AIDValue: Integer = 0): Boolean;
    function IsValueInTableNotMatchInt32ID(const ATableName, AFieldName: String;
                            const AValue: TDateTime;
                            const AIDFieldName: String;
                            const AIDValue: Integer = 0): Boolean;
    function IsValueInTableNotMatchInt32ID(const ATableName, AFieldName: String;
                            const AValue: String;
                            const AIDFieldName: String;
                            const AIDValue: Integer = 0;
                            const ACaseSensitivity: Boolean = True): Boolean;

    function IsValueInTableNotMatchInt64ID(const ATableName, AFieldName: String;
                            const AValue: Integer;
                            const AIDFieldName: String;
                            const AIDValue: Int64 = 0): Boolean;
    function IsValueInTableNotMatchInt64ID(const ATableName, AFieldName: String;
                            const AValue: Int64;
                            const AIDFieldName: String;
                            const AIDValue: Int64 = 0): Boolean;
    function IsValueInTableNotMatchInt64ID(const ATableName, AFieldName: String;
                            const AValue: TDateTime;
                            const AIDFieldName: String;
                            const AIDValue: Int64 = 0): Boolean;
    function IsValueInTableNotMatchInt64ID(const ATableName, AFieldName: String;
                            const AValue: String;
                            const AIDFieldName: String;
                            const AIDValue: Int64 = 0;
                            const ACaseSensitivity: Boolean = True): Boolean;

    function LastWritedInt32ID(const ATableName: String): Integer;
    function LastWritedInt64ID(const ATableName: String): Int64;

    function LastWritedInt32Value(const ATableName, AFieldName: String): Integer;
    function LastWritedInt64Value(const ATableName, AFieldName: String): Int64;
    function LastWritedDateTimeValue(const ATableName, AFieldName: String): TDateTime;
    function LastWritedStringValue(const ATableName, AFieldName: String): String;


    procedure KeyPickList(const ATableName, AKeyFieldName, APickFieldName: String;
                          out AKeyVector: TIntVector;
                          out APickVector: TStrVector;
                          const AKeyValueNotZero: Boolean = False;
                          const AOrderFieldName: String = '');
    procedure KeyPickList(const ATableName, AKeyFieldName, APickFieldName: String;
                          out AKeyList, APickList: TStringList;
                          const AKeyValueNotZero: Boolean = False;
                          const AOrderFieldName: String = '');

  end;



implementation

uses USQLite3ListForm;


{ TSQLite3 }

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

procedure TSQLite3.List(const ACaption: String; const ATableName, AIDFieldName,
  AFieldName: String; const AOrderByName: Boolean; const AIDNotZero: Boolean;
  const AColorFieldName: String);
var
  LF: TSQLite3ListForm;
begin
  LF:= TSQLite3ListForm.Create(nil);
  LF.Caption:= ACaption;
  LF.ListQuery.DataBase:= FConnection;
  LF.ListQuery.Transaction:= FTransaction;
  LF.WriteQuery.DataBase:= FConnection;
  LF.WriteQuery.Transaction:= FTransaction;
  LF.SetNames(ATableName, AIDFieldName, AFieldName, AColorFieldName);
  LF.ListQuery.SQL.Clear;
  LF.ListQuery.SQL.Add('SELECT * FROM' + SqlEsc(ATableName));
  if AIDNotZero then
    LF.ListQuery.SQL.Add('WHERE' + SqlEsc(AIDFieldName) + '> 0');
  if AOrderByName then
    LF.ListQuery.SQL.Add('ORDER BY ' + SqlEsc(AFieldName));
  try
    LF.ShowModal;
  finally
    FreeAndNil(LF);
  end;
end;

procedure TSQLite3.Delete(const ATableName, AIDFieldName: String;
                          const AIDValue: String;
                          const ACaseSensitivity: Boolean = True);
var
  WhereStr, Value: String;
begin
  QSetQuery(FQuery);
  try
    if ACaseSensitivity then
    begin
      WhereStr:= 'WHERE' + SqlEsc(AIDFieldName) + '= :IDValue';
      Value:= AIDValue;
    end
    else begin
      WhereStr:= 'WHERE UPPER(' + SqlEsc(AIDFieldName) + ') = :IDValue';
      Value:= SUpper(AIDValue);
    end;
    QSetSQL(
      'DELETE FROM' + SqlEsc(ATableName) +
      WhereStr
    );
    QParamStr('IDValue', Value);
    QExec;
    QCommit;
  except
    QRollback;
  end;
end;

procedure PrepareUpdate(const ATableName, AFieldName, AIDFieldName: String;
                        out ASQL: String);
begin
  ASQL:=
    'UPDATE' + SqlEsc(ATableName) +
    'SET'    + SqlEsc(AFieldName)   + '= :NewValue ' +
    'WHERE'  + SqlEsc(AIDFieldName) + '= :IDValue'
end;

procedure TSQLite3.UpdateInt32ID(const ATableName, AFieldName,
  AIDFieldName: String; const AIDValue: Integer; const ANewValue: Integer);
var
  S: String;
begin
  try
    PrepareUpdate(ATableName, AFieldName, AIDFieldName, S);
    QSetQuery(FQuery);
    QSetSQL(S);
    QParamInt('IDValue', AIDValue);
    QParamInt('NewValue', ANewValue);
    QExec;
    QCommit;
  except
    QRollback;
  end;
end;

procedure TSQLite3.UpdateInt32ID(const ATableName, AFieldName,
  AIDFieldName: String; const AIDValue: Integer; const ANewValue: Int64);
var
  S: String;
begin
  try
    PrepareUpdate(ATableName, AFieldName, AIDFieldName, S);
    QSetQuery(FQuery);
    QSetSQL(S);
    QParamInt('IDValue', AIDValue);
    QParamInt64('NewValue', ANewValue);
    QExec;
    QCommit;
  except
    QRollback;
  end;
end;

procedure TSQLite3.UpdateInt32ID(const ATableName, AFieldName,
  AIDFieldName: String; const AIDValue: Integer; const ANewValue: TDateTime);
var
  S: String;
begin
  try
    PrepareUpdate(ATableName, AFieldName, AIDFieldName, S);
    QSetQuery(FQuery);
    QSetSQL(S);
    QParamInt('IDValue', AIDValue);
    QParamDT('NewValue', ANewValue);
    QExec;
    QCommit;
  except
    QRollback;
  end;
end;

procedure TSQLite3.UpdateInt32ID(const ATableName, AFieldName,
  AIDFieldName: String; const AIDValue: Integer; const ANewValue: String);
var
  S: String;
begin
  try
    PrepareUpdate(ATableName, AFieldName, AIDFieldName, S);
    QSetQuery(FQuery);
    QSetSQL(S);
    QParamInt('IDValue', AIDValue);
    QParamStr('NewValue', ANewValue);
    QExec;
    QCommit;
  except
    QRollback;
  end;
end;

procedure TSQLite3.UpdateInt64ID(const ATableName, AFieldName,
  AIDFieldName: String; const AIDValue: Int64; const ANewValue: Integer);
var
  S: String;
begin
  try
    PrepareUpdate(ATableName, AFieldName, AIDFieldName, S);
    QSetQuery(FQuery);
    QSetSQL(S);
    QParamInt64('IDValue', AIDValue);
    QParamInt('NewValue', ANewValue);
    QExec;
    QCommit;
  except
    QRollback;
  end;
end;

procedure TSQLite3.UpdateInt64ID(const ATableName, AFieldName,
  AIDFieldName: String; const AIDValue: Int64; const ANewValue: Int64);
var
  S: String;
begin
  try
    PrepareUpdate(ATableName, AFieldName, AIDFieldName, S);
    QSetQuery(FQuery);
    QSetSQL(S);
    QParamInt64('IDValue', AIDValue);
    QParamInt64('NewValue', ANewValue);
    QExec;
    QCommit;
  except
    QRollback;
  end;
end;

procedure TSQLite3.UpdateInt64ID(const ATableName, AFieldName,
  AIDFieldName: String; const AIDValue: Int64; const ANewValue: TDateTime);
var
  S: String;
begin
  try
    PrepareUpdate(ATableName, AFieldName, AIDFieldName, S);
    QSetQuery(FQuery);
    QSetSQL(S);
    QParamInt64('IDValue', AIDValue);
    QParamDT('NewValue', ANewValue);
    QExec;
    QCommit;
  except
    QRollback;
  end;
end;

procedure TSQLite3.UpdateInt64ID(const ATableName, AFieldName,
  AIDFieldName: String; const AIDValue: Int64; const ANewValue: String);
var
  S: String;
begin
  try
    PrepareUpdate(ATableName, AFieldName, AIDFieldName, S);
    QSetQuery(FQuery);
    QSetSQL(S);
    QParamInt64('IDValue', AIDValue);
    QParamStr('NewValue', ANewValue);
    QExec;
    QCommit;
  except
    QRollback;
  end;
end;

procedure PrepareDelete(const ATableName, AIDFieldName: String;
                        out ASQL: String);
begin
  ASQL:=
    'DELETE FROM' + SqlEsc(ATableName) +
    'WHERE' + SqlEsc(AIDFieldName) + '= :IDValue';
end;

procedure TSQLite3.Delete(const ATableName, AIDFieldName: String;
                          const AIDValue: Integer);
var
  S: String;
begin
  PrepareDelete(ATableName, AIDFieldName, S);
  QSetQuery(FQuery);
  try
    QSetSQL(S);
    QParamInt('IDValue', AIDValue);
    QExec;
    QCommit;
  except
    QRollback;
  end;
end;

procedure TSQLite3.Delete(const ATableName, AIDFieldName: String;
                          const AIDValue: Int64);
var
  S: String;
begin
  PrepareDelete(ATableName, AIDFieldName, S);
  QSetQuery(FQuery);
  try
    QSetSQL(S);
    QParamInt64('IDValue', AIDValue);
    QExec;
    QCommit;
  except
    QRollback;
  end;
end;

procedure TSQLite3.Delete(const ATableName, AIDFieldName: String;
                          const AIDValue: TDateTime);
var
  S: String;
begin
  PrepareDelete(ATableName, AIDFieldName, S);
  QSetQuery(FQuery);
  try
    QSetSQL(S);
    QParamDT('IDValue', AIDValue);
    QExec;
    QCommit;
  except
    QRollback;
  end;
end;

procedure PrepareIsValueInTable(const ATableName, AFieldName: String;
                          out ASQL: String);
var
  FieldName: String;
begin
  FieldName:= SqlEsc(AFieldName);
  ASQL:=
    'SELECT' + FieldName +
    'FROM'   + SqlEsc(ATableName) +
    'WHERE'  + FieldName + '= :Value';
end;

function TSQLite3.IsValueInTable(const ATableName, AFieldName: String;
  const AValue: Integer): Boolean;
var
  S: String;
begin
  PrepareIsValueInTable(ATableName, AFieldName, S);
  QSetQuery(FQuery);
  QSetSQL(S);
  QParamInt('Value', AValue);
  QOpen;
  Result:= not QIsEmpty;
  QClose;
end;

function TSQLite3.IsValueInTable(const ATableName, AFieldName: String;
  const AValue: Int64): Boolean;
var
  S: String;
begin
  PrepareIsValueInTable(ATableName, AFieldName, S);
  QSetQuery(FQuery);
  QSetSQL(S);
  QParamInt64('Value', AValue);
  QOpen;
  Result:= not QIsEmpty;
  QClose;
end;

function TSQLite3.IsValueInTable(const ATableName, AFieldName: String;
  const AValue: TDateTime): Boolean;
var
  S: String;
begin
  PrepareIsValueInTable(ATableName, AFieldName, S);
  QSetQuery(FQuery);
  QSetSQL(S);
  QParamDT('Value', AValue);
  QOpen;
  Result:= not QIsEmpty;
  QClose;
end;

function TSQLite3.IsValueInTable(const ATableName, AFieldName: String;
  const AValue: String; const ACaseSensitivity: Boolean = True): Boolean;
var
  FieldName, WhereStr, Value: String;
begin
  FieldName:= SqlEsc(AFieldName);

  if ACaseSensitivity then
  begin
    WhereStr:= 'WHERE'  + FieldName + '= :Value';
    Value:= AValue;
  end
  else begin
    WhereStr:= 'WHERE UPPER('  + FieldName + ') = :Value';
    Value:= SUpper(AValue);
  end;

  QSetQuery(FQuery);
  QSetSQL(
    'SELECT' + FieldName +
    'FROM'   + SqlEsc(ATableName) +
    WhereStr
    );
  QParamStr('Value', Value);
  QOpen;
  Result:= not QIsEmpty;
  QClose;
end;

procedure PrepareIsValueInTableNotMatchStr(
  const ATableName, AFieldName, AIDFieldName, AValue: String;
  const ACaseSensitivity: Boolean;
  out ASQL, AOutValue: String);
var
  FieldName, WhereStr: String;
begin
  FieldName:= SqlEsc(AFieldName);

  WhereStr:= '(' + SqlEsc(AIDFieldName) + '<> :IDValue) AND (';

  if ACaseSensitivity then
  begin
    WhereStr:= WhereStr + FieldName + '= :Value)';
    AOutValue:= AValue;
  end
  else begin
    WhereStr:= WhereStr + 'UPPER('  + FieldName + ') = :Value)';
    AOutValue:= SUpper(AValue);
  end;

  ASQL:=
    'SELECT' + FieldName +
    'FROM'   + SqlEsc(ATableName) +
    WhereStr;
end;

function TSQLite3.IsValueInTableNotMatchInt32ID(const ATableName,
  AFieldName: String; const AValue: String; const AIDFieldName: String;
  const AIDValue: Integer; const ACaseSensitivity: Boolean): Boolean;
var
  S, Value: String;
begin
  PrepareIsValueInTableNotMatchStr(ATableName, AFieldName, AIDFieldName,
                                   AValue, ACaseSensitivity, S, Value);
  QSetQuery(FQuery);
  QSetSQL(S);
  QParamInt('IDValue', AIDValue);
  QParamStr('Value', Value);
  QOpen;
  Result:= not QIsEmpty;
  QClose;
end;

function TSQLite3.IsValueInTableNotMatchInt64ID(const ATableName,
  AFieldName: String; const AValue: String; const AIDFieldName: String;
  const AIDValue: Int64; const ACaseSensitivity: Boolean): Boolean;
var
  S, Value: String;
begin
  PrepareIsValueInTableNotMatchStr(ATableName, AFieldName, AIDFieldName,
                                   AValue, ACaseSensitivity, S, Value);
  QSetQuery(FQuery);
  QSetSQL(S);
  QParamInt64('IDValue', AIDValue);
  QParamStr('Value', Value);
  QOpen;
  Result:= not QIsEmpty;
  QClose;
end;

procedure PrepareIsValueInTableNotMatch(const ATableName, AFieldName, AIDFieldName: String;
  out ASQL: String);
var
  FieldName: String;
begin
  FieldName:= SqlEsc(AFieldName);
  ASQL:=
    'SELECT' + FieldName +
    'FROM'   + SqlEsc(ATableName) +
    'WHERE (' + FieldName + '= :Value) AND (' + SqlEsc(AIDFieldName) + '<> :IDValue)';
end;

function TSQLite3.IsValueInTableNotMatchInt64ID(const ATableName,
  AFieldName: String; const AValue: Integer; const AIDFieldName: String;
  const AIDValue: Int64): Boolean;
var
  S: String;
begin
  PrepareIsValueInTableNotMatch(ATableName, AFieldName, AIDFieldName, S);
  QSetQuery(FQuery);
  QSetSQL(S);
  QParamInt64('IDValue', AIDValue);
  QParamInt('Value', AValue);
  QOpen;
  Result:= not QIsEmpty;
  QClose;
end;

function TSQLite3.IsValueInTableNotMatchInt64ID(const ATableName,
  AFieldName: String; const AValue: Int64; const AIDFieldName: String;
  const AIDValue: Int64): Boolean;
var
  S: String;
begin
  PrepareIsValueInTableNotMatch(ATableName, AFieldName, AIDFieldName, S);
  QSetQuery(FQuery);
  QSetSQL(S);
  QParamInt64('IDValue', AIDValue);
  QParamInt64('Value', AValue);
  QOpen;
  Result:= not QIsEmpty;
  QClose;
end;

function TSQLite3.IsValueInTableNotMatchInt64ID(const ATableName,
  AFieldName: String; const AValue: TDateTime; const AIDFieldName: String;
  const AIDValue: Int64): Boolean;
var
  S: String;
begin
  PrepareIsValueInTableNotMatch(ATableName, AFieldName, AIDFieldName, S);
  QSetQuery(FQuery);
  QSetSQL(S);
  QParamInt64('IDValue', AIDValue);
  QParamDT('Value', AValue);
  QOpen;
  Result:= not QIsEmpty;
  QClose;
end;

function TSQLite3.IsValueInTableNotMatchInt32ID(const ATableName,
  AFieldName: String; const AValue: Integer; const AIDFieldName: String;
  const AIDValue: Integer): Boolean;
var
  S: String;
begin
  PrepareIsValueInTableNotMatch(ATableName, AFieldName, AIDFieldName, S);
  QSetQuery(FQuery);
  QSetSQL(S);
  QParamInt('IDValue', AIDValue);
  QParamInt('Value', AValue);
  QOpen;
  Result:= not QIsEmpty;
  QClose;
end;

function TSQLite3.IsValueInTableNotMatchInt32ID(const ATableName,
  AFieldName: String; const AValue: Int64; const AIDFieldName: String;
  const AIDValue: Integer): Boolean;
var
  S: String;
begin
  PrepareIsValueInTableNotMatch(ATableName, AFieldName, AIDFieldName, S);
  QSetQuery(FQuery);
  QSetSQL(S);
  QParamInt('IDValue', AIDValue);
  QParamInt64('Value', AValue);
  QOpen;
  Result:= not QIsEmpty;
  QClose;
end;

function TSQLite3.IsValueInTableNotMatchInt32ID(const ATableName,
  AFieldName: String; const AValue: TDateTime; const AIDFieldName: String;
  const AIDValue: Integer): Boolean;
var
  S: String;
begin
  PrepareIsValueInTableNotMatch(ATableName, AFieldName, AIDFieldName, S);
  QSetQuery(FQuery);
  QSetSQL(S);
  QParamInt('IDValue', AIDValue);
  QParamDT('Value', AValue);
  QOpen;
  Result:= not QIsEmpty;
  QClose;
end;

function TSQLite3.LastWritedInt32ID(const ATableName: String): Integer;
begin
  Result:= 0;
  QSetQuery(FQuery);
  QSetSQL(
    'SELECT last_insert_rowid() AS LastID ' +
    'FROM' + SqlEsc(ATableName) +
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
    'FROM' + SqlEsc(ATableName) +
    'LIMIT 1'
    );
  QOpen;
  if not QIsEmpty then
    Result:= QFieldInt64('LastID');
  QClose;
end;

function TSQLite3.LastWritedInt32Value(const ATableName, AFieldName: String): Integer;
begin
  Result:= 0;
  QSetQuery(FQuery);
  QSetSQL(
    'SELECT' + SqlEsc(AFieldName) +
    'FROM'   + SqlEsc(ATableName) +
    'ORDER BY RowID DESC '+
    'LIMIT 1'
    );
  QOpen;
  if not QIsEmpty then
    Result:= QFieldInt(AFieldName);
  QClose;
end;

function TSQLite3.LastWritedInt64Value(const ATableName, AFieldName: String): Int64;
begin
  Result:= 0;
  QSetQuery(FQuery);
  QSetSQL(
    'SELECT' + SqlEsc(AFieldName) +
    'FROM'   + SqlEsc(ATableName) +
    'ORDER BY RowID DESC '+
    'LIMIT 1'
    );
  QOpen;
  if not QIsEmpty then
    Result:= QFieldInt64(AFieldName);
  QClose;
end;

function TSQLite3.LastWritedStringValue(const ATableName, AFieldName: String): String;
begin
  Result:= EmptyStr;
  QSetQuery(FQuery);
  QSetSQL(
    'SELECT' + SqlEsc(AFieldName) +
    'FROM'   + SqlEsc(ATableName) +
    'ORDER BY RowID DESC '+
    'LIMIT 1'
    );
  QOpen;
  if not QIsEmpty then
    Result:= QFieldStr(AFieldName);
  QClose;
end;

function TSQLite3.LastWritedDateTimeValue(const ATableName, AFieldName: String): TDateTime;
begin
  Result:= 0;
  QSetQuery(FQuery);
  QSetSQL(
    'SELECT' + SqlEsc(AFieldName) +
    'FROM'   + SqlEsc(ATableName) +
    'ORDER BY RowID DESC '+
    'LIMIT 1'
    );
  QOpen;
  if not QIsEmpty then
    Result:= QFieldDT(AFieldName);
  QClose;
end;

procedure TSQLite3.KeyPickList(const ATableName, AKeyFieldName, APickFieldName: String;
                          out AKeyVector: TIntVector;
                          out APickVector: TStrVector;
                          const AKeyValueNotZero: Boolean = False;
                          const AOrderFieldName: String = '');
var
  QueryStr, KeyField, PickField, OrderField: String;
begin
  AKeyVector:= nil;
  APickVector:= nil;

  KeyField:= SqlEsc(AKeyFieldName);
  PickField:= SqlEsc(APickFieldName);

  if AOrderFieldName=EmptyStr then
    OrderField:=  PickField
  else
    OrderField:= SqlEsc(AOrderFieldName);

  QueryStr:=
    'SELECT' + KeyField + ',' + PickField +
    'FROM'   + SqlEsc(ATableName);
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
      VAppend(AKeyVector, QFieldInt(AKeyFieldName));
      VAppend(APickVector, QFieldStr(APickFieldName));
      QNext;
    end;
  end;
  QClose;
end;

procedure TSQLite3.KeyPickList(const ATableName, AKeyFieldName, APickFieldName: String;
                          out AKeyList, APickList: TStringList;
                          const AKeyValueNotZero: Boolean = False;
                          const AOrderFieldName: String = '');
var
  VKey: TIntVector;
  VPick: TStrVector;
  i: Integer;
begin
  {%H-}APickList.Clear;
  {%H-}AKeyList.Clear;
  KeyPickList(ATableName, AKeyFieldName, APickFieldName,
                 VKey, VPick, AKeyValueNotZero, AOrderFieldName);
  for i:= 0 to High(VKey) do
  begin
    AKeyList.Add(IntToStr(VKey[i]));
    APickList.Add(VPick[i]);
  end;
end;

end.

