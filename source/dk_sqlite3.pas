unit DK_SQLite3;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics, SQLite3Conn, SQLDB, Controls, StdCtrls, VirtualTrees,
  DK_SQLUtils, DK_Vector, DK_Matrix, DK_StrUtils, DK_DBUtils, DK_VSTTables;

type

  { TSQLite3Connection }

  TSQLite3Connection = class(SQLite3Conn.TSQLite3Connection)
  protected
    procedure DoInternalConnect; override;
  end;

  { TSQLite3 }

  TSQLite3 = class
  protected
    FConnection: TSQLite3Connection;
    FTransaction: TSQLTransaction;
    FQuery: TSQLQuery;
  public
    constructor Create;
    destructor  Destroy; override;

    procedure Reconnect;
    procedure Connect(const AFileName: String);
    procedure ExecuteScript(const AFileName: String);

    function EditList(const AFormCaption: String;
                   const ATableName, AIDFieldName, AFieldName: String;
                   const AOrderByName: Boolean = False;
                   const AIDNotZero: Boolean = False;
                   const AColumnWidth: Integer = 400;
                   const AFont: TFont = nil): Boolean;
    function EditCheckList(var AKeyValues: TIntVector;
                   var APickValues: TStrVector;  out AIsAllChecked: Boolean;
                   const ACaption, ATableName, AKeyFieldName, APickFieldName: String;
                   const AOrderByName: Boolean = False;
                   const AKeyNotZero: Boolean = False;
                   const AShowHeader: Boolean = True): Boolean;
    function EditTable(const AFormCaption: String;
                       const ATableName, AIDFieldName: String;
                       const AFieldNames, AColumnNames: TStrVector;
                       const AColumnTypes: TVSTColumnTypes;
                       const AColumnNeedValues: TBoolVector;
                       const AColumnWidths: TIntVector;
                       const AColumnAlignments: array of TAlignment;
                       const AIDNotZero: Boolean = False;
                       const AOrderFieldNames: TStrVector = nil;
                       const AAutoSizeColumnNumber: Integer = 1;
                       const AKeys: TIntMatrix = nil;
                       const APicks: TStrMatrix = nil;
                       const AFont: TFont = nil): Boolean;
    function EditDoubleTable(const AFormCaption: String;
                       const ALeftTableName, ALeftIDFieldName: String;
                       const ALeftFieldNames, ALeftColumnNames: TStrVector;
                       const ALeftColumnTypes: TVSTColumnTypes;
                       const ALeftColumnNeedValues: TBoolVector;
                       const ALeftColumnWidths: TIntVector;
                       const ALeftColumnAlignments: array of TAlignment;
                       const ALeftIDNotZero: Boolean;
                       const ALeftOrderFieldNames: TStrVector;
                       const ALeftAutoSizeColumnNumber: Integer;
                       const ALeftKeys: TIntMatrix;
                       const ALeftPicks: TStrMatrix;

                       const ARightTableName, ARightIDFieldName: String;
                       const ARightFieldNames, ARightColumnNames: TStrVector;
                       const ARightColumnTypes: TVSTColumnTypes;
                       const ARightColumnNeedValues: TBoolVector;
                       const ARightColumnWidths: TIntVector;
                       const ARightColumnAlignments: array of TAlignment;
                       const ARightIDNotZero: Boolean;
                       const ARightOrderFieldNames: TStrVector;
                       const ARightAutoSizeColumnNumber: Integer;
                       const ARightKeys: TIntMatrix;
                       const ARightPicks: TStrMatrix;
                       const AMasterIDFieldName: String;

                       const AFont: TFont = nil): Boolean;

    function ValueStrStrID(const ATableName, AValueFieldName,
                                 AIDFieldName, AIDValue: String): String;
    function ValueInt32Int32ID(const ATableName, AValueFieldName, AIDFieldName: String;
                               const AIDValue: Integer): Integer;
    function ValueInt64Int32ID(const ATableName, AValueFieldName, AIDFieldName: String;
                               const AIDValue: Integer): Int64;
    function ValueDTInt32ID(const ATableName, AValueFieldName, AIDFieldName: String;
                            const AIDValue: Integer): TDateTime;
    function ValueStrInt32ID(const ATableName, AValueFieldName, AIDFieldName: String;
                             const AIDValue: Integer): String;
    function ValueInt32Int64ID(const ATableName, AValueFieldName, AIDFieldName: String;
                               const AIDValue: Int64): Integer;
    function ValueInt64Int64ID(const ATableName, AValueFieldName, AIDFieldName: String;
                               const AIDValue: Int64): Int64;
    function ValueDTInt64ID(const ATableName, AValueFieldName, AIDFieldName: String;
                            const AIDValue: Int64): TDateTime;
    function ValueStrInt64ID(const ATableName, AValueFieldName, AIDFieldName: String;
                             const AIDValue: Int64): String;



    function ValuesInt32Int32ID(const ATableName, AValueFieldName, AIDFieldName: String;
                                const AIDValue: Integer; const AUnique: Boolean = False): TIntVector;

    function ValuesInt32Int32ID(const ATableName, AValueFieldName, AIDFieldName: String;
                                const AIDValues: TIntVector; const AUnique: Boolean = False): TIntVector;



    procedure Delete(const ATableName, AIDFieldName: String;
                     const AIDValue: Integer; const ACommit: Boolean = True);
    procedure Delete(const ATableName, AIDFieldName: String;
                     const AIDValue: Int64; const ACommit: Boolean = True);
    procedure Delete(const ATableName, AIDFieldName: String;
                     const AIDValue: TDateTime; const ACommit: Boolean = True);
    procedure Delete(const ATableName, AIDFieldName: String;
                     const AIDValue: String;
                     const ACaseSensitivity: Boolean = True;
                     const ACommit: Boolean = True);

    procedure UpdateStrID(const ATableName, AFieldName, AIDFieldName, AIDValue: String;
                     const ANewValue: String; const ACommit: Boolean = True);
    procedure UpdateInt32ID(const ATableName, AFieldName, AIDFieldName: String;
                     const AIDValue: Integer;
                     const ANewValue: Integer; const ACommit: Boolean = True);
    procedure UpdateInt32ID(const ATableName, AFieldName, AIDFieldName: String;
                     const AIDValue: Integer;
                     const ANewValue: Int64; const ACommit: Boolean = True);
    procedure UpdateInt32ID(const ATableName, AFieldName, AIDFieldName: String;
                     const AIDValue: Integer;
                     const ANewValue: TDateTime; const ACommit: Boolean = True);
    procedure UpdateInt32ID(const ATableName, AFieldName, AIDFieldName: String;
                     const AIDValue: Integer;
                     const ANewValue: String; const ACommit: Boolean = True);

    procedure UpdateInt64ID(const ATableName, AFieldName, AIDFieldName: String;
                     const AIDValue: Int64;
                     const ANewValue: Integer; const ACommit: Boolean = True);
    procedure UpdateInt64ID(const ATableName, AFieldName, AIDFieldName: String;
                     const AIDValue: Int64;
                     const ANewValue: Int64; const ACommit: Boolean = True);
    procedure UpdateInt64ID(const ATableName, AFieldName, AIDFieldName: String;
                     const AIDValue: Int64;
                     const ANewValue: TDateTime; const ACommit: Boolean = True);
    procedure UpdateInt64ID(const ATableName, AFieldName, AIDFieldName: String;
                     const AIDValue: Int64;
                     const ANewValue: String; const ACommit: Boolean = True);

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

    function MaxInt32ID(const ATableName: String): Integer;
    function MaxInt64ID(const ATableName: String): Int64;

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


    procedure LoadIDsAndNames(AComboBox: TComboBox; out ANameIDs: TIntVector;
            const ATableName, AKeyFieldName, APickFieldName, AOrderFieldName: String;
            const AKeyValueNotZero: Boolean; const AZeroKeyPick: String = '');

    procedure ValueFromCatalog(const AThisTableName, ACatalogTableName,
                               AThisTableIDFieldName, ASearchIDFieldName, ACatalogValueFieldName: String;
                               const AThisTableID: Integer;
                               out ACatalogID: Integer; out ACatalogValue: String);

    function LoadIDsAndNamesSelected(ALabel: TLabel; const ANeedEdit: Boolean;
       var AKeyValues: TIntVector; var APickValues: TStrVector;
       const ACaption, ATableName, AKeyFieldName, APickFieldName, AOrderFieldName: String;
       const AKeyValueNotZero: Boolean; const AAllKeyPick: String = '';
       const AShowHeader: Boolean = True): Boolean;

    property Connection: TSQLite3Connection read FConnection;
    property Transaction: TSQLTransaction read FTransaction;

  end;



implementation

uses USQLite3CheckList, USQLite3Table, USQLite3DoubleTable;

{ TSQLite3Connection }

procedure TSQLite3Connection.DoInternalConnect;
begin
  inherited DoInternalConnect;
  ExecSQL('PRAGMA journal_mode=WAL');
end;

{ TSQLite3 }

constructor TSQLite3.Create;
begin
  FConnection:= TSQLite3Connection.Create(nil);
  FTransaction:= TSQLTransaction.Create(nil);
  FQuery:= TSQLQuery.Create(nil);

  FConnection.CharSet:= 'UTF8';
  FConnection.Params.Add('foreign_keys=ON');
  FConnection.Transaction:= FTransaction;
  FConnection.OpenFlags:= FConnection.OpenFlags + [sofCreate, sofReadWrite];
  FQuery.SQLConnection:= FConnection;
  FQuery.Transaction:= FTransaction;
end;

destructor TSQLite3.Destroy;
begin
  FreeAndNil(FQuery);
  FreeAndNil(FTransaction);
  FreeAndNil(FConnection);
  inherited Destroy;
end;

procedure TSQLite3.Reconnect;
begin
  FConnection.Close(True);
  FConnection.Open;
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
  if not FileExists(AFileName) then Exit;
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

function TSQLite3.EditList(const AFormCaption: String;
                           const ATableName, AIDFieldName, AFieldName: String;
                           const AOrderByName: Boolean;
                           const AIDNotZero: Boolean;
                           const AColumnWidth: Integer = 400;
                           const AFont: TFont = nil): Boolean;
var
  Frm: TSQLite3Table;
  OrderFieldNames: TStrVector;
begin
  Result:= False;
  OrderFieldNames:= nil;
  if AOrderByName then VAppend(OrderFieldNames, AFieldName);
  Frm:= TSQLite3Table.Create(nil);
  try
    Frm.Caption:= AFormCaption;
    Frm.Query.DataBase:= FConnection;
    Frm.Query.Transaction:= FTransaction;
    Frm.SetTable(AFont, ATableName, AIDFieldName,
                 [AFieldName],
                 nil,
                 [ctString],
                 [True],
                 [AColumnWidth],
                 [taLeftJustify],
                 AIDNotZero, False, OrderFieldNames);
    Frm.ShowModal;
    Result:= True;
  finally
    FreeAndNil(Frm);
  end;
end;

function TSQLite3.EditCheckList(var AKeyValues: TIntVector;
  var APickValues: TStrVector; out AIsAllChecked: Boolean;
  const ACaption, ATableName, AKeyFieldName, APickFieldName: String;
  const AOrderByName: Boolean = False;
  const AKeyNotZero: Boolean = False;
  const AShowHeader: Boolean = True): Boolean;
var
  Frm: TSQLite3CheckList;
  VKey: TIntVector;
  VPick: TStrVector;
  S: String;
begin
  Result:= False;
  Frm:= TSQLite3CheckList.Create(nil);

  try
    Frm.Caption:= ACaption;
    Frm.ListQuery.DataBase:= FConnection;
    Frm.ListQuery.Transaction:= FTransaction;
    S:= AKeyFieldName;
    if AOrderByName then
      S:= APickFieldName;
    KeyPickList(ATableName, AKeyFieldName, APickFieldName,
                VKey, VPick, AKeyNotZero, S);
    Frm.KeyValues:= VKey;
    Frm.PickValues:= VPick;
    if not AShowHeader then
      Frm.VT1.Header.Options:= Frm.VT1.Header.Options - [hoVisible];

    Frm.OutKeyValues:= AKeyValues;
    Frm.OutPickValues:= APickValues;

    if Frm.ShowModal=mrOK then
    begin
      Result:= True;
      AKeyValues:= Frm.OutKeyValues;
      APickValues:= Frm.OutPickValues;
      AIsAllChecked:= Frm.IsAllChecked;
    end;

  finally
    FreeAndNil(Frm);
  end;
end;

function TSQLite3.EditTable(const AFormCaption: String;
                       const ATableName, AIDFieldName: String;
                       const AFieldNames, AColumnNames: TStrVector;
                       const AColumnTypes: TVSTColumnTypes;
                       const AColumnNeedValues: TBoolVector;
                       const AColumnWidths: TIntVector;
                       const AColumnAlignments: array of TAlignment;
                       const AIDNotZero: Boolean = False;
                       const AOrderFieldNames: TStrVector = nil;
                       const AAutoSizeColumnNumber: Integer = 1;
                       const AKeys: TIntMatrix = nil;
                       const APicks: TStrMatrix = nil;
                       const AFont: TFont = nil): Boolean;
var
  Frm: TSQLite3Table;
begin
  Result:= False;

  Frm:= TSQLite3Table.Create(nil);
  try
    Frm.Caption:= AFormCaption;
    Frm.Query.DataBase:= FConnection;
    Frm.Query.Transaction:= FTransaction;
    Frm.SetTable(AFont, ATableName, AIDFieldName, AFieldNames,
                 AColumnNames, AColumnTypes, AColumnNeedValues, AColumnWidths, AColumnAlignments,
                 AIDNotZero, not VIsNil(AColumnNames), AOrderFieldNames, AAutoSizeColumnNumber, AKeys, APicks);
    Frm.ShowModal;
    Result:= True;
  finally
    FreeAndNil(Frm);
  end;
end;

function TSQLite3.EditDoubleTable(const AFormCaption: String;
                       const ALeftTableName, ALeftIDFieldName: String;
                       const ALeftFieldNames, ALeftColumnNames: TStrVector;
                       const ALeftColumnTypes: TVSTColumnTypes;
                       const ALeftColumnNeedValues: TBoolVector;
                       const ALeftColumnWidths: TIntVector;
                       const ALeftColumnAlignments: array of TAlignment;
                       const ALeftIDNotZero: Boolean;
                       const ALeftOrderFieldNames: TStrVector;
                       const ALeftAutoSizeColumnNumber: Integer;
                       const ALeftKeys: TIntMatrix;
                       const ALeftPicks: TStrMatrix;

                       const ARightTableName, ARightIDFieldName: String;
                       const ARightFieldNames, ARightColumnNames: TStrVector;
                       const ARightColumnTypes: TVSTColumnTypes;
                       const ARightColumnNeedValues: TBoolVector;
                       const ARightColumnWidths: TIntVector;
                       const ARightColumnAlignments: array of TAlignment;
                       const ARightIDNotZero: Boolean;
                       const ARightOrderFieldNames: TStrVector;
                       const ARightAutoSizeColumnNumber: Integer;
                       const ARightKeys: TIntMatrix;
                       const ARightPicks: TStrMatrix;
                       const AMasterIDFieldName: String;

                       const AFont: TFont = nil): Boolean;
var
  Frm: TSQLite3DoubleTable;
begin
  Result:= False;

  Frm:= TSQLite3DoubleTable.Create(nil);
  try
    Frm.Caption:= AFormCaption;
    Frm.LeftQuery.DataBase:= FConnection;
    Frm.LeftQuery.Transaction:= FTransaction;
    Frm.RightQuery.DataBase:= FConnection;
    Frm.RightQuery.Transaction:= FTransaction;
    Frm.SetRightTable(AFont, ARightTableName, ARightIDFieldName, ARightFieldNames,
                 ARightColumnNames, ARightColumnTypes, ARightColumnNeedValues, ARightColumnWidths, ARightColumnAlignments,
                 ARightIDNotZero, not VIsNil(ARightColumnNames), ARightOrderFieldNames,
                 ARightAutoSizeColumnNumber, ARightKeys, ARightPicks, AMasterIDFieldName);
    Frm.SetLeftTable(AFont, ALeftTableName, ALeftIDFieldName, ALeftFieldNames,
                 ALeftColumnNames, ALeftColumnTypes, ALeftColumnNeedValues, ALeftColumnWidths, ALeftColumnAlignments,
                 ALeftIDNotZero, not VIsNil(ALeftColumnNames), ALeftOrderFieldNames,
                 ALeftAutoSizeColumnNumber, ALeftKeys, ALeftPicks);

    Frm.ShowModal;
    Result:= True;
  finally
    FreeAndNil(Frm);
  end;
end;

procedure PrepareValue(const ATableName, AValueFieldName, AIDFieldName: String;
                       out ASQL: String; const AUnique: Boolean = False);
begin
  ASQL:= 'SELECT';
  if AUnique then
    ASQL:= ASQL + ' DISTINCT';
  ASQL:= ASQL     + SqlEsc(AValueFieldName) +
         'FROM'   + SqlEsc(ATableName) +
         'WHERE'  + SqlEsc(AIDFieldName) + '= :IDValue';
end;

function TSQLite3.ValueStrStrID(const ATableName, AValueFieldName,
  AIDFieldName, AIDValue: String): String;
var
  S: String;
begin
  Result:= EmptyStr;
  PrepareValue(ATableName, AValueFieldName, AIDFieldName, S);
  QSetQuery(FQuery);
  QSetSQL(S);
  QParamStr('IDValue', AIDValue);
  QOpen;
  if not QIsEmpty then
  begin
    QFirst;
    Result:= QFieldStr(AValueFieldName);
  end;
  QClose;
end;

function TSQLite3.ValueInt32Int32ID(const ATableName, AValueFieldName,
  AIDFieldName: String; const AIDValue: Integer): Integer;
var
  S: String;
begin
  Result:= 0;
  PrepareValue(ATableName, AValueFieldName, AIDFieldName, S);
  QSetQuery(FQuery);
  QSetSQL(S);
  QParamInt('IDValue', AIDValue);
  QOpen;
  if not QIsEmpty then
  begin
    QFirst;
    Result:= QFieldInt(AValueFieldName);
  end;
  QClose;
end;

function TSQLite3.ValueInt64Int32ID(const ATableName, AValueFieldName,
  AIDFieldName: String; const AIDValue: Integer): Int64;
var
  S: String;
begin
  Result:= 0;
  PrepareValue(ATableName, AValueFieldName, AIDFieldName, S);
  QSetQuery(FQuery);
  QSetSQL(S);
  QParamInt('IDValue', AIDValue);
  QOpen;
  if not QIsEmpty then
  begin
    QFirst;
    Result:= QFieldInt64(AValueFieldName);
  end;
  QClose;
end;

function TSQLite3.ValueDTInt32ID(const ATableName, AValueFieldName,
  AIDFieldName: String; const AIDValue: Integer): TDateTime;
var
  S: String;
begin
  Result:= 0;
  PrepareValue(ATableName, AValueFieldName, AIDFieldName, S);
  QSetQuery(FQuery);
  QSetSQL(S);
  QParamInt('IDValue', AIDValue);
  QOpen;
  if not QIsEmpty then
  begin
    QFirst;
    Result:= QFieldDT(AValueFieldName);
  end;
  QClose;
end;

function TSQLite3.ValueStrInt32ID(const ATableName, AValueFieldName,
  AIDFieldName: String; const AIDValue: Integer): String;
var
  S: String;
begin
  Result:= EmptyStr;
  PrepareValue(ATableName, AValueFieldName, AIDFieldName, S);
  QSetQuery(FQuery);
  QSetSQL(S);
  QParamInt('IDValue', AIDValue);
  QOpen;
  if not QIsEmpty then
  begin
    QFirst;
    Result:= QFieldStr(AValueFieldName);
  end;
  QClose;
end;

function TSQLite3.ValueInt32Int64ID(const ATableName, AValueFieldName,
  AIDFieldName: String; const AIDValue: Int64): Integer;
var
  S: String;
begin
  Result:= 0;
  PrepareValue(ATableName, AValueFieldName, AIDFieldName, S);
  QSetQuery(FQuery);
  QSetSQL(S);
  QParamInt64('IDValue', AIDValue);
  QOpen;
  if not QIsEmpty then
  begin
    QFirst;
    Result:= QFieldInt(AValueFieldName);
  end;
  QClose;
end;

function TSQLite3.ValueInt64Int64ID(const ATableName, AValueFieldName,
  AIDFieldName: String; const AIDValue: Int64): Int64;
var
  S: String;
begin
  Result:= 0;
  PrepareValue(ATableName, AValueFieldName, AIDFieldName, S);
  QSetQuery(FQuery);
  QSetSQL(S);
  QParamInt64('IDValue', AIDValue);
  QOpen;
  if not QIsEmpty then
  begin
    QFirst;
    Result:= QFieldInt64(AValueFieldName);
  end;
  QClose;
end;

function TSQLite3.ValueDTInt64ID(const ATableName, AValueFieldName,
  AIDFieldName: String; const AIDValue: Int64): TDateTime;
var
  S: String;
begin
  Result:= 0;
  PrepareValue(ATableName, AValueFieldName, AIDFieldName, S);
  QSetQuery(FQuery);
  QSetSQL(S);
  QParamInt64('IDValue', AIDValue);
  QOpen;
  if not QIsEmpty then
  begin
    QFirst;
    Result:= QFieldDT(AValueFieldName);
  end;
  QClose;
end;

function TSQLite3.ValueStrInt64ID(const ATableName, AValueFieldName,
  AIDFieldName: String; const AIDValue: Int64): String;
var
  S: String;
begin
  Result:= EmptyStr;
  PrepareValue(ATableName, AValueFieldName, AIDFieldName, S);
  QSetQuery(FQuery);
  QSetSQL(S);
  QParamInt64('IDValue', AIDValue);
  QOpen;
  if not QIsEmpty then
  begin
    QFirst;
    Result:= QFieldStr(AValueFieldName);
  end;
  QClose;
end;

function TSQLite3.ValuesInt32Int32ID(const ATableName, AValueFieldName, AIDFieldName: String;
                                     const AIDValue: Integer;
                                     const AUnique: Boolean = False): TIntVector;
var
  S: String;
begin
  Result:= nil;
  PrepareValue(ATableName, AValueFieldName, AIDFieldName, S, AUnique);
  QSetQuery(FQuery);
  QSetSQL(S);
  QParamInt('IDValue', AIDValue);
  QOpen;
  if not QIsEmpty then
  begin
    QFirst;
    while not QEOF do
    begin
      VAppend(Result, QFieldInt(AValueFieldName));
      QNext;
    end;
  end;
  QClose;
end;

procedure PrepareValues(const ATableName, AValueFieldName, AIDFieldName: String;
                        const AIDValuesCount: Integer;
                        out ASQL: String; const AUnique: Boolean = False);
begin
  ASQL:= 'SELECT';
  if AUnique then
    ASQL:= ASQL + ' DISTINCT';
  ASQL:= ASQL     + SqlEsc(AValueFieldName) +
         'FROM'   + SqlEsc(ATableName) +
         'WHERE'  + SqlIN('', AIDFieldName, AIDValuesCount);
end;


function TSQLite3.ValuesInt32Int32ID(const ATableName, AValueFieldName, AIDFieldName: String;
                                const AIDValues: TIntVector; const AUnique: Boolean = False): TIntVector;
var
  S: String;
begin
  Result:= nil;
  if VIsNil(AIDValues) then Exit;
  PrepareValues(ATableName, AValueFieldName, AIDFieldName, Length(AIDValues), S, AUnique);
  QSetQuery(FQuery);
  QSetSQL(S);
  QParamsInt(AIDValues);
  QOpen;
  if not QIsEmpty then
  begin
    QFirst;
    while not QEOF do
    begin
      VAppend(Result, QFieldInt(AValueFieldName));
      QNext;
    end;
  end;
  QClose;
end;

procedure TSQLite3.Delete(const ATableName, AIDFieldName: String;
                          const AIDValue: String;
                          const ACaseSensitivity: Boolean = True;
                          const ACommit: Boolean = True);
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
    if ACommit then QCommit;
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

procedure TSQLite3.UpdateStrID(const ATableName, AFieldName, AIDFieldName,
  AIDValue: String; const ANewValue: String; const ACommit: Boolean = True);
var
  S: String;
begin
  try
    PrepareUpdate(ATableName, AFieldName, AIDFieldName, S);
    QSetQuery(FQuery);
    QSetSQL(S);
    QParamStr('IDValue', AIDValue);
    QParamStr('NewValue', ANewValue);
    QExec;
    if ACommit then QCommit;
  except
    QRollback;
  end;
end;

procedure TSQLite3.UpdateInt32ID(const ATableName, AFieldName,
  AIDFieldName: String; const AIDValue: Integer; const ANewValue: Integer;
  const ACommit: Boolean = True);
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
    if ACommit then QCommit;
  except
    QRollback;
  end;
end;

procedure TSQLite3.UpdateInt32ID(const ATableName, AFieldName,
  AIDFieldName: String; const AIDValue: Integer; const ANewValue: Int64;
  const ACommit: Boolean = True);
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
    if ACommit then QCommit;
  except
    QRollback;
  end;
end;

procedure TSQLite3.UpdateInt32ID(const ATableName, AFieldName,
  AIDFieldName: String; const AIDValue: Integer; const ANewValue: TDateTime;
  const ACommit: Boolean = True);
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
    if ACommit then QCommit;
  except
    QRollback;
  end;
end;

procedure TSQLite3.UpdateInt32ID(const ATableName, AFieldName,
  AIDFieldName: String; const AIDValue: Integer; const ANewValue: String; const ACommit: Boolean = True);
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
    if ACommit then QCommit;
  except
    QRollback;
  end;
end;

procedure TSQLite3.UpdateInt64ID(const ATableName, AFieldName,
  AIDFieldName: String; const AIDValue: Int64; const ANewValue: Integer;
  const ACommit: Boolean = True);
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
    if ACommit then QCommit;
  except
    QRollback;
  end;
end;

procedure TSQLite3.UpdateInt64ID(const ATableName, AFieldName,
  AIDFieldName: String; const AIDValue: Int64; const ANewValue: Int64;
  const ACommit: Boolean = True);
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
    if ACommit then QCommit;
  except
    QRollback;
  end;
end;

procedure TSQLite3.UpdateInt64ID(const ATableName, AFieldName,
  AIDFieldName: String; const AIDValue: Int64; const ANewValue: TDateTime;
  const ACommit: Boolean = True);
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
    if ACommit then QCommit;
  except
    QRollback;
  end;
end;

procedure TSQLite3.UpdateInt64ID(const ATableName, AFieldName,
  AIDFieldName: String; const AIDValue: Int64; const ANewValue: String;
  const ACommit: Boolean = True);
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
    if ACommit then QCommit;
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
                          const AIDValue: Integer; const ACommit: Boolean = True);
var
  S: String;
begin
  PrepareDelete(ATableName, AIDFieldName, S);
  QSetQuery(FQuery);
  try
    QSetSQL(S);
    QParamInt('IDValue', AIDValue);
    QExec;
    if ACommit then QCommit;
  except
    QRollback;
  end;
end;

procedure TSQLite3.Delete(const ATableName, AIDFieldName: String;
                          const AIDValue: Int64; const ACommit: Boolean = True);
var
  S: String;
begin
  PrepareDelete(ATableName, AIDFieldName, S);
  QSetQuery(FQuery);
  try
    QSetSQL(S);
    QParamInt64('IDValue', AIDValue);
    QExec;
    if ACommit then QCommit;
  except
    QRollback;
  end;
end;

procedure TSQLite3.Delete(const ATableName, AIDFieldName: String;
                          const AIDValue: TDateTime; const ACommit: Boolean = True);
var
  S: String;
begin
  PrepareDelete(ATableName, AIDFieldName, S);
  QSetQuery(FQuery);
  try
    QSetSQL(S);
    QParamDT('IDValue', AIDValue);
    QExec;
    if ACommit then QCommit;
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

  WhereStr:= 'WHERE (' + SqlEsc(AIDFieldName) + '<> :IDValue) AND (';

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
  Result:= DK_DBUtils.LastWritedInt32ID(FQuery, ATableName);
end;

function TSQLite3.LastWritedInt64ID(const ATableName: String): Int64;
begin
  Result:= DK_DBUtils.LastWritedInt64ID(FQuery, ATableName);
end;

function TSQLite3.MaxInt32ID(const ATableName: String): Integer;
begin
  Result:= DK_DBUtils.MaxInt32ID(FQuery, ATableName);
end;

function TSQLite3.MaxInt64ID(const ATableName: String): Int64;
begin
  Result:= DK_DBUtils.MaxInt64ID(FQuery, ATableName);
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

procedure TSQLite3.LoadIDsAndNames(AComboBox: TComboBox; out ANameIDs: TIntVector;
   const ATableName, AKeyFieldName, APickFieldName, AOrderFieldName: String;
   const AKeyValueNotZero: Boolean; const AZeroKeyPick: String = '');

var
  MN: TStrVector;
  Ind: Integer;
begin
  AComboBox.Items.Clear;
  KeyPickList(ATableName, AKeyFieldName, APickFieldName,
              ANameIDs, MN, AKeyValueNotZero, AOrderFieldName);
  if VIsNil(ANameIDs) then Exit;

  if not AKeyValueNotZero then
  begin
    Ind:= VIndexOf(ANameIDs, 0);
    if Ind>=0 then
      MN[Ind]:= AZeroKeyPick;
  end;

  VToStrings(MN, AComboBox.Items);
  AComboBox.ItemIndex:= 0;
end;

procedure TSQLite3.ValueFromCatalog(const AThisTableName, ACatalogTableName,
                               AThisTableIDFieldName, ASearchIDFieldName, ACatalogValueFieldName: String;
                               const AThisTableID: Integer;
                               out ACatalogID: Integer;  out ACatalogValue: String);
var
  SearchIDFieldName, SearchValueFieldName: String;
begin
  SearchIDFieldName:= SqlEsc(ASearchIDFieldName);
  SearchValueFieldName:= SqlEsc(ACatalogValueFieldName);
  ACatalogValue:= EmptyStr;
  ACatalogID:= 0;
  QSetQuery(FQuery);
  QSetSQL(
    'SELECT ' +
      't1.'+ SearchIDFieldName + ', t2.' + SearchValueFieldName + ' ' +
    'FROM ' + SqlEsc(AThisTableName) + ' t1 ' +
    'INNER JOIN ' + SqlEsc(ACatalogTableName) + ' t2 ON (t1.' + SearchIDFieldName + '=t2.' + SearchIDFieldName + ') ' +
    'WHERE ' +
        't1.' + SqlEsc(AThisTableIDFieldName) + ' = :RecID'
  );
  QParamInt('RecID', AThisTableID);
  QOpen;
  if not QIsEmpty then
  begin
    QFirst;
    ACatalogID:= QFieldInt(ASearchIDFieldName);
    ACatalogValue:= QFieldStr(ACatalogValueFieldName);
  end;
  QClose;
end;

function TSQLite3.LoadIDsAndNamesSelected(ALabel: TLabel;
  const ANeedEdit: Boolean; var AKeyValues: TIntVector;
  var APickValues: TStrVector; const ACaption, ATableName, AKeyFieldName,
  APickFieldName, AOrderFieldName: String; const AKeyValueNotZero: Boolean;
  const AAllKeyPick: String = '';
  const AShowHeader: Boolean = True): Boolean;
var
  IsAllChecked: Boolean;
  S: String;
begin
  S:= EmptyStr;
  if ANeedEdit then
  begin
    Result:= EditCheckList(AKeyValues, APickValues, IsAllChecked,
                    ACaption, ATableName, AKeyFieldName, APickFieldName,
                    AOrderFieldName=APickFieldName, AKeyValueNotZero,
                    AShowHeader);
    if IsAllChecked then
      S:= AAllKeyPick;
  end
  else begin
    KeyPickList(ATableName, AKeyFieldName, APickFieldName,
                AKeyValues, APickValues, AKeyValueNotZero, AOrderFieldName);
    S:= AAllKeyPick;
    Result:= True;
  end;

  if not Result then Exit;

  if S=EmptyStr then
    S:= VVectorToStr(APickValues, ', ');
  ALabel.Caption:= S;
  ALabel.ShowHint:= True;
  ALabel.Hint:= ALabel.Caption;
end;

end.

