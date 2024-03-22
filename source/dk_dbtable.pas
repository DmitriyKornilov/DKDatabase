unit DK_DBTable;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Buttons, VirtualTrees, ExtCtrls, SQLDB, Controls, Graphics,
  SQLite3,

  DK_VSTTables, DK_Vector, DK_Matrix, DK_SQLUtils, DK_StrUtils, DK_Dialogs,
  DK_DBUtils, DK_CtrlUtils,

  UDBImages;

type
  TDBTableSelectEvent = procedure of object;

  { TDBTable }

  TDBTable = class (TObject)
  private
    FButtonAdd: TSpeedButton;
    FButtonDelete: TSpeedButton;
    FButtonEdit: TSpeedButton;
    FButtonSave: TSpeedButton;
    FButtonCancel: TSpeedButton;
    FButtonUpdate: TSpeedButton;
    FQuery: TSQLQuery;
    FToolPanel: TPanel;
    FTree: TVirtualStringTree;
    FDBImages: TDBImages;
    FEdit: TVSTEdit;

    FLastErrorCode: Integer;

    FTableName, FIDFieldName, FReadSQL: String;
    FFieldNames, FColumnNames: TStrVector;
    FColumnWidths: TIntVector;
    FColumnTypes: TVSTColumnTypes;
    FColumnNeedValues: TBoolVector;

    FKeys: TIntMatrix;
    FPicks: TStrMatrix;
    FDataValues: TStrMatrix;
    FIDValues: TInt64Vector;
    FEditingRowIndex: Integer;
    FIsInserting: Boolean;

    FMasterIDFieldName: String;
    FMasterIDFieldValue: String;

    FOnSelect: TDBTableSelectEvent;

    procedure ActionInsert(Sender: TObject);
    procedure ActionDelete(Sender: TObject);
    procedure ActionEdit(Sender: TObject);
    procedure ActionSave(Sender: TObject);
    procedure ActionCancel(Sender: TObject);
    procedure ActionUpdate(Sender: TObject);

    procedure DataLoad;
    procedure DataShow;

    procedure CellSelect;
    procedure EditingBegin;
    function GetIDValue: String;

  public
    constructor Create(const APanel: TPanel; const AQuery: TSQLQuery);
    destructor Destroy; override;
    procedure Settings(const AFont: TFont;
                       const ATableName, AIDFieldName: String;
                       const AFieldNames, AColumnNames: TStrVector;
                       const AColumnTypes: TVSTColumnTypes;
                       const AColumnNeedValues: TBoolVector;
                       const AColumnWidths: TIntVector;
                       const AColumnAlignments: array of TAlignment;
                       const AIDNotZero, AHeaderVisible: Boolean;
                       const AOrderFieldNames: TStrVector = nil;
                       const AAutoSizeColumnNumber: Integer = 1;
                       const AKeys: TIntMatrix = nil;
                       const APicks: TStrMatrix = nil;
                       const AMasterIDFieldName: String = '');
    procedure Update(const AMasterIDFieldValue: String = '');
    property OnSelect: TDBTableSelectEvent read FOnSelect write FOnSelect;
    property IDValue: String read GetIDValue;
    property LastErrorCode: Integer read FLastErrorCode;
    property Edit: TVSTEdit read FEdit;
  end;

implementation

{ TDBTable }

constructor TDBTable.Create(const APanel: TPanel; const AQuery: TSQLQuery);

  procedure ButtonCreate(var AButton: TSpeedButton; const AIconIndex: Integer;
                         const AHint: String);
  begin
    AButton:= TSpeedButton.Create(FToolPanel);
    AButton.Parent:= FToolPanel;
    AButton.Cursor:= crHandPoint;
    AButton.Align:= alLeft;
    AButton.Images:= FDBImages.ToolIcons;
    AButton.ImageIndex:= AIconIndex;
    AButton.AutoSize:= False;
    ControlWidth(AButton, TOOL_BUTTON_WIDTH_DEFAULT);
    AButton.Hint:= AHint;
    AButton.ShowHint:= True;
  end;

begin
  FQuery:= AQuery;

  FLastErrorCode:= SQLITE_OK;

  FDBImages:= TDBImages.Create(nil);

  FToolPanel:= TPanel.Create(APanel);
  FToolPanel.Parent:= APanel;
  FToolPanel.Align:= alTop;
  FToolPanel.AutoSize:= False;
  ControlHeight(FToolPanel, TOOL_PANEL_HEIGHT_DEFAULT);
  FToolPanel.BevelInner:= bvNone;
  FToolPanel.BevelOuter:= bvNone;
  FToolPanel.BorderStyle:= bsSingle;
  FToolPanel.AnchorToNeighbour(akLeft, 2, APanel);
  FToolPanel.AnchorToNeighbour(akTop, 2, APanel);
  FToolPanel.AnchorToNeighbour(akRight, 2, APanel);

  FTree:= TVirtualStringTree.Create(APanel);
  FTree.Parent:= APanel;
  FTree.Align:= alClient;
  FTree.AnchorClient(2);

  ButtonCreate(FButtonUpdate, 5, 'Обновить');
  ButtonCreate(FButtonCancel, 4, 'Отмена');
  ButtonCreate(FButtonSave,   3, 'Сохранить');
  ButtonCreate(FButtonEdit,   2, 'Редактировать');
  ButtonCreate(FButtonDelete, 1, 'Удалить запись');
  ButtonCreate(FButtonAdd,    0, 'Вставить запись');

  FButtonAdd.OnClick:=    @ActionInsert;
  FButtonDelete.OnClick:= @ActionDelete;
  FButtonEdit.OnClick:=   @ActionEdit;
  FButtonSave.OnClick:=   @ActionSave;
  FButtonCancel.OnClick:= @ActionCancel;
  FButtonUpdate.OnClick:= @ActionUpdate;

  FEdit:= TVSTEdit.Create(FTree);
  FEdit.CanUnselect:= False;
  FEdit.IsBeginEditOnKeyPress:= False;
  FEdit.OnSelect:= @CellSelect;
  FEdit.OnEdititingBegin:= @EditingBegin;

  FButtonDelete.Enabled:= False;
  FButtonEdit.Enabled:= False;
  FButtonSave.Enabled:= False;
  FButtonCancel.Enabled:= False;

  FIsInserting:= False;

end;

destructor TDBTable.Destroy;
begin
  if FEdit.IsEditing then
    ActionCancel(nil);

  FreeAndNil(FToolPanel);
  FreeAndNil(FTree);
  FreeAndNil(FDBImages);
  FreeAndNil(FEdit);
end;

procedure TDBTable.ActionInsert(Sender: TObject);
var
  InsIndex: Integer;
begin
  FIsInserting:= True;

  InsIndex:= 0;
  if FEdit.IsSelected then
    InsIndex:= FEdit.SelectedRowIndex;

  MRowIns(FDataValues, InsIndex);
  FEdit.RowInsert(InsIndex);

  FEdit.Select(InsIndex, FColumnNames[0]);
  FEdit.Select(InsIndex, FColumnNames[0]);
end;

procedure TDBTable.ActionDelete(Sender: TObject);
var
  DelIndex: Integer;
begin
  if not FEdit.IsSelected then Exit;
  if not Confirm('Удалить выбранную запись?') then Exit;

  DelIndex:= FEdit.SelectedRowIndex;
  QSetQuery(FQuery);
  QSetSQL(
    'DELETE FROM' + SqlEsc(FTableName) +
    'WHERE' + SqlEsc(FIDFieldName) + '= :IDValue'
  );
  try
    QParamInt64('IDValue', FIDValues[DelIndex]);
    QExec;
    QCommit;
  except
    QRollBack;
  end;


  MRowDel(FDataValues, DelIndex);
  VDel(FIDValues, DelIndex);

  FEdit.CanUnselect:= True;
  FEdit.UnSelect(False);
  if not VIsNil(FIDValues) then
  begin
    if DelIndex>0 then
      FEdit.Select(DelIndex-1, 1)
    else
      FEdit.Select(DelIndex, 1);
  end;
  FEdit.RowDelete(DelIndex);
  FEdit.CanUnselect:= False;
end;

procedure TDBTable.ActionEdit(Sender: TObject);
begin
  FEdit.Select(FEdit.SelectedRowIndex, FEdit.SelectedColIndex);
end;

procedure TDBTable.ActionSave(Sender: TObject);
var
  i: Integer;
  S: String;
  NewValues: TStrVector;
begin
  FEdit.IsOneRowEditing:= False;
  FEdit.UnSelect(True);
  NewValues:= VCut(FEdit.RowValues[FEditingRowIndex], 1);

  for i:= 0 to High(FFieldNames) do
  begin
    if FColumnNeedValues[i] and SEmpty(NewValues[i]) then
    begin
      S:= 'Не указано значение';
      if Length(FFieldNames)>1 then
      begin
        S:= S + ' для столбца ';
        if FEdit.HeaderVisible then
          S:= S + '"' + FColumnNames[i] + '"'
        else
          S:= S + '№' + IntToStr(i+1);
      end;
      ShowInfo(S + '!');
      FEdit.Select(FEditingRowIndex, i+1);
      FEdit.Select(FEditingRowIndex, i+1);
      Exit;
    end;
  end;

  QSetQuery(FQuery);
  if FIsInserting then
  begin
    QSetSQL(
      SqlINSERT(FTableName, FFieldNames)
    );
  end
  else begin
    QSetSQL(
      SqlUPDATE(FTableName, FFieldNames) +
      'WHERE' + SqlEsc(FIDFieldName) + '= :IDValue'
    );
    QParamInt64('IDValue', FIDValues[FEditingRowIndex]);
  end;

  for i:= 0 to High(FFieldNames) do
  begin
    case FColumnTypes[i] of
      ctInteger: QParamIntFromStr(FFieldNames[i], NewValues[i]);
      ctString:  QParamStrFromStr(FFieldNames[i], NewValues[i]);
      ctDate:    QParamDTFromStr(FFieldNames[i], NewValues[i]);
      ctTime:    QParamDTFromStr(FFieldNames[i], NewValues[i]);
      ctKeyPick: QParamIntFromStr(FFieldNames[i], NewValues[i]);
      //ctFloat
    end;
  end;

  try
    QExec;
    QCommit;
  except
    on E:ESQLDatabaseError do
    begin
      FLastErrorCode:= E.ErrorCode;
      QRollBack;
    end;
  end;

  MRowSet(FDataValues, FEditingRowIndex, NewValues);
  if FIsInserting then
  begin
    FIsInserting:= False;
    VIns(FIDValues, FEditingRowIndex, LastWritedInt64ID(FQuery, FTableName));
  end;
end;

procedure TDBTable.ActionCancel(Sender: TObject);
var
  OldValues: TStrVector;
begin
  FEdit.IsOneRowEditing:= False;
  FEdit.UnSelect(False);
  if FIsInserting then
  begin
    FIsInserting:= False;
    MRowDel(FDataValues, FEditingRowIndex);
    FEdit.RowDelete(FEditingRowIndex);
  end
  else begin
    OldValues:= MRowGet(FDataValues, FEditingRowIndex);
    FEdit.RowValues[FEditingRowIndex]:= VAdd([EmptyStr], OldValues);
  end;
end;

procedure TDBTable.ActionUpdate(Sender: TObject);
begin
  DataLoad;
  DataShow;
end;

procedure TDBTable.DataLoad;
var
  i: Integer;
begin
  FDataValues:= nil;
  FIDValues:= nil;
  MDim(FDataValues, Length(FFieldNames));

  if (not SEmpty(FMasterIDFieldName)) and SEmpty(FMasterIDFieldValue) then Exit;

  QSetQuery(FQuery);
  QSetSQL(FReadSQL);
  if not SEmpty(FMasterIDFieldName) then
    QParamInt64('MasterIDValue', StrToInt64(FMasterIDFieldValue));
  QOpen;
  if not QIsEmpty then
  begin
    QFirst;
    while not QEOF do
    begin
      VAppend(FIDValues, QFieldInt64(FIDFieldName));
      for i:= 0 to High(FFieldNames) do
      begin
        case FColumnTypes[i] of
          ctInteger: VAppend(FDataValues[i], IntToStr(QFieldInt(FFieldNames[i])));
          ctString:  VAppend(FDataValues[i], QFieldStr(FFieldNames[i]));
          ctDate:    VAppend(FDataValues[i], DateToStr(QFieldDT(FFieldNames[i])));
          ctTime:    VAppend(FDataValues[i], TimeToStr(QFieldDT(FFieldNames[i])));
          ctKeyPick: VAppend(FDataValues[i], IntToStr(QFieldInt(FFieldNames[i])));
          //ctFloat
        end;
      end;
      QNext;
    end;
  end;
  QClose;
end;

procedure TDBTable.DataShow;
var
  i: Integer;
  V: TStrVector;
begin
  FEdit.ValuesClear;
  if (not MIsNil(FDataValues)) and (not VIsNil(FDataValues[0])) then
  begin
    //V:= VIntToStr(VOrder(Length(FDataValues[0]));
    VDim(V{%H-}, Length(FDataValues[0]), EmptyStr);
    FEdit.SetColumnRowTitles(V, taLeftJustify);
    for i:= 0 to High(FFieldNames) do
      FEdit.SetColumnString(FColumnNames[i], FDataValues[i]);
  end;
  FEdit.Draw;

  if (not MIsNil(FDataValues)) and (not VIsNil(FDataValues[0])) then
    FEdit.Select(0,1);
end;

procedure TDBTable.CellSelect;
begin
  FButtonAdd.Enabled:= not FEdit.IsOneRowEditing;
  FButtonDelete.Enabled:= FEdit.IsSelected and (not FEdit.IsOneRowEditing);
  FButtonEdit.Enabled:= FEdit.IsSelected and (not FEdit.IsOneRowEditing);
  FButtonSave.Enabled:= FEdit.IsOneRowEditing;
  FButtonCancel.Enabled:= FEdit.IsOneRowEditing;
  FButtonUpdate.Enabled:= not FEdit.IsOneRowEditing;
  if Assigned(FOnSelect) then FOnSelect;
end;

procedure TDBTable.EditingBegin;
begin
  FEditingRowIndex:= FEdit.SelectedRowIndex;
  FEdit.IsOneRowEditing:= True;
end;

function TDBTable.GetIDValue: String;
begin
  Result:= EmptyStr;
  if (not FEdit.IsSelected) or VIsNil(FIDValues) then Exit;
  Result:= IntToStr(FIDValues[FEdit.SelectedRowIndex]);
end;

procedure TDBTable.Settings(const AFont: TFont;
                       const ATableName, AIDFieldName: String;
                       const AFieldNames, AColumnNames: TStrVector;
                       const AColumnTypes: TVSTColumnTypes;
                       const AColumnNeedValues: TBoolVector;
                       const AColumnWidths: TIntVector;
                       const AColumnAlignments: array of TAlignment;
                       const AIDNotZero, AHeaderVisible: Boolean;
                       const AOrderFieldNames: TStrVector = nil;
                       const AAutoSizeColumnNumber: Integer = 1;
                       const AKeys: TIntMatrix = nil;
                       const APicks: TStrMatrix = nil;
                       const AMasterIDFieldName: String = '');

  procedure SetColumnNames;
  var
    i: Integer;
  begin
    if not VIsNil(AColumnNames) then
      FColumnNames:= AColumnNames
    else begin
      VDim(FColumnNames, Length(AFieldNames));
      for i:= 0 to High(FColumnNames) do
        FColumnNames[i]:= 'Column' + IntToStr(i+1);
    end;
  end;

  procedure SetReadSQL;
  var
    S: String;
  begin
    if not SEmpty(AMasterIDFieldName) then
      FReadSQL:= SqlFieldsEnum(VAdd(AFieldNames, [AIDFieldName, AMasterIDFieldName]))
    else
      FReadSQL:= SqlFieldsEnum(VAdd(AFieldNames, [AIDFieldName]));

    FReadSQL:= 'SELECT ' +  SqlFieldsEnum(VAdd(AFieldNames, [AIDFieldName]))  + ' FROM' + SqlEsc(ATableName);
    S:= EmptyStr;
    if AIDNotZero then
      S:= ' (' + SqlEsc(AIDFieldName) + ' > 0) ';
    if not SEmpty(AMasterIDFieldName) then
    begin
      if not SEmpty(S) then
        S:= S + 'AND';
      S:= ' (' + SqlEsc(AMasterIDFieldName) + ' = :MasterIDValue) ';
    end;
    if not SEmpty(S) then
      FReadSQL:= FReadSQL + 'WHERE' + S;
    if not VIsNil(AOrderFieldNames) then
      FReadSQL:= FReadSQL + 'ORDER BY' + SqlFieldsEnum(AOrderFieldNames);
  end;

  procedure SetColumns;
  var
    i: Integer;
  begin
    if Assigned(AFont) then
      FEdit.SetSingleFont(AFont);
    FEdit.HeaderVisible:= AHeaderVisible;
    if AAutoSizeColumnNumber<=0 then
      FEdit.AutosizeColumnDisable
    else
      FEdit.AutosizeColumnEnable(AAutoSizeColumnNumber);
    FEdit.AddColumnRowTitles('', 0);
    FTree.Header.Columns[0].MinWidth:= 0;
    for i:= 0 to High(FFieldNames) do
    begin
      case FColumnTypes[i] of
        ctInteger: FEdit.AddColumnInteger(FColumnNames[i], FColumnWidths[i], taCenter, AColumnAlignments[i]);
        ctString:  FEdit.AddColumnString(FColumnNames[i], FColumnWidths[i], taCenter, AColumnAlignments[i]);
        ctDate:    FEdit.AddColumnDate(FColumnNames[i], 'dd.mm.yyyy', FColumnWidths[i], taCenter, AColumnAlignments[i]);
        ctTime:    FEdit.AddColumnTime(FColumnNames[i], '00:00:00', FColumnWidths[i], taCenter, AColumnAlignments[i]);
        ctKeyPick: FEdit.AddColumnKeyPick(FColumnNames[i], FKeys[i], FPicks[i], FColumnWidths[i], taCenter, AColumnAlignments[i]);
        //ctFloat
      end;
    end;
  end;

begin
  FTableName:= ATableName;
  FIDFieldName:= AIDFieldName;
  FFieldNames:= AFieldNames;
  FColumnTypes:= AColumnTypes;
  FColumnNeedValues:= AColumnNeedValues;
  FColumnWidths:= AColumnWidths;
  FKeys:= AKeys;
  FPicks:= APicks;
  FMasterIDFieldName:= AMasterIDFieldName;

  SetColumnNames;
  SetReadSQL;
  SetColumns;
  ActionUpdate(nil);
end;

procedure TDBTable.Update(const AMasterIDFieldValue: String);
begin
  if SSame(FMasterIDFieldValue, AMasterIDFieldValue) and (not MIsNil(FDataValues)) then Exit;
  FMasterIDFieldValue:= AMasterIDFieldValue;
  ActionUpdate(nil);
end;

end.

