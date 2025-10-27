unit DK_DBTable;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Buttons, VirtualTrees, ExtCtrls, SQLDB, Controls, Graphics,
  SQLite3, Forms, DividerBevel, ComCtrls,

  DK_Vector, DK_Matrix, DK_SQLUtils, DK_StrUtils, DK_Dialogs,
  DK_DBUtils, DK_CtrlUtils, DK_VSTEdit, DK_VSTTypes, DK_Filter, DK_SQLite3,

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
    FSQLite3: TSQLite3;
    FToolPanel: TPanel;
    FTree: TVirtualStringTree;
    FDBImages: TDBImages;
    FEdit: TVSTEdit;
    FBevel: TDividerBevel;
    FFilterPanel: TPanel;

    FLastErrorCode: Integer;

    FTableName, FIDFieldName, FNotZeroIDFieldName: String;
    FFieldNames, FColumnNames, FOrderFieldNames: TStrVector;
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
    FMasterIDValue: String;
    FFilterFieldName: String;
    FFilterValue: String;

    FOnSelect: TDBTableSelectEvent;

    procedure ActionInsert(Sender: TObject);
    procedure ActionDelete(Sender: TObject);
    procedure ActionEdit(Sender: TObject);
    procedure ActionSave(Sender: TObject);
    procedure ActionCancel(Sender: TObject);
    procedure ActionUpdate(Sender: TObject);

    procedure DataFilter(const AFilterValue: String);
    procedure DataLoad;
    procedure DataShow;

    procedure CellSelect;
    procedure EditingBegin;
    function GetIDValue: String;

  public
    constructor Create(const AFont: TFont;
                       const APanel: TPanel;
                       const ASQLite3: TSQLite3;
                       const ANeedFilter: Boolean = False;
                       const AFilterCaption: String = '';
                       const AFilterDelayMS: Integer = DELAY_MILLISECONDS_DEFAULT);
    destructor Destroy; override;
    procedure Settings(const ATableName, AIDFieldName: String;
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
                       const AMasterIDFieldName: String = '';
                       const AMasterIDValue: String = '';
                       const AFilterFieldName: String = '');
    procedure EditingCancel;
    procedure MasterIDUpdate(const AMasterIDValue: String = '');
    property OnSelect: TDBTableSelectEvent read FOnSelect write FOnSelect;
    property IDValue: String read GetIDValue;
    property LastErrorCode: Integer read FLastErrorCode;
    property Edit: TVSTEdit read FEdit;
  end;

implementation

{ TDBTable }

constructor TDBTable.Create(const AFont: TFont;
                           const APanel: TPanel;
                           const ASQLite3: TSQLite3;
                           const ANeedFilter: Boolean = False;
                           const AFilterCaption: String = '';
                           const AFilterDelayMS: Integer = DELAY_MILLISECONDS_DEFAULT);
var
  Images: TImageList;

  procedure ButtonCreate(var AButton: TSpeedButton; const AIconIndex: Integer;
                         const AHint: String);
  begin
    AButton:= TSpeedButton.Create(FToolPanel);
    AButton.Parent:= FToolPanel;
    AButton.Cursor:= crHandPoint;
    AButton.Align:= alLeft;
    AButton.Images:= Images;
    AButton.ImageIndex:= AIconIndex;
    AButton.AutoSize:= False;
    SetControlWidthScaleToForm(AButton, TOOL_BUTTON_WIDTH_DEFAULT);
    AButton.Hint:= AHint;
    AButton.ShowHint:= True;
  end;

begin
  FSQLite3:= ASQLite3;

  FLastErrorCode:= SQLITE_OK;
  FMasterIDValue:= EmptyStr;

  FDBImages:= TDBImages.Create(nil);
  Images:= ChooseImageListForScreenPPI(FDBImages.PX24, FDBImages.PX30,
                                       FDBImages.PX36, FDBImages.PX42);

  FToolPanel:= TPanel.Create(nil);
  FToolPanel.Parent:= APanel;
  FToolPanel.Align:= alTop;
  FToolPanel.AutoSize:= False;
  SetControlHeightScaleToForm(FToolPanel, TOOL_PANEL_HEIGHT_DEFAULT);
  FToolPanel.BevelInner:= bvNone;
  FToolPanel.BevelOuter:= bvNone;
  FToolPanel.BorderStyle:= bsSingle;
  FToolPanel.AnchorToNeighbour(akLeft, 0, APanel);
  FToolPanel.AnchorToNeighbour(akTop, 0, APanel);
  FToolPanel.AnchorToNeighbour(akRight, 0, APanel);

  FTree:= TVirtualStringTree.Create(nil);
  FTree.Parent:= APanel;
  FTree.Align:= alClient;
  FTree.AnchorToCompanion(akTop, 2, FToolPanel, True);
  FTree.AnchorToNeighbour(akLeft, 0, APanel);
  FTree.AnchorToNeighbour(akBottom, 0, APanel);
  FTree.AnchorToNeighbour(akRight, 0, APanel);
  FTree.OnChangeBounds:= @ActionCancel;

  if ANeedFilter then
  begin
    FBevel:= TDividerBevel.Create(FToolPanel);
    FBevel.BevelStyle:= bsLowered;
    FBevel.BevelWidth:= 2;
    FBevel.Orientation:= trVertical;
    FBevel.Style:= gsSimple;
    FBevel.Parent:= FToolPanel;
    FBevel.Align:= alLeft;
  end;

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

  FButtonDelete.Enabled:= False;
  FButtonEdit.Enabled:= False;
  FButtonSave.Enabled:= False;
  FButtonCancel.Enabled:= False;

  FEdit:= TVSTEdit.Create(FTree);
  FEdit.CanUnselect:= False;
  FEdit.IsBeginEditOnKeyPress:= False;
  FEdit.OnSelect:= @CellSelect;
  FEdit.OnEdititingBegin:= @EditingBegin;
  if Assigned(AFont) then FEdit.SetSingleFont(AFont);

  FIsInserting:= False;

  if not ANeedFilter then Exit;

  FFilterPanel:= TPanel.Create(FToolPanel);
  FFilterPanel.Parent:= FToolPanel;
  FFilterPanel.Align:= alClient;
  FFilterPanel.BevelInner:= bvNone;
  FFilterPanel.BevelOuter:= bvNone;
  FFilterPanel.BorderStyle:= bsNone;

  DKFilterCreate(AFilterCaption, FFilterPanel, @DataFilter, AFilterDelayMS);
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

  FEdit.UnSelect(False);

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

  DelIndex:= FEdit.SelectedRowIndex;
  FEdit.UnSelect(False);
  if not Confirm('Удалить выбранную запись?') then Exit;

  QSetQuery(FSQLite3.Query);
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
var
  i, j: Integer;
begin
  i:= FEdit.SelectedRowIndex;
  j:= FEdit.SelectedColIndex;
  FEdit.UnSelect(False);
  FEdit.Select(i, j);
end;

procedure TDBTable.ActionSave(Sender: TObject);
var
  NewValues: TStrVector;

  procedure ValuesVerify;
  var
    i: Integer;
    S: String;
  begin
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
        Inform(S + '!');
        FEdit.Select(FEditingRowIndex, i+1);
        FEdit.Select(FEditingRowIndex, i+1);
        Exit;
      end;
    end;
  end;

  procedure QueryTune;
  var
    i: Integer;
    S: String;
  begin
    QSetQuery(FSQLite3.Query);
    if FIsInserting then
    begin
      if not SEmpty(FMasterIDFieldName) then
        S:= SqlINSERT(FTableName, VAdd(FFieldNames, [FMasterIDFieldName]))
      else
        S:= SqlINSERT(FTableName, FFieldNames);
      QSetSQL(S);
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
        ctColor:   QParamIntFromStr(FFieldNames[i], NewValues[i]);
        //ctDouble
      end;
    end;

    if not SEmpty(FMasterIDFieldName) then
      QParamIntFromStr(FMasterIDFieldName, FMasterIDValue);
  end;

begin
  FEdit.IsOneRowEditing:= False;
  FEdit.UnSelect(True);
  NewValues:= VCut(FEdit.RowValues[FEditingRowIndex], 1);

  ValuesVerify;
  QueryTune;

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
    VIns(FIDValues, FEditingRowIndex, LastWritedInt64ID(FSQLite3.Query, FTableName));
  end;
end;

procedure TDBTable.ActionCancel(Sender: TObject);
begin
  EditingCancel;
end;

procedure TDBTable.ActionUpdate(Sender: TObject);
begin
  DataFilter(FFilterValue);
end;

procedure TDBTable.DataFilter(const AFilterValue: String);
begin
  FFilterValue:= AFilterValue;
  DataLoad;
  DataShow;
end;

procedure TDBTable.DataLoad;
var
  MasterIDValue: Int64;
  FieldNames: TStrVector;
  Values: TStrMatrix;
  IDFieldIndex: Integer;
begin
  FDataValues:= nil;
  FIDValues:= nil;

  if VIsNil(FFieldNames) or SEmpty(FIDFieldName) then Exit;

  FieldNames:= VCut(FFieldNames);
  IDFieldIndex:= VindexOf(FFieldNames, FIDFieldName);
  if IDFieldIndex<0 then
  begin
    VIns(FieldNames, 0, FIDFieldName);
    IDFieldIndex:= 0;
  end;

  if not TryStrToInt64(FMasterIDValue, MasterIDValue) then
    MasterIDValue:= -1;

  FSQLite3.TableMatch(FFilterValue, FTableName, FieldNames, FOrderFieldNames, Values,
                      FNotZeroIDFieldName, FMasterIDFieldName, MasterIDValue);

  FIDValues:= VStrToInt64(Values[IDFieldIndex]);
  FDataValues:= MCut(Values);
  if Length(FDataValues)>Length(FFieldNames) then
    MDel(FDataValues, IDFieldIndex);
end;

procedure TDBTable.DataShow;
var
  i: Integer;
  V: TStrVector;
begin
  FEdit.ValuesClear;
  if (not MIsNil(FDataValues)) and (not VIsNil(FDataValues[0])) then
  begin
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

procedure TDBTable.Settings(const ATableName, AIDFieldName: String;
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
                       const AMasterIDFieldName: String = '';
                       const AMasterIDValue: String = '';
                       const AFilterFieldName: String = '');

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

  procedure SetColumns;
  var
    i: Integer;
  begin
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
        ctColor:   FEdit.AddColumnColor(FColumnNames[i], FColumnWidths[i]);
        //ctDouble
      end;
    end;
  end;

begin
  FEdit.Clear;

  FTableName:= ATableName;
  FIDFieldName:= AIDFieldName;
  FFieldNames:= AFieldNames;
  FColumnTypes:= AColumnTypes;
  FColumnNeedValues:= AColumnNeedValues;
  FColumnWidths:= AColumnWidths;
  FOrderFieldNames:= AOrderFieldNames;
  FKeys:= AKeys;
  FPicks:= APicks;
  FMasterIDFieldName:= AMasterIDFieldName;
  FFilterFieldName:= AFilterFieldName;
  FNotZeroIDFieldName:= EmptyStr;
  if AIDNotZero then
    FNotZeroIDFieldName:= FIDFieldName;

  SetColumnNames;
  SetColumns;
  MasterIDUpdate(AMasterIDValue);
end;

procedure TDBTable.EditingCancel;
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

procedure TDBTable.MasterIDUpdate(const AMasterIDValue: String);
begin
  if SSame(FMasterIDValue, AMasterIDValue) and (not MIsNil(FDataValues)) then Exit;
  FMasterIDValue:= AMasterIDValue;
  ActionUpdate(nil);
end;

end.

