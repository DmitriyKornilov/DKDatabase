unit UDBTableSingle;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, ExtCtrls, Graphics, Controls,

  DK_Vector, DK_Matrix, DK_DBTable, DK_PPI, DK_CtrlUtils, DK_SQLite3,
  DK_VSTTypes;

type

  { TDBTableSingle }

  TDBTableSingle = class (TObject)
  private
    FForm: TForm;
    FPanel: TPanel;
    FDBTable: TDBTable;
    FTotalWidth: Integer;
    procedure FormShow(Sender: TObject);
  public
    constructor Create(const ADataBase: TSQLite3;
                       const AFont: TFont;
                       const ANeedFilter: Boolean = False;
                       const AFilterCaption: String = '';
                       const AFilterDelayMS: Integer = 1);
    procedure Show(const AFormCaption: String;
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
                       const AMasterIDFieldName: String = '';
                       const AMasterIDValue: String = '';
                       const AFilterFieldName: String = '');
    destructor Destroy; override;
  end;

implementation

{ TDBTableSingle }

procedure TDBTableSingle.FormShow(Sender: TObject);
begin
  if FTotalWidth<(Sender as TForm).Constraints.MinWidth then
    FTotalWidth:= (Sender as TForm).Constraints.MinWidth;
  FTotalWidth:= WidthFromDefaultToScreen(FTotalWidth);
  if FTotalWidth>Screen.Width then
    FTotalWidth:= Screen.Width - 20;
  (Sender as TForm).Width:= FTotalWidth;
  FormToScreenCenter(Sender as TForm);
end;

constructor TDBTableSingle.Create(const ADataBase: TSQLite3;
                       const AFont: TFont;
                       const ANeedFilter: Boolean = False;
                       const AFilterCaption: String = '';
                       const AFilterDelayMS: Integer = 1);
begin
  inherited Create;

  FForm:= TForm.Create(nil);
  FForm.OnShow:= @FormShow;
  FForm.BorderIcons:= [biSystemMenu, biMaximize];
  FForm.Position:= poScreenCenter;
  FForm.Width:= 550;
  FForm.Height:= 650;
  FForm.Constraints.MinWidth:= 500;
  FForm.Constraints.MinHeight:= 200;

  FPanel:= TPanel.Create(FForm);
  FPanel.Parent:= FForm;
  FPanel.BevelInner:= bvNone;
  FPanel.BevelOuter:= bvNone;
  FPanel.BorderStyle:= bsNone;
  FPanel.Align:= alClient;
  FPanel.AnchorClient(2);

  FDBTable:= TDBTable.Create(AFont, FPanel, ADataBase,
                             ANeedFilter, AFilterCaption, AFilterDelayMS);
  FDBTable.Edit.HeaderFont.Style:= FDBTable.Edit.HeaderFont.Style + [fsBold];
end;

procedure TDBTableSingle.Show(const AFormCaption: String;
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
                       const AMasterIDFieldName: String = '';
                       const AMasterIDValue: String = '';
                       const AFilterFieldName: String = '');
begin
  FForm.Caption:= AFormCaption;
  FTotalWidth:= VSum(AColumnWidths) + 10;
  FDBTable.Settings(ATableName, AIDFieldName, AFieldNames,
                    AColumnNames, AColumnTypes, AColumnNeedValues,
                    AColumnWidths, AColumnAlignments,
                    AIDNotZero, not VIsNil(AColumnNames),
                    AOrderFieldNames, AAutoSizeColumnNumber, AKeys, APicks,
                    AMasterIDFieldName, AMasterIDValue, AFilterFieldName);
  FForm.ShowModal;
end;

destructor TDBTableSingle.Destroy;
begin
  FreeAndNil(FForm);
  FreeAndNil(FDBTable);
  inherited Destroy;
end;

end.

