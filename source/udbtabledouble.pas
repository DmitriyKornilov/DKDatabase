unit UDBTableDouble;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, ExtCtrls, Graphics, Controls,

  DK_Vector, DK_Matrix, DK_DBTable, DK_PPI, DK_CtrlUtils, DK_SQLite3,
  DK_VSTTypes;

type

  { TDBTableDouble }

  TDBTableDouble = class (TObject)
  private
    FForm: TForm;
    FLeftPanel, FRightPanel: TPanel;
    FSplitter: TSplitter;
    FLeftDBTable, FRightDBTable: TDBTable;
    FTotalWidth, FLeftWidth, FRightWidth: Integer;
    FNeedLeftFilter, FNeedRightFilter: Boolean;
    procedure FormShow(Sender: TObject);
    procedure LeftTableSelect;
    procedure RightTableSelect;
  public
    constructor Create(const ADataBase: TSQLite3;
                       const AFont: TFont;
                       const ANeedLeftFilter: Boolean;
                       const ALeftFilterCaption: String;
                       const ALeftFilterDelayMS: Integer;
                       const ANeedRightFilter: Boolean;
                       const ARightFilterCaption: String;
                       const ARightFilterDelayMS: Integer);
    procedure Show(const AFormCaption: String;

                   const ALeftTableName, ALeftIDFieldName: String;
                   const ALeftFieldNames, ALeftColumnNames: TStrVector;
                   const ALeftColumnTypes: TVSTColumnTypes;
                   const ALeftColumnNeedValues: TBoolVector;
                   const ALeftColumnWidths: TIntVector;
                   const ALeftColumnAlignments: array of TAlignment;
                   const ALeftIDNotZero, ALeftHeaderVisible: Boolean;
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
                   const ARightIDNotZero, ARightHeaderVisible: Boolean;
                   const ARightOrderFieldNames: TStrVector;
                   const ARightAutoSizeColumnNumber: Integer;
                   const ARightKeys: TIntMatrix;
                   const ARightPicks: TStrMatrix;
                   const ARightMasterIDFieldName: String);

    destructor Destroy; override;
  end;

implementation

{ TDBTableDouble }

procedure TDBTableDouble.FormShow(Sender: TObject);
const
  MINWIDTH = 300;
var
  W: Integer;
begin
  W:= MINWIDTH;
  if FNeedLeftFilter then W:= Round(1.5*W);
  if FLeftWidth<W then FLeftWidth:= W;

  W:= MINWIDTH;
  if FNeedRightFilter then W:= Round(1.5*W);
  if FRightWidth<W then FRightWidth:= W;

  FTotalWidth:= FLeftWidth + FRightWidth + 10;
  FLeftPanel.Width:= WidthFromDefaultToScreen(FLeftWidth);

  if FTotalWidth<(Sender as TForm).Constraints.MinWidth then
    FTotalWidth:= (Sender as TForm).Constraints.MinWidth;
  FTotalWidth:= WidthFromDefaultToScreen(FTotalWidth);
  if FTotalWidth>Screen.Width then
    FTotalWidth:= Screen.Width - 20;
  (Sender as TForm).Width:= FTotalWidth;
  FormToScreenCenter(Sender as TForm);
end;

procedure TDBTableDouble.LeftTableSelect;
begin
  if FRightDBTable.Edit.IsEditing then
    FRightDBTable.EditingCancel;
  FRightDBTable.MasterIDUpdate(FLeftDBTable.IDValue);
end;

procedure TDBTableDouble.RightTableSelect;
begin
  if FLeftDBTable.Edit.IsEditing then
    FLeftDBTable.EditingCancel;
end;

constructor TDBTableDouble.Create(const ADataBase: TSQLite3;
                       const AFont: TFont;
                       const ANeedLeftFilter: Boolean;
                       const ALeftFilterCaption: String;
                       const ALeftFilterDelayMS: Integer;
                       const ANeedRightFilter: Boolean;
                       const ARightFilterCaption: String;
                       const ARightFilterDelayMS: Integer);
begin
  inherited Create;

  FNeedLeftFilter:= ANeedLeftFilter;
  FNeedRightFilter:= ANeedRightFilter;

  FForm:= TForm.Create(nil);
  FForm.OnShow:= @FormShow;
  FForm.BorderIcons:= [biSystemMenu, biMaximize];
  FForm.Position:= poScreenCenter;
  FForm.Width:= 550;
  FForm.Height:= 650;
  FForm.Constraints.MinWidth:= 500;
  FForm.Constraints.MinHeight:= 200;

  FSplitter:= TSplitter.Create(FForm);
  FSplitter.Parent:= FForm;
  FSplitter.ResizeStyle:= rsLine;
  FSplitter.Align:= alLeft;
  FSplitter.ResizeAnchor:= akLeft;

  FLeftPanel:= TPanel.Create(FForm);
  FLeftPanel.Parent:= FForm;
  FLeftPanel.BevelInner:= bvNone;
  FLeftPanel.BevelOuter:= bvNone;
  FLeftPanel.BorderStyle:= bsNone;
  FLeftPanel.Align:= alLeft;
  FLeftPanel.AnchorToNeighbour(akTop, 2, FForm);
  FLeftPanel.AnchorToNeighbour(akLeft, 2, FForm);
  FLeftPanel.AnchorToNeighbour(akBottom, 2, FForm);

  FRightPanel:= TPanel.Create(FForm);
  FRightPanel.Parent:= FForm;
  FRightPanel.BevelInner:= bvNone;
  FRightPanel.BevelOuter:= bvNone;
  FRightPanel.BorderStyle:= bsNone;
  FRightPanel.Align:= alClient;
  FRightPanel.AnchorToNeighbour(akTop, 2, FForm);
  FRightPanel.AnchorToNeighbour(akRight, 2, FForm);
  FRightPanel.AnchorToNeighbour(akBottom, 2, FForm);

  FLeftDBTable:= TDBTable.Create(AFont, FLeftPanel, ADataBase,
                             ANeedLeftFilter, ALeftFilterCaption, ALeftFilterDelayMS);
  FLeftDBTable.Edit.HeaderFont.Style:= FLeftDBTable.Edit.HeaderFont.Style + [fsBold];
  FLeftDBTable.OnSelect:= @LeftTableSelect;

  FRightDBTable:= TDBTable.Create(AFont, FRightPanel, ADataBase,
                             ANeedRightFilter, ARightFilterCaption, ARightFilterDelayMS);
  FRightDBTable.Edit.HeaderFont.Style:= FRightDBTable.Edit.HeaderFont.Style + [fsBold];
  FRightDBTable.OnSelect:= @RightTableSelect;
end;

procedure TDBTableDouble.Show(const AFormCaption: String;

                   const ALeftTableName, ALeftIDFieldName: String;
                   const ALeftFieldNames, ALeftColumnNames: TStrVector;
                   const ALeftColumnTypes: TVSTColumnTypes;
                   const ALeftColumnNeedValues: TBoolVector;
                   const ALeftColumnWidths: TIntVector;
                   const ALeftColumnAlignments: array of TAlignment;
                   const ALeftIDNotZero, ALeftHeaderVisible: Boolean;
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
                   const ARightIDNotZero, ARightHeaderVisible: Boolean;
                   const ARightOrderFieldNames: TStrVector;
                   const ARightAutoSizeColumnNumber: Integer;
                   const ARightKeys: TIntMatrix;
                   const ARightPicks: TStrMatrix;
                   const ARightMasterIDFieldName: String);
begin
  FForm.Caption:= AFormCaption;
  FLeftWidth:= VSum(ALeftColumnWidths) + 10;
  FRightWidth:= VSum(ARightColumnWidths) + 10;

  FLeftDBTable.Settings(ALeftTableName, ALeftIDFieldName, ALeftFieldNames,
                        ALeftColumnNames, ALeftColumnTypes, ALeftColumnNeedValues,
                        ALeftColumnWidths, ALeftColumnAlignments,
                        ALeftIDNotZero, ALeftHeaderVisible, ALeftOrderFieldNames,
                        ALeftAutoSizeColumnNumber, ALeftKeys, ALeftPicks);
  FRightDBTable.Settings(ARightTableName, ARightIDFieldName, ARightFieldNames,
                        ARightColumnNames, ARightColumnTypes, ARightColumnNeedValues,
                        ARightColumnWidths, ARightColumnAlignments,
                        ARightIDNotZero, ARightHeaderVisible, ARightOrderFieldNames,
                        ARightAutoSizeColumnNumber, ARightKeys, ARightPicks,
                        ARightMasterIDFieldName);
  FForm.ShowModal;
end;

destructor TDBTableDouble.Destroy;
begin
  FreeAndNil(FForm);
  FreeAndNil(FLeftDBTable);
  FreeAndNil(FRightDBTable);
  inherited Destroy;
end;

end.

