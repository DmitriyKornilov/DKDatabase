unit DK_DBUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, DbCtrls, Graphics, DB, SQLDB, DK_SQLUtils;

procedure DataSetChangesSave(const ADataSet: TDataSet);

procedure ChangeDbNavigatorGlyphs(ADbNavigator: TDbNavigator; AImageList: TImageList);

function LastWritedInt32ID(const AQuery: TSQLQuery; const ATableName: String): Integer;
function LastWritedInt64ID(const AQuery: TSQLQuery; const ATableName: String): Int64;

function MaxInt32ID(const AQuery: TSQLQuery; const ATableName: String): Integer;
function MaxInt64ID(const AQuery: TSQLQuery; const ATableName: String): Int64;

implementation

procedure DataSetChangesSave(const ADataSet: TDataSet);
begin
  (ADataSet As TSQLQuery).ApplyUpdates;
  (ADataSet As TSQLQuery).SQLTransaction.CommitRetaining;
end;

procedure ChangeDbNavButton(ADBNav: TDbNavigator;
                            const ADBBtnType: TDBNavButtonType;
                            const ADBBtnGlyph: TBitmap;
                            const ADBBtnCursor: TCursor = crDefault);
var
  i: Integer;
  NB: TDBNavButton;
begin
  for i := 0 to ADBNav.ControlCount - 1 do
  begin
    if ADBNav.Controls[i].ClassName = 'TDBNavButton' then
    begin
      NB:= (ADBNav.Controls[i] As TDBNavButton);
      if NB.Index= ADBBtnType then
      begin
        NB.Glyph := ADBBtnGlyph;
        NB.Cursor:= ADBBtnCursor;
      end;
    end;
  end;
end;

procedure ChangeDbNavigatorGlyphs(ADbNavigator: TDbNavigator; AImageList: TImageList);
var
  BM: TBitmap;
begin
  if not Assigned(AImageList) then Exit;
  if AImageList.Count<5 then Exit;
  BM:= TBitmap.Create;
  try
    AImageList.GetBitmap(0, BM);
    ChangeDBNavButton(ADbNavigator, nbInsert, BM, crHandPoint);
    AImageList.GetBitmap(1, BM);
    ChangeDBNavButton(ADbNavigator, nbDelete, BM, crHandPoint);
    AImageList.GetBitmap(2, BM);
    ChangeDBNavButton(ADbNavigator, nbEdit, BM, crHandPoint);
    AImageList.GetBitmap(3, BM);
    ChangeDBNavButton(ADbNavigator, nbPost, BM, crHandPoint);
    AImageList.GetBitmap(4, BM);
    ChangeDBNavButton(ADbNavigator, nbCancel, BM, crHandPoint);
  finally
    FreeAndNil(BM);
  end;
end;

function LastWritedInt32ID(const AQuery: TSQLQuery; const ATableName: String): Integer;
begin
  Result:= 0;
  QSetQuery(AQuery);
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

function LastWritedInt64ID(const AQuery: TSQLQuery; const ATableName: String): Int64;
begin
  Result:= 0;
  QSetQuery(AQuery);
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

function MaxInt32ID(const AQuery: TSQLQuery; const ATableName: String): Integer;
begin
  Result:= 0;
  QSetQuery(AQuery);
  QSetSQL(
    'SELECT MAX(RowID) AS MaxID ' +
    'FROM' + SqlEsc(ATableName)
    );
  QOpen;
  if not QIsEmpty then
    Result:= QFieldInt('MaxID');
  QClose;
end;

function MaxInt64ID(const AQuery: TSQLQuery; const ATableName: String): Int64;
begin
  Result:= 0;
  QSetQuery(AQuery);
  QSetSQL(
    'SELECT MAX(RowID) AS MaxID ' +
    'FROM' + SqlEsc(ATableName)
    );
  QOpen;
  if not QIsEmpty then
    Result:= QFieldInt64('MaxID');
  QClose;
end;

end.

