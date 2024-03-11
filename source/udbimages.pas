unit UDBImages;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls;

type

  { TDBImages }

  TDBImages = class(TDataModule)
    ToolIcons: TImageList;
  private

  public

  end;

var
  DBImages: TDBImages;

implementation

{$R *.lfm}

end.

