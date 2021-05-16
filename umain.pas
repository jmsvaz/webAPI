unit uMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, OMDB;

type

  { TfmWebQuery }

  TfmWebQuery = class(TForm)
    btSearch: TButton;
    cbSearchAPI: TComboBox;
    cbSearchMethod: TComboBox;
    edSearch: TEdit;
    mmResult: TMemo;
    procedure btSearchClick(Sender: TObject);
    procedure cbSearchAPIChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private    
    aOMDBAPI: TOMDB;
  public

  end;

var
  fmWebQuery: TfmWebQuery;

implementation

uses WebQuerybase;

const
{$I API_KEYS.inc}

{$R *.lfm}

{ TfmWebQuery }

procedure TfmWebQuery.FormCreate(Sender: TObject);
begin
  aOMDBAPI:= TOMDB.Create(OMDB_API_KEY);

  cbSearchAPI.Items.Add('OMDB');
  cbSearchAPI.ItemIndex:= 0;
end;

procedure TfmWebQuery.FormDestroy(Sender: TObject);
begin
  aOMDBAPI.Free;
end;

procedure TfmWebQuery.btSearchClick(Sender: TObject);
Var
  aResult: TCustomJSONResponse; //
  aOMDBMovie: TOMDBMovie;
begin
  mmResult.Clear;
  try
    aResult:= aOMDBAPI.GetMovieByTitle(edSearch.Text);
    if aResult is TOMDBMovie then
      begin
        aOMDBMovie:= TOMDBMovie(aResult);
        mmResult.Append('Movie Title: ' + aOMDBMovie.Title);
      end;

    mmResult.Append(aResult.FormatJSON);
  except
    on e: Exception do
      begin
        ShowMessage('Exception : ' + e.Message);
      end;
  end;
   aResult.Free;
end;

procedure TfmWebQuery.cbSearchAPIChange(Sender: TObject);
begin
  case cbSearchAPI.ItemIndex of
    0: //OMDB

  end;
end;





end.

