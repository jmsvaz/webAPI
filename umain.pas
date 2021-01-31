unit uMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TfmWebQuery }

  TfmWebQuery = class(TForm)
    Button1: TButton;
    Edit1: TEdit;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
  private

  public

  end;

var
  fmWebQuery: TfmWebQuery;

implementation

uses OMDB;

const
{$I API_KEYS.inc}

{$R *.lfm}

{ TfmWebQuery }

procedure TfmWebQuery.Button1Click(Sender: TObject);
Var
  aOMDBMovie: TOMDBMovie;
  aOMDBAPI : TOMDB;
begin
  Memo1.Clear;
  aOMDBAPI:= TOMDB.Create(OMDB_API_KEY);
  try
    aOMDBMovie:= aOMDBAPI.GetMovieByTitle(Edit1.Text);
    Memo1.Append('Title: ' + aOMDBMovie.Title);
    Memo1.Append(aOMDBMovie.FormatJSON);
  except
    on e: Exception do
      begin
        ShowMessage('Exception : ' + e.Message);
        aOMDBAPI.Free;
        Exit;
      end;
  end;
  aOMDBMovie.Free;
  aOMDBAPI.Free;
end;



end.

