unit uMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  fphttpclient, fpjson, jsonparser;

type

  { TfmWebQuery }

  TfmWebQuery = class(TForm)
    Button1: TButton;
    Edit1: TEdit;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
  private
    procedure DumpJSONData(J : TJSonData; DOEOLN : Boolean = True);
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
  J: TJSONData;
  aOMDBMovie: TOMDBMovie;
  aOMDBAPI : TOMDB;
begin
  Memo1.Clear;
  aOMDBAPI:= TOMDB.Create(OMDB_API);
      try
      aOMDBMovie:= aOMDBAPI.GetMovieByTitle(Edit1.Text);
//      S := httpClient.Post('https://httpbin.org/post');
      J:= GetJSON(aOMDBMovie.JSON);
      memo1.Append(J.FormatJSON());
      Memo1.Append('========================');
      Memo1.Append('Response: ' + BoolToStr(aOMDBMovie.Response,True));
      Memo1.Append('Title: ' + aOMDBMovie.Title);
      Memo1.Append('Genre: ' + aOMDBMovie.Genre.Text);
      except
      on e: Exception do
          begin
          ShowMessage('Exception : ' + e.Message);
          aOMDBAPI.Free;
          Exit;
          end;
      end;
    aOMDBAPI.Free;

//    DumpJsonData(J);
end;

procedure TfmWebQuery.DumpJSONData(J: TJSonData; DOEOLN: Boolean);
var
  I : Integer;
  DataString : String;
begin
  // JSONType property determines kind of value.
  Case J.jsontype of
    jtNull   : Memo1.Append('Null');
    jtBoolean : If J.AsBoolean then
                  Memo1.Append('True')
                else
                  Memo1.Append('False');
    jtNumber : {JSONNumber has extra NumberType property
                which determines kind of value (int/float).}
               Case TJSONNumber(J).NumberType of
                 ntInteger : Memo1.Append(inttostr(J.AsInteger));
                 ntFloat   : Memo1.Append(floattostr(J.AsFloat));
               end;
    jtString : Memo1.Append('"' + J.AsString +'"');
    jtArray  : begin
               Memo1.Append('======================');
               For I:=0 to J.Count-1 do
                 begin
                 DumpJSONData(J.Items[I],False);
                 If I<J.Count-1 then
                   Memo1.Append('+++++++++++++++++++++++');
                 end;
               Memo1.Append('=======================');
               end;
    jtObject : begin
               Memo1.Append('========================');
               For I:=0 to J.Count-1 do
                 begin
                 Memo1.Append('"' + TJSONObject(J).Names[i] + '" : ');
                 DumpJSONData(J.Items[I],False);
                 If I<J.Count-1 then
                   Memo1.Append('+++++++++++++++++++++++++')
                 end;
               Memo1.Append('=========================');
               end;
   end;
   If DOEOLN then
     Memo1.Append('');
end;

end.

