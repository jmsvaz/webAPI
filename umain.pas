unit uMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, OMDB, TMDB;

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
    aTMDBAPI: TTMDB;
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
  aTMDBAPI:= TTMDB.Create(TMDB_API_KEY);

  cbSearchAPI.Items.Add(aOMDBAPI.Caption);
  cbSearchAPI.Items.Add(aTMDBAPI.Caption);
  cbSearchAPI.ItemIndex:= 0;
  cbSearchAPIChange(sender);
end;

procedure TfmWebQuery.FormDestroy(Sender: TObject);
begin
  aOMDBAPI.Free;
end;

procedure TfmWebQuery.cbSearchAPIChange(Sender: TObject);
begin
  cbSearchMethod.Items.Clear;
  if cbSearchAPI.Items[cbSearchAPI.ItemIndex] = aOMDBAPI.Caption then
    begin
      cbSearchMethod.Items.Add('By Title');
      cbSearchMethod.Items.Add('By IMDBid');
      cbSearchMethod.Items.Add('Movie By Title');
      cbSearchMethod.Items.Add('Movie By IMDBid');
      cbSearchMethod.Items.Add('Series By Title');
      cbSearchMethod.Items.Add('Series By IMDBid');
      cbSearchMethod.Items.Add('Episode By Title');
      cbSearchMethod.Items.Add('Episode By IMDBid');
      cbSearchMethod.Items.Add('Game By Title');
      cbSearchMethod.Items.Add('Game By IMDBid');
      cbSearchMethod.Items.Add('Search');
      cbSearchMethod.Items.Add('Search Movie');
      cbSearchMethod.Items.Add('Search Series');
      cbSearchMethod.Items.Add('Search Episode');
      cbSearchMethod.Items.Add('Search Game');

      cbSearchMethod.ItemIndex:= 0;
    end;
  if cbSearchAPI.Items[cbSearchAPI.ItemIndex] = aTMDBAPI.Caption then
    begin
      cbSearchMethod.Items.Add('Configuration');
      cbSearchMethod.Items.Add('Countries');
      cbSearchMethod.Items.Add('Jobs');
      cbSearchMethod.Items.Add('Languages');
      cbSearchMethod.Items.Add('Time Zones');

      cbSearchMethod.ItemIndex:= 0;
    end;
end;

procedure TfmWebQuery.btSearchClick(Sender: TObject);
Var
  aResult: TPersistent;
begin
  mmResult.Clear;
  aResult:= nil;
  try
    if cbSearchAPI.Items[cbSearchAPI.ItemIndex] = aOMDBAPI.Caption then
      begin
        if cbSearchMethod.Items[cbSearchMethod.ItemIndex] = 'By Title' then
          aResult:= aOMDBAPI.GetByTitle(edSearch.Text);
        if cbSearchMethod.Items[cbSearchMethod.ItemIndex] = 'By IMDBid' then
          aResult:= aOMDBAPI.GetByIMDBid(edSearch.Text);
        if cbSearchMethod.Items[cbSearchMethod.ItemIndex] = 'Movie By Title' then
          aResult:= aOMDBAPI.GetMovieByTitle(edSearch.Text);
        if cbSearchMethod.Items[cbSearchMethod.ItemIndex] = 'Movie By IMDBid' then
          aResult:= aOMDBAPI.GetMovieByIMDBid(edSearch.Text);
        if cbSearchMethod.Items[cbSearchMethod.ItemIndex] = 'Series By Title' then
          aResult:= aOMDBAPI.GetSeriesByTitle(edSearch.Text);
        if cbSearchMethod.Items[cbSearchMethod.ItemIndex] = 'Series By IMDBid' then
          aResult:= aOMDBAPI.GetSeriesByIMDBid(edSearch.Text);
        if cbSearchMethod.Items[cbSearchMethod.ItemIndex] = 'Episode By Title' then
          aResult:= aOMDBAPI.GetEpisodeByTitle(edSearch.Text);
        if cbSearchMethod.Items[cbSearchMethod.ItemIndex] = 'Episode By IMDBid' then
          aResult:= aOMDBAPI.GetEpisodeByIMDBid(edSearch.Text);
        if cbSearchMethod.Items[cbSearchMethod.ItemIndex] = 'Game By Title' then
          aResult:= aOMDBAPI.GetGameByTitle(edSearch.Text);
        if cbSearchMethod.Items[cbSearchMethod.ItemIndex] = 'Game By IMDBid' then
          aResult:= aOMDBAPI.GetGameByIMDBid(edSearch.Text);
        if cbSearchMethod.Items[cbSearchMethod.ItemIndex] = 'Search' then
          aResult:= aOMDBAPI.Search(edSearch.Text);
        if cbSearchMethod.Items[cbSearchMethod.ItemIndex] = 'Search Movie' then
          aResult:= aOMDBAPI.SearchMovie(edSearch.Text);
        if cbSearchMethod.Items[cbSearchMethod.ItemIndex] = 'Search Series' then
          aResult:= aOMDBAPI.SearchSeries(edSearch.Text);
        if cbSearchMethod.Items[cbSearchMethod.ItemIndex] = 'Search Episode' then
          aResult:= aOMDBAPI.SearchEpisode(edSearch.Text);
        if cbSearchMethod.Items[cbSearchMethod.ItemIndex] = 'Search Game' then
          aResult:= aOMDBAPI.SearchGame(edSearch.Text);

        if aResult is TOMDBMovie then
          mmResult.Append('Movie Title: ' + TOMDBMovie(aResult).Title);
        if aResult is TOMDBSeries then
          mmResult.Append('Series Title: ' + TOMDBSeries(aResult).Title);
        if aResult is TOMDBEpisode then
          begin
            mmResult.Append('Series ID: ' + TOMDBEpisode(aResult).seriesID);
            mmResult.Append('Episode Title: ' + TOMDBEpisode(aResult).Title);
          end;
        if aResult is TOMDBGame then
          mmResult.Append('Game Title: ' + TOMDBGame(aResult).Title);
        if aResult is TOMDBSearch then
          mmResult.Append('Search Result: ' + IntToStr(TOMDBSearch(aResult).TotalResults));
      end;
    if cbSearchAPI.Items[cbSearchAPI.ItemIndex] = aTMDBAPI.Caption then
      begin
        if cbSearchMethod.Items[cbSearchMethod.ItemIndex] = 'Configuration' then
          aResult:= aTMDBAPI.GetConfiguration;
        if cbSearchMethod.Items[cbSearchMethod.ItemIndex] = 'Countries' then
          aResult:= aTMDBAPI.GetCountries;
        if cbSearchMethod.Items[cbSearchMethod.ItemIndex] = 'Jobs' then
          aResult:= aTMDBAPI.GetJobs;
        if cbSearchMethod.Items[cbSearchMethod.ItemIndex] = 'Languages' then
          aResult:= aTMDBAPI.GetLanguages;
        if cbSearchMethod.Items[cbSearchMethod.ItemIndex] = 'Time Zones' then
          aResult:= aTMDBAPI.GetTimeZones;
      end;
    if Assigned(aResult) then
      begin
        if aResult is TCustomJSONResponse then
          mmResult.Append(TCustomJSONResponse(aResult).FormatJSON);
        if aResult is TCollectionJSONResponse then
          mmResult.Append(TCollectionJSONResponse(aResult).FormatJSON);
      end;
  except
    on e: Exception do
      begin
        ShowMessage('Exception : ' + e.Message);
      end;
  end;
// TODO: find out why freeing aResult is generating a SIGSEV if aResult is get from aTMDBAPI.GetJobs, but not if is get from aTMDBAPI.GetCountries
//   if Assigned(aResult) then
//       FreeAndNil(aResult);
end;



end.

