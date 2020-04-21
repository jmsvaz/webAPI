unit OMDB;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpJSON;

type

  { TOMDB }

  TOMDBAPIVersion = (oaV1);

  { TOMDBResponse }

  TOMDBResponse = class
    private
      fJSON: string;
      fJSONData: TJSONData;
      FResponse: Boolean;
      function GetJSON: string;
      procedure SetResponse(AValue: Boolean);
      procedure SetJSON(AValue: string); virtual;
      procedure DeStream;
    public
      constructor Create(aJSON: string = '');
      destructor Destroy; override;
      property JSON: string read GetJSON write SetJSON;
    published
      property Response: Boolean read FResponse write SetResponse;

    end;

  { TOMDBMovie }

  TOMDBMovie = class(TOMDBResponse)
    private
      fGenre: TStrings;
      fTitle: string;
      fYear: string;
//      procedure SetGenre(AValue: TStrings);
      procedure SetTitle(AValue: string);
      procedure SetYear(AValue: string);
    public
      constructor Create(aJSON: string = '');
      destructor Destroy; override;
    published
      property Year: string read fYear write SetYear;
      property Title: string read fTitle write SetTitle;
      property Genre: TStrings read fGenre;
  end;

  TOMDB = class
    private
      fAPIKey: string;
      fTimeOut: Integer;
      fVersion: TOMDBAPIVersion;
      function YearParam(aYear: string): string;
      function RequestURL(aParams: string): string;
      function DoRequest(aURL: string): string;
      function ValidYear(aYear: string): Boolean;
      procedure SetAPIKey(AValue: string);
      procedure SetTimeOut(AValue: Integer);
      procedure SetVersion(AValue: TOMDBAPIVersion);

    public
      constructor Create(aAPIKey: string = '');
      property TimeOut: Integer read fTimeOut write SetTimeOut;
      property APIKey: string read fAPIKey write SetAPIKey;
      property Version: TOMDBAPIVersion read fVersion write SetVersion;
      function GetMovieByTitle(aTitle: string; aYear: string = ''): TOMDBMovie;
      function GetByIMDBid(aIMDBid: string): TOMDBResponse;
      function Search(aTitle: string; aYear: string = ''): TOMDBResponse;
      function SearchMovie(aTitle: string; aYear: string = ''): TOMDBResponse;
  end;

implementation

uses fphttpclient, RegExpr, jsonparser, fpjsonrtti;

function ValidIMDBid(aID: string): Boolean;
var
  regex: TRegExpr;
begin
  regex:= TRegExpr.Create;
  try
    regex.Expression:= 'tt\d{5,7}';
    Result:= regex.Exec(aID);
  finally
    regex.Free;
  end;
end;

const
  OMDBBASEURL = 'http://www.omdbapi.com/';
  OMDBVersionString: array[TOMDBAPIVersion] of string = ('1');

{ TOMDBMovie }

procedure TOMDBMovie.SetYear(AValue: string);
begin
  if fYear= AValue then Exit;
  fYear:= AValue;
end;

constructor TOMDBMovie.Create(aJSON: string);
begin
  inherited Create(aJSON);
  fGenre:= TStringList.Create;
end;

destructor TOMDBMovie.Destroy;
begin
  fGenre.Free;
  inherited Destroy;
end;

procedure TOMDBMovie.SetTitle(AValue: string);
begin
  if fTitle = AValue then Exit;
  fTitle:= AValue;
end;

{ TOMDBResponse }

constructor TOMDBResponse.Create(aJSON: string);
begin
  JSON:= aJSON;
end;

destructor TOMDBResponse.Destroy;
begin
  if Assigned(fJSONData) then
    FreeAndNil(fJSONData);
  inherited Destroy;
end;

function TOMDBResponse.GetJSON: string;
begin
  if Assigned(fJSONData) then
    Result:= fJSONData.AsJSON;
end;

procedure TOMDBResponse.SetResponse(AValue: Boolean);
begin
  if fResponse = AValue then Exit;
  fResponse:= AValue;
end;

procedure TOMDBResponse.SetJSON(AValue: string);
begin
  if Assigned(fJSONData) then
    FreeAndNil(fJSONData);
  fJSONData:= fpJSON.GetJSON(AValue);
  DeStream;
end;

procedure TOMDBResponse.DeStream;
var
  DeStreamer: TJSONDeStreamer;
begin
  DeStreamer := TJSONDeStreamer.Create(nil);
  try
    DeStreamer.JSONToObject(JSON,self);

  finally
    DeStreamer.Free;
  end;

end;

{ TOMDB }

constructor TOMDB.Create(aAPIKey: string);
begin
  Version:= oaV1;
  TimeOut:= 0; // Infinite timeout on most platforms
  APIKey:= aAPIKey;
end;


procedure TOMDB.SetAPIKey(AValue: string);
begin
  if fAPIKey = AValue then Exit;
  fAPIKey:= AValue;
end;

procedure TOMDB.SetTimeOut(AValue: Integer);
begin
  if fTimeOut = AValue then Exit;
  fTimeOut:= AValue;
end;

procedure TOMDB.SetVersion(AValue: TOMDBAPIVersion);
begin
  if fVersion = AValue then Exit;
  fVersion:= AValue;
end;

function TOMDB.YearParam(aYear: string): string;
begin
  if ValidYear(aYear) then
    Result:= '&y=' + EncodeURLElement(aYear);
end;

function TOMDB.RequestURL(aParams: string): string;
begin
  Result:= OMDBBASEURL + '?apikey=' + APIKey + '&v=' + OMDBVersionString[Version] +
           '$r=json' + aParams;
end;

function TOMDB.DoRequest(aURL: string): string;
var
  httpClient : TFPHTTPClient;
  aResult: string;
begin
  httpClient := TFPHTTPClient.Create(nil);
  try
    httpClient.AddHeader('Accept','application/json');
    httpClient.AddHeader('Content-Type','application/json');
    httpClient.IOTimeout:= Timeout;
    aResult:= httpClient.Get(aURL);
    //      memo1.Append('Request Response : ' + httpClient.ResponseStatusText);
  finally
    httpClient.Free;
  end;
  Result:= aResult;
end;

function TOMDB.ValidYear(aYear: string): Boolean;
var
  y: Integer;
begin
  Result:= (Length(aYear) > 0) and TryStrToInt(aYear,y);
end;

function TOMDB.GetMovieByTitle(aTitle: string; aYear: string): TOMDBMovie;
var
  completeURL: string;
begin
  completeURL:= RequestURL('&type=movie&plot=full&t=' + EncodeURLElement(aTitle) + YearParam(aYear));
  Result:= TOMDBMovie.Create(DoRequest(completeURL));
end;

function TOMDB.GetByIMDBid(aIMDBid: string): TOMDBResponse;
var
  completeURL: string;
begin
  if not ValidIMDBid(aIMDBid) then Exit;
  completeURL:= RequestURL('&plot=full&i=' + EncodeURLElement(aIMDBid));
  Result:= TOMDBMovie.Create(DoRequest(completeURL));
end;

function TOMDB.Search(aTitle: string; aYear: string): TOMDBResponse;
var
  completeURL: string;
begin
  completeURL:= RequestURL('&plot=full&s=' + EncodeURLElement(aTitle) + YearParam(aYear));
  Result:= TOMDBResponse.Create(DoRequest(completeURL));

end;

function TOMDB.SearchMovie(aTitle: string; aYear: string): TOMDBResponse;
var
  completeURL: string;
begin
  completeURL:= RequestURL('&type=movie&plot=full&s=' + EncodeURLElement(aTitle) + YearParam(aYear));
  Result:= TOMDBResponse.Create(DoRequest(completeURL));
end;

end.

{       // Movie
"Title" : "The Matrix",
"Year" : "1999",
"Rated" : "R",
"Released" : "31 Mar 1999",
"Runtime" : "136 min",
"Genre" : "Action, Sci-Fi",
"Director" : "Lana Wachowski, Lilly Wachowski",
"Writer" : "Lilly Wachowski, Lana Wachowski",
"Actors" : "Keanu Reeves, Laurence Fishburne, Carrie-Anne Moss, Hugo Weaving",
"Plot" : "Thomas A. Anderson is a man living two lives. By day he is an average computer programmer and by night a hacker known as Neo. Neo has always questioned his reality, but the truth is far beyond his imagination. Neo finds himself targeted by the police when he is contacted by Morpheus, a legendary computer hacker branded a terrorist by the government. Morpheus awakens Neo to the real world, a ravaged wasteland where most of humanity have been captured by a race of machines that live off of the humans' body heat and electrochemical energy and who imprison their minds within an artificial reality known as the Matrix. As a rebel against the machines, Neo must return to the Matrix and confront the agents: super-powerful computer programs devoted to snuffing out Neo and the entire human rebellion.",
"Language" : "English",
"Country" : "USA",
"Awards" : "Won 4 Oscars. Another 34 wins & 48 nominations.",
"Poster" : "https://m.media-amazon.com/images/M/MV5BNzQzOTk3OTAtNDQ0Zi00ZTVkLWI0MTEtMDllZjNkYzNjNTc4L2ltYWdlXkEyXkFqcGdeQXVyNjU0OTQ0OTY@._V1_SX300.jpg",
"Ratings" : [
  {
    "Source" : "Internet Movie Database",
    "Value" : "8.7/10"
  },
  {
    "Source" : "Rotten Tomatoes",
    "Value" : "87%"
  },
  {
    "Source" : "Metacritic",
    "Value" : "73/100"
  }
],
"Metascore" : "73",
"imdbRating" : "8.7",
"imdbVotes" : "1,443,259",
"imdbID" : "tt0133093",
"Type" : "movie",
"DVD" : "21 Sep 1999",
"BoxOffice" : "N/A",
"Production" : "Warner Bros. Pictures",
"Website" : "http://www.whatisthematrix.com",
"Response" : "True"
}

