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
      fJSONData: TJSONData;
      FResponse: Boolean;
      function GetJSON: string;
      procedure SetResponse(AValue: Boolean);
      procedure SetJSON(AValue: string); virtual;
      procedure DeStream;
    public
      constructor Create(aJSON: string = '');
      destructor Destroy; override;
      function FormatJSON: string;
      property JSON: string read GetJSON write SetJSON;
      property Response: Boolean read FResponse write SetResponse;
    end;

  { TOMDBMovie }

  TOMDBMovie = class(TOMDBResponse)
    private
      FActors: string;
      FAwards: string;
      FBoxOffice: string;
      FCountry: string;
      FDirector: string;
      FDVD: string;
      fGenre: string;
      FimdbID: string;
      FimdbRating: string;
      FimdbVotes: string;
      FLanguage: string;
      FMetascore: string;
      FPlot: string;
      FPoster: string;
      FProduction: string;
      FRated: string;
      FRatings: TStrings;
      FReleased: string;
      FRuntime: string;
      fTitle: string;
      FWebsite: string;
      FWriter: string;
      fYear: string;
      procedure SetActors(AValue: string);
      procedure SetAwards(AValue: string);
      procedure SetBoxOffice(AValue: string);
      procedure SetCountry(AValue: string);
      procedure SetDirector(AValue: string);
      procedure SetDVD(AValue: string);
      procedure SetGenre(AValue: string);
      procedure SetimdbID(AValue: string);
      procedure SetimdbRating(AValue: string);
      procedure SetimdbVotes(AValue: string);
      procedure SetLanguage(AValue: string);
      procedure SetMetascore(AValue: string);
      procedure SetPlot(AValue: string);
      procedure SetPoster(AValue: string);
      procedure SetProduction(AValue: string);
      procedure SetRated(AValue: string);
      procedure SetReleased(AValue: string);
      procedure SetRuntime(AValue: string);
      procedure SetTitle(AValue: string);
      procedure SetWebsite(AValue: string);
      procedure SetWriter(AValue: string);
      procedure SetYear(AValue: string);
    public
      constructor Create(aJSON: string = '');
      destructor Destroy; override;
    published
      property Year: string read fYear write SetYear;
      property Title: string read fTitle write SetTitle;
      property Rated: string read FRated write SetRated;
      property Released: string read FReleased write SetReleased;
      property Runtime: string read FRuntime write SetRuntime;
      property Genre: string read fGenre write SetGenre;
      property Director: string read FDirector write SetDirector;
      property Writer: string read FWriter write SetWriter;
      property Actors: string read FActors write SetActors;
      property Plot: string read FPlot write SetPlot;
      property Language: string read FLanguage write SetLanguage;
      property Country: string read FCountry write SetCountry;
      property Awards: string read FAwards write SetAwards;
      property Poster: string read FPoster write SetPoster;
//      property Ratings: TStrings read FRatings;
      property Metascore: string read FMetascore write SetMetascore;
      property imdbRating: string read FimdbRating write SetimdbRating;
      property imdbVotes: string read FimdbVotes write SetimdbVotes;
      property imdbID: string read FimdbID write SetimdbID;
      property DVD: string read FDVD write SetDVD;
      property BoxOffice: string read FBoxOffice write SetBoxOffice;
      property Production: string read FProduction write SetProduction;
      property Website: string read FWebsite write SetWebsite;
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
      function GetByIMDBid(aIMDBid: string): TOMDBMovie;
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

constructor TOMDBMovie.Create(aJSON: string);
begin
  inherited Create(aJSON);
  FRatings:= TStringList.Create;
end;

destructor TOMDBMovie.Destroy;
begin
  FRatings.Free;
  inherited Destroy;
end;

procedure TOMDBMovie.SetGenre(AValue: string);
begin
  if fGenre = AValue then Exit;
  fGenre:= AValue;
end;

procedure TOMDBMovie.SetimdbID(AValue: string);
begin
  if FimdbID=AValue then Exit;
  FimdbID:=AValue;
end;

procedure TOMDBMovie.SetimdbRating(AValue: string);
begin
  if FimdbRating=AValue then Exit;
  FimdbRating:=AValue;
end;

procedure TOMDBMovie.SetimdbVotes(AValue: string);
begin
  if FimdbVotes=AValue then Exit;
  FimdbVotes:=AValue;
end;

procedure TOMDBMovie.SetLanguage(AValue: string);
begin
  if FLanguage=AValue then Exit;
  FLanguage:=AValue;
end;

procedure TOMDBMovie.SetMetascore(AValue: string);
begin
  if FMetascore=AValue then Exit;
  FMetascore:=AValue;
end;

procedure TOMDBMovie.SetPlot(AValue: string);
begin
  if FPlot=AValue then Exit;
  FPlot:=AValue;
end;

procedure TOMDBMovie.SetPoster(AValue: string);
begin
  if FPoster=AValue then Exit;
  FPoster:=AValue;
end;

procedure TOMDBMovie.SetProduction(AValue: string);
begin
  if FProduction=AValue then Exit;
  FProduction:=AValue;
end;

procedure TOMDBMovie.SetDirector(AValue: string);
begin
  if FDirector=AValue then Exit;
  FDirector:=AValue;
end;

procedure TOMDBMovie.SetDVD(AValue: string);
begin
  if FDVD=AValue then Exit;
  FDVD:=AValue;
end;

procedure TOMDBMovie.SetActors(AValue: string);
begin
  if FActors=AValue then Exit;
  FActors:=AValue;
end;

procedure TOMDBMovie.SetAwards(AValue: string);
begin
  if FAwards=AValue then Exit;
  FAwards:=AValue;
end;

procedure TOMDBMovie.SetBoxOffice(AValue: string);
begin
  if FBoxOffice=AValue then Exit;
  FBoxOffice:=AValue;
end;

procedure TOMDBMovie.SetCountry(AValue: string);
begin
  if FCountry=AValue then Exit;
  FCountry:=AValue;
end;

procedure TOMDBMovie.SetRated(AValue: string);
begin
  if FRated=AValue then Exit;
  FRated:=AValue;
end;

procedure TOMDBMovie.SetReleased(AValue: string);
begin
  if FReleased=AValue then Exit;
  FReleased:=AValue;
end;

procedure TOMDBMovie.SetRuntime(AValue: string);
begin
  if FRuntime=AValue then Exit;
  FRuntime:=AValue;
end;

procedure TOMDBMovie.SetTitle(AValue: string);
begin
  if fTitle = AValue then Exit;
  fTitle:= AValue;
end;

procedure TOMDBMovie.SetWebsite(AValue: string);
begin
  if FWebsite=AValue then Exit;
  FWebsite:=AValue;
end;

procedure TOMDBMovie.SetWriter(AValue: string);
begin
  if FWriter=AValue then Exit;
  FWriter:=AValue;
end;

procedure TOMDBMovie.SetYear(AValue: string);
begin
  if fYear= AValue then Exit;
  fYear:= AValue;
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

function TOMDBResponse.FormatJSON: string;
begin
  if Assigned(fJSONData) then
    Result:= fJSONData.FormatJSON();
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
  aRequest: string;
begin
  completeURL:= RequestURL('&type=movie&plot=full&t=' + EncodeURLElement(aTitle) + YearParam(aYear));
  // TODO: better error check
  aRequest:= DoRequest(completeURL);
  Result:= TOMDBMovie.Create(aRequest);
end;

function TOMDB.GetByIMDBid(aIMDBid: string): TOMDBMovie;
var
  completeURL: string;
  aRequest: string;
begin
  if not ValidIMDBid(aIMDBid) then Exit;
  completeURL:= RequestURL('&plot=full&i=' + EncodeURLElement(aIMDBid));
  // TODO: better error check
  aRequest:= DoRequest(completeURL);
  Result:= TOMDBMovie.Create(aRequest);
end;

function TOMDB.Search(aTitle: string; aYear: string): TOMDBResponse;
var
  completeURL: string;
  aRequest: string;
begin
  completeURL:= RequestURL('&plot=full&s=' + EncodeURLElement(aTitle) + YearParam(aYear));
  // TODO: better error check
  aRequest:= DoRequest(completeURL);
  Result:= TOMDBResponse.Create(aRequest);
end;

function TOMDB.SearchMovie(aTitle: string; aYear: string): TOMDBResponse;
var
  completeURL: string;
  aRequest: string;
begin
  completeURL:= RequestURL('&type=movie&plot=full&s=' + EncodeURLElement(aTitle) + YearParam(aYear));
  // TODO: better error check
  aRequest:= DoRequest(completeURL);
  Result:= TOMDBResponse.Create(aRequest);
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

{
procedure DumpJSONData(J: TJSonData; DOEOLN: Boolean);
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
}
