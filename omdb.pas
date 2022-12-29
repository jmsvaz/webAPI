unit OMDB;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpJSON, WebQuerybase;

type

  { TOMDBAPIVersion }

  TOMDBAPIVersion = (OMDBAPIv1);

  { TOMDBAPIError }

  TOMDBAPIError = class(TCustomJSONResponse)
    private
      fError: string;
      procedure SetError(AValue: string);
    published
      property Error: string read fError write SetError;
  end;

  TOMDBAPIMovieNotFoundError = class(TOMDBAPIError);

  TOMDBAPIIncorrectIMDbIDError = class(TOMDBAPIError);


  { TOMDBRatingItem }

  TOMDBRatingItem = class(TCollectionitem)
    private
      FSource: string;
      FValue: string;
      procedure SetSource(AValue: string);
      procedure SetValue(AValue: string);
    published
      property Source: string read FSource write SetSource;
      property Value: string read FValue write SetValue;
  end;

  { TOMDBMedia }

  TOMDBMedia = class(TCustomJSONResponse)
    private
      FActors: string;
      FAwards: string;
      FCountry: string;
      FDirector: string;
      fGenre: string;
      FimdbID: string;
      FimdbRating: string;
      FimdbVotes: string;
      FLanguage: string;
      FMetascore: string;
      FPlot: string;
      FPoster: string;
      FRated: string;
      fRatings: TCollection;
      FReleased: string;
      FRuntime: string;
      fTitle: string;
      FWriter: string;
      fYear: string;
      procedure SetActors(AValue: string);
      procedure SetAwards(AValue: string);
      procedure SetCountry(AValue: string);
      procedure SetDirector(AValue: string);
      procedure SetGenre(AValue: string);
      procedure SetimdbID(AValue: string);
      procedure SetimdbRating(AValue: string);
      procedure SetimdbVotes(AValue: string);
      procedure SetLanguage(AValue: string);
      procedure SetMetascore(AValue: string);
      procedure SetPlot(AValue: string);
      procedure SetPoster(AValue: string);
      procedure SetRated(AValue: string);
      procedure SetReleased(AValue: string);
      procedure SetRuntime(AValue: string);
      procedure SetTitle(AValue: string);
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
      property Ratings: TCollection read fRatings;
      property Metascore: string read FMetascore write SetMetascore;
      property imdbRating: string read FimdbRating write SetimdbRating;
      property imdbVotes: string read FimdbVotes write SetimdbVotes;
      property imdbID: string read FimdbID write SetimdbID;
  end;

  { TOMDBMOvie }

  TOMDBMovie = class(TOMDBMedia)
    private
      FBoxOffice: string;
      FDVD: string;
      FProduction: string;
      FWebsite: string;
      procedure SetBoxOffice(AValue: string);
      procedure SetDVD(AValue: string);
      procedure SetProduction(AValue: string);
      procedure SetWebsite(AValue: string);
    published
      property DVD: string read FDVD write SetDVD;
      property BoxOffice: string read FBoxOffice write SetBoxOffice;
      property Production: string read FProduction write SetProduction;
      property Website: string read FWebsite write SetWebsite;
    end;

  { TOMDBSeries }

  TOMDBSeries = class(TOMDBMedia)
    private
      fTotalSeasons: Integer;
      procedure SetTotalSeasons(AValue: Integer);
    published
      property TotalSeasons: Integer read fTotalSeasons write SetTotalSeasons;
    end;

  { TOMDBEpisode }

  TOMDBEpisode = class(TOMDBMedia)
    private
      fEpisode: Integer;
      fSeason: Integer;
      fSeriesID: string;
      procedure SetEpisode(AValue: Integer);
      procedure SetSeason(AValue: Integer);
      procedure SetSeriesID(AValue: string);
    published
      property seriesID: string read fSeriesID write SetSeriesID;
      property Season: Integer read fSeason write SetSeason;
      property Episode: Integer read fEpisode write SetEpisode;
    end;

  { TOMDBGame }

  TOMDBGame = class(TOMDBMedia)
    private
      FBoxOffice: string;
      FDVD: string;
      FProduction: string;
      FWebsite: string;
      procedure SetBoxOffice(AValue: string);
      procedure SetDVD(AValue: string);
      procedure SetProduction(AValue: string);
      procedure SetWebsite(AValue: string);
    published
      property DVD: string read FDVD write SetDVD;
      property BoxOffice: string read FBoxOffice write SetBoxOffice;
      property Production: string read FProduction write SetProduction;
      property Website: string read FWebsite write SetWebsite;
    end;

  { TOMDBSearchItem }

  TOMDBSearchItem = class(TCollectionitem)
    private
      FimdbID: string;
      FPoster: string;
      FTitle: string;
      FYear: string;
      procedure SetimdbID(AValue: string);
      procedure SetPoster(AValue: string);
      procedure SetTitle(AValue: string);
      procedure SetYear(AValue: string);
    published
      property Title: string read FTitle write SetTitle;
      property Year: string read FYear write SetYear;
      property imdbID: string read FimdbID write SetimdbID;
//    property Type: string
      property Poster: string read FPoster write SetPoster;
    end;

  TOMDBSearch = class(TCustomJSONResponse)
    private
      fSearch: TCollection;
      FTotalResults: Integer;
      procedure SetTotalResults(AValue: Integer);
    public
      constructor Create(aJSON: string = '');
      destructor Destroy; override;
    published
      property Search: TCollection read fSearch;
      property totalResults: Integer read FTotalResults write SetTotalResults;
    end;


  TOMDB = class
    private
      fAPIKey: string;
      fTimeOut: Integer;
      fVersion: TOMDBAPIVersion;
      function YearParam(aYear: string): string;
      function TitleParam(aTitle: string): string;
      function IMDBidParam(aIMDBid: string): string;
      function SearchParam(aSearch: string): string;
      function PageParam(aPage: string): string;
      function MovieParam: string;
      function SeriesParam: string;
      function EpisodeParam: string;
      function GameParam: string;
      function RequestURL(aParams: string): string;
      function DoRequest(aURL: string): string;
      function ProcessRequest(aJSON: string): TCustomJSONResponse;
      function GetResponse(aParams: string): TCustomJSONResponse;
      procedure SetAPIKey(AValue: string);
      procedure SetTimeOut(AValue: Integer);
      procedure SetVersion(AValue: TOMDBAPIVersion);

    public
      constructor Create(aAPIKey: string = '');
      property TimeOut: Integer read fTimeOut write SetTimeOut;
      property APIKey: string read fAPIKey write SetAPIKey;
      property Version: TOMDBAPIVersion read fVersion write SetVersion;
      function GetByTitle(aTitle: string; aYear: string = ''): TCustomJSONResponse;
      function GetByIMDBid(aIMDBid: string): TCustomJSONResponse;
      function GetMovieByTitle(aTitle: string; aYear: string = ''): TCustomJSONResponse;
      function GetMovieByIMDBid(aIMDBid: string): TCustomJSONResponse;
      function GetSeriesByTitle(aTitle: string; aYear: string = ''): TCustomJSONResponse;
      function GetSeriesByIMDBid(aIMDBid: string): TCustomJSONResponse;
      function GetEpisodeByTitle(aTitle: string; aYear: string = ''): TCustomJSONResponse;
      function GetEpisodeByIMDBid(aIMDBid: string): TCustomJSONResponse;
      function GetGameByTitle(aTitle: string; aYear: string = ''): TCustomJSONResponse;
      function GetGameByIMDBid(aIMDBid: string): TCustomJSONResponse;
      function Search(aTitle: string; aYear: string = ''): TCustomJSONResponse;
      function SearchMovie(aTitle: string; aYear: string = ''): TCustomJSONResponse;
      function SearchSeries(aTitle: string; aYear: string = ''): TCustomJSONResponse;
      function SearchEpisode(aTitle: string; aYear: string = ''): TCustomJSONResponse;
      function SearchGame(aTitle: string; aYear: string = ''): TCustomJSONResponse;
  end;

implementation

uses fphttpclient;





const
  OMDBBASEURL = 'http://www.omdbapi.com/';
  OMDBVersionString: array[TOMDBAPIVersion] of string = ('1');

{ TOMDBEpisode }

procedure TOMDBEpisode.SetEpisode(AValue: Integer);
begin
  if fEpisode=AValue then Exit;
  fEpisode:=AValue;
end;

procedure TOMDBEpisode.SetSeason(AValue: Integer);
begin
  if fSeason=AValue then Exit;
  fSeason:=AValue;
end;

procedure TOMDBEpisode.SetSeriesID(AValue: string);
begin
  if fSeriesID=AValue then Exit;
  fSeriesID:=AValue;
end;

{ TOMDBSeries }

procedure TOMDBSeries.SetTotalSeasons(AValue: integer);
begin
  if fTotalSeasons=AValue then Exit;
  fTotalSeasons:=AValue;
end;

{ TOMDBGame }

procedure TOMDBGame.SetBoxOffice(AValue: string);
begin
  if FBoxOffice=AValue then Exit;
  FBoxOffice:=AValue;
end;

procedure TOMDBGame.SetDVD(AValue: string);
begin
  if FDVD=AValue then Exit;
  FDVD:=AValue;
end;

procedure TOMDBGame.SetProduction(AValue: string);
begin
  if FProduction=AValue then Exit;
  FProduction:=AValue;
end;

procedure TOMDBGame.SetWebsite(AValue: string);
begin
  if FWebsite=AValue then Exit;
  FWebsite:=AValue;
end;

{ TOMDBMOvie }

procedure TOMDBMOvie.SetDVD(AValue: string);
begin
  if FDVD=AValue then Exit;
  FDVD:=AValue;
end;

procedure TOMDBMOvie.SetProduction(AValue: string);
begin
  if FProduction=AValue then Exit;
  FProduction:=AValue;
end;

procedure TOMDBMOvie.SetWebsite(AValue: string);
begin
  if FWebsite=AValue then Exit;
  FWebsite:=AValue;
end;

procedure TOMDBMOvie.SetBoxOffice(AValue: string);
begin
  if FBoxOffice=AValue then Exit;
  FBoxOffice:=AValue;
end;

{ TOMDBSearch }

procedure TOMDBSearch.SetTotalResults(AValue: Integer);
begin
  if FTotalResults=AValue then Exit;
  FTotalResults:=AValue;
end;

constructor TOMDBSearch.Create(aJSON: string);
begin
  fSearch:= TCollection.Create(TOMDBSearchItem);
  inherited Create(aJSON);
end;

destructor TOMDBSearch.Destroy;
begin
  fSearch.Free;
  inherited Destroy;
end;

{ TOMDBSearchItem }

procedure TOMDBSearchItem.SetTitle(AValue: string);
begin
  if FTitle=AValue then Exit;
  FTitle:=AValue;
end;

procedure TOMDBSearchItem.SetimdbID(AValue: string);
begin
  if FimdbID=AValue then Exit;
  FimdbID:=AValue;
end;

procedure TOMDBSearchItem.SetPoster(AValue: string);
begin
  if FPoster=AValue then Exit;
  FPoster:=AValue;
end;

procedure TOMDBSearchItem.SetYear(AValue: string);
begin
  if FYear=AValue then Exit;
  FYear:=AValue;
end;

{ TOMDBAPIError }

procedure TOMDBAPIError.SetError(AValue: string);
begin
  if FError=AValue then Exit;
  FError:=AValue;
end;

{ TOMDBRatingItem }

procedure TOMDBRatingItem.SetSource(AValue: string);
begin
  if FSource=AValue then Exit;
  FSource:=AValue;
end;

procedure TOMDBRatingItem.SetValue(AValue: string);
begin
  if FValue=AValue then Exit;
  FValue:=AValue;
end;

{ TOMDBMovie }

constructor TOMDBMedia.Create(aJSON: string);
begin
  FRatings:= TCollection.Create(TOMDBRatingItem);
  inherited Create(aJSON);
end;

destructor TOMDBMedia.Destroy;
begin
  FRatings.Free;
  inherited Destroy;
end;

procedure TOMDBMedia.SetGenre(AValue: string);
begin
  if fGenre = AValue then Exit;
  fGenre:= AValue;
end;

procedure TOMDBMedia.SetimdbID(AValue: string);
begin
  if FimdbID=AValue then Exit;
  FimdbID:=AValue;
end;

procedure TOMDBMedia.SetimdbRating(AValue: string);
begin
  if FimdbRating=AValue then Exit;
  FimdbRating:=AValue;
end;

procedure TOMDBMedia.SetimdbVotes(AValue: string);
begin
  if FimdbVotes=AValue then Exit;
  FimdbVotes:=AValue;
end;

procedure TOMDBMedia.SetLanguage(AValue: string);
begin
  if FLanguage=AValue then Exit;
  FLanguage:=AValue;
end;

procedure TOMDBMedia.SetMetascore(AValue: string);
begin
  if FMetascore=AValue then Exit;
  FMetascore:=AValue;
end;

procedure TOMDBMedia.SetPlot(AValue: string);
begin
  if FPlot=AValue then Exit;
  FPlot:=AValue;
end;

procedure TOMDBMedia.SetPoster(AValue: string);
begin
  if FPoster=AValue then Exit;
  FPoster:=AValue;
end;

procedure TOMDBMedia.SetDirector(AValue: string);
begin
  if FDirector=AValue then Exit;
  FDirector:=AValue;
end;

procedure TOMDBMedia.SetActors(AValue: string);
begin
  if FActors=AValue then Exit;
  FActors:=AValue;
end;

procedure TOMDBMedia.SetAwards(AValue: string);
begin
  if FAwards=AValue then Exit;
  FAwards:=AValue;
end;

procedure TOMDBMedia.SetCountry(AValue: string);
begin
  if FCountry=AValue then Exit;
  FCountry:=AValue;
end;

procedure TOMDBMedia.SetRated(AValue: string);
begin
  if FRated=AValue then Exit;
  FRated:=AValue;
end;

procedure TOMDBMedia.SetReleased(AValue: string);
begin
  if FReleased=AValue then Exit;
  FReleased:=AValue;
end;

procedure TOMDBMedia.SetRuntime(AValue: string);
begin
  if FRuntime=AValue then Exit;
  FRuntime:=AValue;
end;

procedure TOMDBMedia.SetTitle(AValue: string);
begin
  if fTitle = AValue then Exit;
  fTitle:= AValue;
end;

procedure TOMDBMedia.SetWriter(AValue: string);
begin
  if FWriter=AValue then Exit;
  FWriter:=AValue;
end;

procedure TOMDBMedia.SetYear(AValue: string);
begin
  if fYear= AValue then Exit;
  fYear:= AValue;
end;

{ TOMDB }

constructor TOMDB.Create(aAPIKey: string);
begin
  Version:= OMDBAPIv1;
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
var
  y: Integer;
begin
  if (Length(aYear) > 0) and TryStrToInt(aYear,y) then
    Result:= '&y=' + EncodeURLElement(aYear);
end;


function TOMDB.TitleParam(aTitle: string): string;
begin
  Result:= '&t=' + EncodeURLElement(aTitle);
end;

function TOMDB.IMDBidParam(aIMDBid: string): string;
begin
  if ValidIMDBid(aIMDBid) then
    Result:= '&i=' + EncodeURLElement(aIMDBid);
end;

function TOMDB.SearchParam(aSearch: string): string;
begin
  Result:= '&s=' + EncodeURLElement(aSearch);
end;

function TOMDB.PageParam(aPage: string): string;
var
  p: Integer;
begin
  if TryStrToInt(aPage,p) then
    if (p > 0) and (p <= 100) then
      Result:= '&page=' + aPage;
end;

function TOMDB.MovieParam: string;
begin
  Result:= '&type=movie';
end;

function TOMDB.SeriesParam: string;
begin
  Result:= '&type=series';
end;

function TOMDB.EpisodeParam: string;
begin
  Result:= '&type=episode';
end;

function TOMDB.GameParam: string;
begin
  Result:= '&type=game';
end;

function TOMDB.RequestURL(aParams: string): string;
begin
  Result:= OMDBBASEURL + '?apikey=' + APIKey + '&v=' + OMDBVersionString[Version] +
           '$r=json' + '&plot=full' + aParams;
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

function TOMDB.ProcessRequest(aJSON: string): TCustomJSONResponse;
var
  json: TJSONObject;
  data: TJSONdata;
begin
  Result:= nil;
  try
    json:= TJSONObject(GetJSON(aJSON));

    if json.FindPath('Response').AsString = 'False' then
      begin
        if json.FindPath('Error').AsString = 'Movie not found!' then
          Result:= TOMDBAPIMovieNotFoundError.Create(aJSON)
        else
          if json.FindPath('Error').AsString = 'Incorrect IMDb ID.' then
            Result:= TOMDBAPIIncorrectIMDbIDError.Create(aJSON)
          else
            Result:= TOMDBAPIError.Create(aJSON);
      end
    else
      begin
        if json.FindPath('Response').AsString = 'True' then
          try
            data:= json.FindPath('Type');
            if Assigned(data) then
              begin
                if data.AsString = 'movie' then
                  Result:= TOMDBMovie.Create(aJSON);
                if data.AsString = 'series' then
                  Result:= TOMDBSeries.Create(aJSON);
                if data.AsString = 'episode' then
                  Result:= TOMDBEpisode.Create(aJSON);
                if data.AsString = 'game' then
                  Result:= TOMDBGame.Create(aJSON);
              end
            else
            begin
             data:= json.FindPath('Search');
            if Assigned(data) then
              Result:= TOMDBSearch.Create(aJSON);

            end;

          except
            Result:= TOMDBAPIError.Create(aJSON);
          end;
      end;
  except
    Result:= TOMDBAPIError.Create;
  end;
end;

function TOMDB.GetResponse(aParams: string): TCustomJSONResponse;
var
  completeURL: string;
  aRequest: string;
begin
  completeURL:= RequestURL(aParams);
  aRequest:= DoRequest(completeURL);
  Result:= ProcessRequest(aRequest);
end;


function TOMDB.GetByTitle(aTitle: string; aYear: string): TCustomJSONResponse;
begin
  Result:= GetResponse(TitleParam(aTitle) + YearParam(aYear));
end;

function TOMDB.GetByIMDBid(aIMDBid: string): TCustomJSONResponse;
begin
  Result:= GetResponse(IMDBidParam(aIMDBid));
end;

function TOMDB.GetMovieByTitle(aTitle: string; aYear: string
  ): TCustomJSONResponse;
begin
  Result:= GetResponse(MovieParam + TitleParam(aTitle) + YearParam(aYear));
end;

function TOMDB.GetMovieByIMDBid(aIMDBid: string): TCustomJSONResponse;
begin
  Result:= GetResponse(MovieParam + IMDBidParam(aIMDBid));
end;

function TOMDB.GetSeriesByTitle(aTitle: string; aYear: string
  ): TCustomJSONResponse;
begin
  Result:= GetResponse(SeriesParam + TitleParam(aTitle) + YearParam(aYear));
end;

function TOMDB.GetSeriesByIMDBid(aIMDBid: string): TCustomJSONResponse;
begin
  Result:= GetResponse(SeriesParam + IMDBidParam(aIMDBid));
end;

function TOMDB.GetEpisodeByTitle(aTitle: string; aYear: string
  ): TCustomJSONResponse;
begin
  Result:= GetResponse(EpisodeParam + TitleParam(aTitle) + YearParam(aYear));
end;

function TOMDB.GetEpisodeByIMDBid(aIMDBid: string): TCustomJSONResponse;
begin
  Result:= GetResponse(EpisodeParam + IMDBidParam(aIMDBid));
end;

function TOMDB.GetGameByTitle(aTitle: string; aYear: string
  ): TCustomJSONResponse;
begin
  Result:= GetResponse(GameParam + TitleParam(aTitle) + YearParam(aYear));
end;

function TOMDB.GetGameByIMDBid(aIMDBid: string): TCustomJSONResponse;
begin
  Result:= GetResponse(GameParam + IMDBidParam(aIMDBid));
end;

function TOMDB.Search(aTitle: string; aYear: string): TCustomJSONResponse;
begin               
  Result:= GetResponse(SearchParam(aTitle) + YearParam(aYear));
end;

function TOMDB.SearchMovie(aTitle: string; aYear: string): TCustomJSONResponse;
begin
  Result:= GetResponse(MovieParam + SearchParam(aTitle) + YearParam(aYear));
end;

function TOMDB.SearchSeries(aTitle: string; aYear: string): TCustomJSONResponse;
begin
  Result:= GetResponse(SeriesParam + SearchParam(aTitle) + YearParam(aYear));
end;

function TOMDB.SearchEpisode(aTitle: string; aYear: string
  ): TCustomJSONResponse;
begin
  Result:= GetResponse(EpisodeParam + SearchParam(aTitle) + YearParam(aYear));
end;

function TOMDB.SearchGame(aTitle: string; aYear: string): TCustomJSONResponse;
begin
  Result:= GetResponse(GameParam + SearchParam(aTitle) + YearParam(aYear));
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

{       // series
  "Title" : "A glitch in the Matrix",
  "Year" : "2021–",
  "Rated" : "N/A",
  "Released" : "01 May 2021",
  "Runtime" : "N/A",
  "Genre" : "Documentary",
  "Director" : "N/A",
  "Writer" : "N/A",
  "Actors" : "Samantha Goldberg, Matthew Campbell, Martyn Hale, Dave McNulty",
  "Plot" : "N/A",
  "Language" : "N/A",
  "Country" : "USA",
  "Awards" : "N/A",
  "Poster" : "https://m.media-amazon.com/images/M/MV5BM2Y1NDgwMTYtOTUzZi00M2I1LTkwMzAtOWVlNGY4NDliMzI3XkEyXkFqcGdeQXVyMTI2NDg0Mzc2._V1_SX300.jpg",
  "Ratings" : [
  ],
  "Metascore" : "N/A",
  "imdbRating" : "N/A",
  "imdbVotes" : "N/A",
  "imdbID" : "tt13285880",
  "Type" : "series",
  "totalSeasons" : "N/A",
  "Response" : "True"
}

{  // episode
  "Title" : "Tabula Rasa",
  "Year" : "2004",
  "Rated" : "TV-14",
  "Released" : "06 Oct 2004",
  "Season" : "1",
  "Episode" : "3",
  "Runtime" : "43 min",
  "Genre" : "Adventure, Drama, Fantasy, Mystery, Sci-Fi, Thriller",
  "Director" : "Jack Bender",
  "Writer" : "Jeffrey Lieber (created by), J.J. Abrams (created by), Damon Lindelof (created by), Damon Lindelof",
  "Actors" : "Naveen Andrews, Emilie de Ravin, Matthew Fox, Jorge Garcia",
  "Plot" : "Jack and Hurley discover an alarming secret about Kate, while the marshal's life hangs in the balance.",
  "Language" : "English, Korean",
  "Country" : "USA",
  "Awards" : "N/A",
  "Poster" : "https://m.media-amazon.com/images/M/MV5BMTY2NTU2MTMyN15BMl5BanBnXkFtZTgwMTgzNDY2MjE@._V1_SX300.jpg",
  "Ratings" : [
    {
      "Source" : "Internet Movie Database",
      "Value" : "8.5/10"
    }
  ],
  "Metascore" : "N/A",
  "imdbRating" : "8.5",
  "imdbVotes" : "5740",
  "imdbID" : "tt0636294",
  "seriesID" : "tt0411008",
  "Type" : "episode",
  "Response" : "True"
}

{   // game
  "Title":"Enter the Matrix",
  "Year":"2003",
  "Rated":"T",
  "Released":"15 May 2003",
  "Runtime":"N/A",
  "Genre":"Action, Adventure, Sci-Fi, Thriller",
  "Director":"Lana Wachowski, Lilly Wachowski",
  "Writer":"Lilly Wachowski, Lana Wachowski",
  "Actors":"Mary Alice, Christine Anu, Steve Bastoni, Don Battee",
  "Plot":"The game's story picks up just before The Matrix Reloaded and runs parallel to that of the film. Bend the rules of the Matrix with martial arts, pilot the fastest hovercraft in the fleet, or just fight with lots of guns.",
  "Language":"English",
  "Country":"USA",
  "Awards":"2 wins & 2 nominations.",
  "Poster":"https://m.media-amazon.com/images/M/MV5BNWM3MDU2MWQtYjdlNC00NDBlLTkyNGMtNjdhYjdlNTdiNTFlXkEyXkFqcGdeQXVyNTEwNDY2MjU@._V1_SX300.jpg",
  "Ratings":[
    {
      "Source":"Internet Movie Database",
      "Value":"6.9/10"
    }
  ],
  "Metascore":"N/A",
  "imdbRating":"6.9",
  "imdbVotes":"3,263",
  "imdbID":"tt0277828",
  "Type":"game",
  "DVD":"N/A",
  "BoxOffice":"N/A",
  "Production":"N/A",
  "Website":"N/A",
  "Response":"True"
  }


{       // search
  "Search" : [
    {
      "Title" : "The Matrix",
      "Year" : "1999",
      "imdbID" : "tt0133093",
      "Type" : "movie",
      "Poster" : "https://m.media-amazon.com/images/M/MV5BNzQzOTk3OTAtNDQ0Zi00ZTVkLWI0MTEtMDllZjNkYzNjNTc4L2ltYWdlXkEyXkFqcGdeQXVyNjU0OTQ0OTY@._V1_SX300.jpg"
    },
    {
      "Title" : "The Matrix Reloaded",
      "Year" : "2003",
      "imdbID" : "tt0234215",
      "Type" : "movie",
      "Poster" : "https://m.media-amazon.com/images/M/MV5BODE0MzZhZTgtYzkwYi00YmI5LThlZWYtOWRmNWE5ODk0NzMxXkEyXkFqcGdeQXVyNjU0OTQ0OTY@._V1_SX300.jpg"
    },
    {
      "Title" : "The Matrix Revolutions",
      "Year" : "2003",
      "imdbID" : "tt0242653",
      "Type" : "movie",
      "Poster" : "https://m.media-amazon.com/images/M/MV5BNzNlZTZjMDctZjYwNi00NzljLWIwN2QtZWZmYmJiYzQ0MTk2XkEyXkFqcGdeQXVyNTAyODkwOQ@@._V1_SX300.jpg"
    },
    {
      "Title" : "The Matrix Revisited",
      "Year" : "2001",
      "imdbID" : "tt0295432",
      "Type" : "movie",
      "Poster" : "https://m.media-amazon.com/images/M/MV5BMTIzMTA4NDI4NF5BMl5BanBnXkFtZTYwNjg5Nzg4._V1_SX300.jpg"
    },
    {
      "Title" : "Enter the Matrix",
      "Year" : "2003",
      "imdbID" : "tt0277828",
      "Type" : "game",
      "Poster" : "https://m.media-amazon.com/images/M/MV5BNWM3MDU2MWQtYjdlNC00NDBlLTkyNGMtNjdhYjdlNTdiNTFlXkEyXkFqcGdeQXVyNTEwNDY2MjU@._V1_SX300.jpg"
    },
    {
      "Title" : "The Matrix: Path of Neo",
      "Year" : "2005",
      "imdbID" : "tt0451118",
      "Type" : "game",
      "Poster" : "https://m.media-amazon.com/images/M/MV5BZGFiNGU4MjEtODM2ZC00OTg0LThkNmEtZTBlN2FkMmFjOWYzXkEyXkFqcGdeQXVyNTEwNDY2MjU@._V1_SX300.jpg"
    },
    {
      "Title" : "CR: Enter the Matrix",
      "Year" : "2009",
      "imdbID" : "tt1675286",
      "Type" : "game",
      "Poster" : "https://m.media-amazon.com/images/M/MV5BMTExMzY3NTQ1NjleQTJeQWpwZ15BbWU3MDAyMjk2NzM@._V1_SX300.jpg"
    },
    {
      "Title" : "A Glitch in the Matrix",
      "Year" : "2021",
      "imdbID" : "tt9847360",
      "Type" : "movie",
      "Poster" : "https://m.media-amazon.com/images/M/MV5BMWRhNGY3NGQtMDAxMS00YjY2LTgzOTUtZjljZmUyYWQwMGI2XkEyXkFqcGdeQXVyMDM2NDM2MQ@@._V1_SX300.jpg"
    },
    {
      "Title" : "Sex and the Matrix",
      "Year" : "2000",
      "imdbID" : "tt0274085",
      "Type" : "movie",
      "Poster" : "N/A"
    },
    {
      "Title" : "Making 'The Matrix'",
      "Year" : "1999",
      "imdbID" : "tt0365467",
      "Type" : "movie",
      "Poster" : "https://m.media-amazon.com/images/M/MV5BZjJjMTg5MTEtMDkwMy00ZjUyLTg5ODYtMmNmY2ZiNGVlZTdjXkEyXkFqcGdeQXVyODA1NjQ0OTY@._V1_SX300.jpg"
    }
  ],
  "totalResults" : "76",
  "Response" : "True"
}

