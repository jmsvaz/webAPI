unit JustWatch;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fpJSON, WebQuerybase;

type

  TJustWatchAPIError = class(TCustomJSONError);
  TJustWatchAPICollectionJSONError = class(TCollectionJSONError);
  TJustWatchAPILocalesError = class(TJustWatchAPICollectionJSONError);
  TJustWatchAPIProvidesError = class(TJustWatchAPICollectionJSONError);
  TJustWatchAPIGenresError = class(TJustWatchAPICollectionJSONError);


  { TJustWatchLocaleItem }

  TJustWatchLocaleItem = class(TCollectionitem)
    private
      FCountry: string;
      FCurrency: string;
      FCurrency_Name: string;
      FFull_Locale: string;
      Fi18n_State: string;
      FISO_3166_2: string;
      FTimeZone: string;
      FTimeZone_Abbreviation: string;
      FTimeZone_Offset: string;
      FTitle_Urls_Enabled: boolean;
      procedure SetCountry(AValue: string);
      procedure SetCurrency(AValue: string);
      procedure SetCurrency_Name(AValue: string);
      procedure SetFull_Locale(AValue: string);
      procedure Seti18n_State(AValue: string);
      procedure SetISO_3166_2(AValue: string);
      procedure SetTimeZone(AValue: string);
      procedure SetTimeZone_Abbreviation(AValue: string);
      procedure SetTimeZone_Offset(AValue: string);
      procedure SetTitle_Urls_Enabled(AValue: boolean);

    published
      property Full_Locale: string read FFull_Locale write SetFull_Locale;
      property ISO_3166_2: string read FISO_3166_2 write SetISO_3166_2;
      property Country: string read FCountry write SetCountry;
      property i18n_State: string read Fi18n_State write Seti18n_State;
      property Currency: string read FCurrency write SetCurrency;
      property Currency_Name: string read FCurrency_Name write SetCurrency_Name;
      property TimeZone: string read FTimeZone write SetTimeZone;
      property TimeZone_Offset: string read FTimeZone_Offset write SetTimeZone_Offset;
      property TimeZone_Abbreviation: string read FTimeZone_Abbreviation write SetTimeZone_Abbreviation;
      property Title_Urls_Enabled: boolean read FTitle_Urls_Enabled write SetTitle_Urls_Enabled;
  end;

  TJustWatchLocales = class(TCollectionJSONResponse);

  { TJustWatchProviderItem }

  TJustWatchProviderItem = class(TCollectionitem)
    private
      FClear_Name: string;
      FIcon_Blur_Hash: string;
      FIcon_URL: string;
      FID: Integer;
      FShort_Name: string;
      FSlug: string;
      FTechnical_Name: string;
      procedure SetClear_Name(AValue: string);
      procedure SetIcon_Blur_Hash(AValue: string);
      procedure SetIcon_URL(AValue: string);
      procedure SetID(AValue: Integer);
      procedure SetShort_Name(AValue: string);
      procedure SetSlug(AValue: string);
      procedure SetTechnical_Name(AValue: string);
    published
      property ID: Integer read FID write SetID;
      property Technical_Name: string read FTechnical_Name write SetTechnical_Name;
      property Short_Name: string read FShort_Name write SetShort_Name;
      property Clear_Name: string read FClear_Name write SetClear_Name;
      property Icon_URL: string read FIcon_URL write SetIcon_URL;
      property Icon_Blur_Hash: string read FIcon_Blur_Hash write SetIcon_Blur_Hash;
      property Slug: string read FSlug write SetSlug;
  end;

  TJustWatchProviders = class(TCollectionJSONResponse);



  { TJustWatchGenreItem }

  TJustWatchGenreItem = class(TCollectionitem)
    private
      FID: Integer;
      FShort_Name: string;
      FSlug: string;
      FTechnical_Name: string;
      FTranslation: string;
      procedure SetID(AValue: Integer);
      procedure SetShort_Name(AValue: string);
      procedure SetSlug(AValue: string);
      procedure SetTechnical_Name(AValue: string);
      procedure SetTranslation(AValue: string);

    published
      property ID: Integer read FID write SetID;
      property Technical_Name: string read FTechnical_Name write SetTechnical_Name;
      property Short_Name: string read FShort_Name write SetShort_Name;
      property Translation: string read FTranslation write SetTranslation;
      property Slug: string read FSlug write SetSlug;
    end;

  TJustWatchGenres = class(TCollectionJSONResponse);


  { TJustWatch }

  TJustWatch = class
    private
      aCaption: string;
      FCountry: string;
      fLocale: string;
      fLocales: TJustWatchLocales;
      fTimeOut: Integer;
      function DoRequest(aURL: string): string;
      function DoPost(aURL: string; aJSON: string): string;
      function GetGenres: TCollectionJSONResponse;
      procedure SetCountry(AValue: string);
      procedure SetTimeOut(AValue: Integer);
      function LocalesURL: string;
      function ProvidersURL: string;
      function GenresURL: string;
      function GetLocales: TCollectionJSONResponse;
      function GetProviders: TCollectionJSONResponse;


    public
      constructor Create;
      property TimeOut: Integer read fTimeOut write SetTimeOut;
      property Caption: string read aCaption;
      property Locale: string read FLocale;
      property Country: string read FCountry write SetCountry;
      property Locales: TCollectionJSONResponse read GetLocales;
      property Providers: TCollectionJSONResponse read GetProviders;
      property Genres: TCollectionJSONResponse read GetGenres;

  {      property Countries: TTMDBCountries read fCountries;
      property Jobs: TTMDBJobs read fJobs;
      property TimeZones: TTMDBTimeZones read fTimeZones;
      property Configuration: TTMDBConfiguration read fConfiguration;
      property PrimaryTranslations: TTMDBPrimaryTranslations read fPrimaryTranslations;
      property MovieGenres: TTMDBGenreList read fMovieGenres;
      property TVGenres: TTMDBGenreList read fTVGenres;
      function GetMovie(aMovieID: string): TCustomJSONResponse;
      function GetCompany(aCompanyID: string): TCustomJSONResponse;
      function GetPerson(aPersonID: string): TCustomJSONResponse;
      function GetNetwork(aNetworkID: string): TCustomJSONResponse;
      function GetCollection(aCollectionID: string): TCustomJSONResponse;
      function GetTVShow(aTVShowID: string): TCustomJSONResponse;
      function SearchCompany(aCompany: string; aPage: Integer = 1): TCustomJSONResponse;
      function SearchMovie(aMovie: string; aPage: Integer = 1; aIncludeAdult: Boolean = False;
                           aRegion: string = ''; aYear: Integer = 0; aPrimaryReleaseYear: Integer = 0): TCustomJSONResponse;
      function SearchPerson(aPerson: string; aPage: Integer = 1; aIncludeAdult: Boolean = False;
                           aRegion: string = ''): TCustomJSONResponse;     }
  end;

implementation

uses fphttpclient, opensslsockets;

resourcestring
  sECountryNotFoundError = 'Country Not Found Error';

const
  JUSTWATCHBASEURL = 'https://apis.justwatch.com/content/';

{ TJustWatchLocaleItem }

procedure TJustWatchLocaleItem.SetCountry(AValue: string);
begin
  if FCountry=AValue then Exit;
  FCountry:=AValue;
end;

procedure TJustWatchLocaleItem.SetCurrency(AValue: string);
begin
  if FCurrency=AValue then Exit;
  FCurrency:=AValue;
end;

procedure TJustWatchLocaleItem.SetCurrency_Name(AValue: string);
begin
  if FCurrency_Name=AValue then Exit;
  FCurrency_Name:=AValue;
end;

procedure TJustWatchLocaleItem.SetFull_Locale(AValue: string);
begin
  if FFull_Locale=AValue then Exit;
  FFull_Locale:=AValue;
end;

procedure TJustWatchLocaleItem.Seti18n_State(AValue: string);
begin
  if Fi18n_State=AValue then Exit;
  Fi18n_State:=AValue;
end;

procedure TJustWatchLocaleItem.SetISO_3166_2(AValue: string);
begin
  if FISO_3166_2=AValue then Exit;
  FISO_3166_2:=AValue;
end;

procedure TJustWatchLocaleItem.SetTimeZone(AValue: string);
begin
  if FTimeZone=AValue then Exit;
  FTimeZone:=AValue;
end;

procedure TJustWatchLocaleItem.SetTimeZone_Abbreviation(AValue: string);
begin
  if FTimeZone_Abbreviation=AValue then Exit;
  FTimeZone_Abbreviation:=AValue;
end;

procedure TJustWatchLocaleItem.SetTimeZone_Offset(AValue: string);
begin
  if FTimeZone_Offset=AValue then Exit;
  FTimeZone_Offset:=AValue;
end;

procedure TJustWatchLocaleItem.SetTitle_Urls_Enabled(AValue: boolean);
begin
  if FTitle_Urls_Enabled=AValue then Exit;
  FTitle_Urls_Enabled:=AValue;
end;

{ TJustWatchProviderItem }

procedure TJustWatchProviderItem.SetID(AValue: Integer);
begin
  if FID=AValue then Exit;
  FID:=AValue;
end;

procedure TJustWatchProviderItem.SetClear_Name(AValue: string);
begin
  if FClear_Name=AValue then Exit;
  FClear_Name:=AValue;
end;

procedure TJustWatchProviderItem.SetIcon_Blur_Hash(AValue: string);
begin
  if FIcon_Blur_Hash=AValue then Exit;
  FIcon_Blur_Hash:=AValue;
end;

procedure TJustWatchProviderItem.SetIcon_URL(AValue: string);
begin
  if FIcon_URL=AValue then Exit;
  FIcon_URL:=AValue;
end;

procedure TJustWatchProviderItem.SetShort_Name(AValue: string);
begin
  if FShort_Name=AValue then Exit;
  FShort_Name:=AValue;
end;

procedure TJustWatchProviderItem.SetSlug(AValue: string);
begin
  if FSlug=AValue then Exit;
  FSlug:=AValue;
end;

procedure TJustWatchProviderItem.SetTechnical_Name(AValue: string);
begin
  if FTechnical_Name=AValue then Exit;
  FTechnical_Name:=AValue;
end;

{ TJustWatchGenreItem }

procedure TJustWatchGenreItem.SetID(AValue: Integer);
begin
  if FID=AValue then Exit;
  FID:=AValue;
end;

procedure TJustWatchGenreItem.SetShort_Name(AValue: string);
begin
  if FShort_Name=AValue then Exit;
  FShort_Name:=AValue;
end;

procedure TJustWatchGenreItem.SetSlug(AValue: string);
begin
  if FSlug=AValue then Exit;
  FSlug:=AValue;
end;

procedure TJustWatchGenreItem.SetTechnical_Name(AValue: string);
begin
  if FTechnical_Name=AValue then Exit;
  FTechnical_Name:=AValue;
end;

procedure TJustWatchGenreItem.SetTranslation(AValue: string);
begin
  if FTranslation=AValue then Exit;
  FTranslation:=AValue;
end;


{ TJustWatch } 

constructor TJustWatch.Create;
begin
  TimeOut:= 0; // Infinite timeout on most platforms
  Country:= 'Brazil';   //'United States';
  aCaption:= 'JustWatch';
end;

function TJustWatch.DoRequest(aURL: string): string;
var
  httpClient : TFPHTTPClient;
  aResult: string;
begin
  aResult:= EmptyStr;
  httpClient := TFPHTTPClient.Create(nil);
  try
    httpClient.AddHeader('Accept','application/json');
    httpClient.AddHeader('Content-Type','application/json;charset=utf-8');
    httpClient.AllowRedirect:= True;
    httpClient.IOTimeout:= Timeout;
    aResult:= httpClient.Get(aURL);
    if httpClient.ResponseStatusCode <> 200 then
      case httpClient.ResponseStatusCode of
        401: raise E401UnauthorizedError.Create(sE401UnauthorizedError);
        404: raise E404NotFoundError.Create(sE404NotFoundError); 
        429: raise E429NotFoundError.Create(sE429NotFoundError);
      else
        raise EHTTPError.Create(sEHTTPError);
      end;
  finally
    httpClient.Free;
  end;
  Result:= aResult;
end;

function TJustWatch.DoPost(aURL: string; aJSON: string): string;
var
  httpClient : TFPHTTPClient;
  aResult: string;
begin
  aResult:= EmptyStr;
  httpClient := TFPHTTPClient.Create(nil);
  try
    httpClient.AddHeader('Accept','application/json');
    httpClient.AddHeader('Content-Type','application/json;charset=utf-8');
    httpClient.AllowRedirect:= True;
    httpClient.IOTimeout:= Timeout;
    httpClient.RequestBody := TRawByteStringStream.Create(aJSON);
    aResult:= httpClient.Post(aURL);
    if httpClient.ResponseStatusCode <> 201 then
      case httpClient.ResponseStatusCode of
        401: raise E401UnauthorizedError.Create(sE401UnauthorizedError);
        404: raise E404NotFoundError.Create(sE404NotFoundError);
        429: raise E429NotFoundError.Create(sE429NotFoundError);
      else
        raise EHTTPError.Create(sEHTTPError);
      end;
  finally
    httpClient.RequestBody.Free;
    httpClient.Free;
  end;
  Result:= aResult;
end;


procedure TJustWatch.SetTimeOut(AValue: Integer);
begin
  if fTimeOut=AValue then Exit;
  fTimeOut:=AValue;
end;

function TJustWatch.LocalesURL: string;
begin
  Result:= JUSTWATCHBASEURL + 'locales/state';
end;

function TJustWatch.ProvidersURL: string;
begin
  Result:= JUSTWATCHBASEURL + 'providers/locale/' + Locale;
end;

function TJustWatch.GenresURL: string;
begin
  Result:= JUSTWATCHBASEURL + 'genres/locale/' + Locale;
end;

function TJustWatch.GetLocales: TCollectionJSONResponse;
var
  aRequest: string;
begin
  try
    aRequest:= DoRequest(LocalesURL);
    Result:= TJustWatchLocales.Create(TJustWatchLocaleItem, aRequest);
  except
    Result:= TJustWatchAPILocalesError.Create(TJustWatchLocaleItem);
  end;
end;

function TJustWatch.GetProviders: TCollectionJSONResponse;
var
  aRequest: string;
begin
  try
    aRequest:= DoRequest(ProvidersURL);
    Result:= TJustWatchProviders.Create(TJustWatchProviderItem, aRequest);
  except
    Result:= TJustWatchAPIProvidesError.Create(TJustWatchProviderItem);
  end;
end;


function TJustWatch.GetGenres: TCollectionJSONResponse;
var
  aRequest: string;
begin
  try
    aRequest:= DoRequest(GenresURL);
    Result:= TJustWatchGenres.Create(TJustWatchGenreItem, aRequest);
  except
    Result:= TJustWatchAPIGenresError.Create(TJustWatchGenreItem);
  end;
end;

procedure TJustWatch.SetCountry(AValue: string);
var
  aLocale: TCollectionItem;
begin
  if FCountry=AValue then Exit;

  for aLocale in Locales do
    begin
      if TJustWatchLocaleItem(aLocale).Country = AValue then
        begin
          FCountry:= AValue;
          FLocale:= TJustWatchLocaleItem(aLocale).Full_Locale;
          System.Break;
        end;
    end;
  if FCountry = EmptyStr then
    raise Exception.Create(sECountryNotFoundError);
end;

end.

