unit TMDB;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TTMDBAPIVersion }

  TTMDBAPIVersion = (TMDBAPIv3, TMDBAPIv4);



implementation

uses fphttpclient, RegExpr, fpjsonrtti;

end.

