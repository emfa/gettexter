-module(translate_me).
-export([main/3]).

-define(DOMAIN, my_app).
-include("../include/shortcuts.hrl").
-define(OD, other_app).

main(Name, What, N) ->
    Locale = <<"ru">>,
    gettexter:bindtextdomain(?DOMAIN, "test/locale"),
    gettexter:ensure_loaded(?DOMAIN, Locale),

    Question = case What of
                 sleep -> ?_(Locale, <<"Wanna sleep?">>);
                 eat   -> ?_(Locale, <<"Wanna eat?">>)
               end,
    Suggest = case What of
                  sleep -> ?N_(Locale,
                               <<"go sleep a hour">>,
                               <<"go sleep ~p hours">>, N);
                  eat -> ?N_(Locale,
                             <<"take one babana">>,
                             <<"take ~p bananas">>, N)
              end,
    io:format(?_(Locale, <<"Hello, ~p! ~ts">>), [Name, Question]),
    io:format(?_(Locale, <<"So, you may ~ts">>), [Suggest]),
    io:format(?D_(?OD, Locale, <<"Other domain">>)),
    io:format(?DN_(?OD, Locale, <<"One other domain">>, <<"~p other domains">>, N)),
    io:format(?NP_(Locale, <<"the-context">>, <<"One other domain">>, <<"~p other domains">>, N)).
