%% @doc Uses locale/se/LC_MESSAGES/default.po
-module(gettexter_test).

-define(DOMAIN, default).

-include("shortcuts.hrl").

-include_lib("eunit/include/eunit.hrl").

gettext4_noloaded_test_() ->
    {setup, fun start/0, fun stop/1, fun gettext4_noloaded/1}.

gettext6_noloaded_test_() ->
    {setup, fun start/0, fun stop/1, fun gettext6_noloaded/1}.

gettext4_loaded_test_() ->
    {setup, fun start_load/0, fun stop/1, fun gettext4_loaded/1}.

gettext6_loaded_test_() ->
    {setup, fun start_load/0, fun stop/1, fun gettext6_loaded/1}.

gettext4_loaded(_) ->
    [?_assertEqual(<<"Hejsan">>, ?_(<<"se">>, <<"Hello">>)),
     ?_assertEqual(<<"NoTranslation">>, ?_(<<"se">>, <<"NoTranslation">>)),
     ?_assertEqual(<<"Tjena">>, ?P_(<<"se">>, <<"Context">>, <<"Hello">>)),
     ?_assertEqual(<<"NoTranslation">>, ?P_(<<"se">>, <<"Context">>, <<"NoTranslation">>))].

gettext4_noloaded(_) ->
     [?_assertEqual(<<"Hello">>, ?_(<<"se">>, <<"Hello">>)),
      ?_assertEqual(<<>>, ?_(<<"se">>, <<>>)),
      ?_assertEqual(<<"Hello">>, ?P_(<<"se">>, <<"Context">>, <<"Hello">>)),
      ?_assertEqual(<<>>, ?P_(<<"se">>, <<>>, <<>>)),
      ?_assertEqual(<<>>, ?_(<<>>, <<>>)),
      ?_assertEqual(<<>>, ?P_(<<>>, <<>>, <<>>))].

gettext6_loaded(_) ->
    [?_assertEqual(<<"Fisk">>, ?N_(<<"se">>, <<"Fish">>, <<"Fishes">>, 1)),
     ?_assertEqual(<<"Fiskar">>, ?N_(<<"se">>, <<"Fish">>, <<"Fishes">>, 2)),
     ?_assertEqual(<<"NoTranslation">>, ?N_(<<"se">>, <<"NoTranslation">>, <<"NoTranslations">>, 1)),
     ?_assertEqual(<<"NoTranslations">>, ?N_(<<"se">>, <<"NoTranslation">>, <<"NoTranslations">>, 2)),
     ?_assertEqual(<<"Get">>, ?NP_(<<"se">>, <<"Context">>, <<"Goat">>, <<"Goats">>, 1)),
     ?_assertEqual(<<"Getter">>, ?NP_(<<"se">>, <<"Context">>, <<"Goat">>, <<"Goats">>, 2)),
     ?_assertEqual(<<"NoTranslation">>, ?NP_(<<"se">>, <<"Context">>, <<"NoTranslation">>, <<"NoTranslations">>, 1)),
     ?_assertEqual(<<"NoTranslations">>, ?NP_(<<"se">>, <<"Context">>, <<"NoTranslation">>, <<"NoTranslations">>, 2))].

gettext6_noloaded(_) ->
    [?_assertEqual(<<"Fish">>, ?N_(<<"se">>, <<"Fish">>, <<"Fishes">>, 1)),
     ?_assertEqual(<<"Fishes">>, ?N_(<<"se">>, <<"Fish">>, <<"Fishes">>, 2)),
     ?_assertEqual(<<>>, ?N_(<<"se">>, <<>>, <<"Fishes">>, 1)),
     ?_assertEqual(<<>>, ?N_(<<"se">>, <<"Fish">>, <<>>, 2)),
     ?_assertEqual(<<"Goat">>, ?NP_(<<"se">>, <<"Context">>, <<"Goat">>, <<"Goats">>, 1)),
     ?_assertEqual(<<"Goats">>, ?NP_(<<"se">>, <<"Context">>, <<"Goat">>, <<"Goats">>, 2)),
     ?_assertEqual(<<>>, ?NP_(<<"se">>, <<>>, <<>>, <<"Goats">>, 1)),
     ?_assertEqual(<<>>, ?NP_(<<"se">>, <<>>, <<"Goat">>, <<>>, 2)),
     ?_assertEqual(<<>>, ?N_(<<>>, <<>>, <<>>, 1)),
     ?_assertEqual(<<>>, ?N_(<<>>, <<>>, <<>>, 2)),
     ?_assertEqual(<<>>, ?NP_(<<>>, <<>>, <<>>, <<>>, 1)),
     ?_assertEqual(<<>>, ?NP_(<<>>, <<>>, <<>>, <<>>, 2))].

start() ->
    case gettexter_server:start_link() of
         {ok, Pid} -> Pid;
         {error, {already_started, Pid}} -> Pid
    end.

start_load() ->
    Pid = start(),
    gettexter:bindtextdomain(?DOMAIN, "../test/locale"),
    gettexter:ensure_loaded(?DOMAIN, <<"se">>),
    Pid.

stop(_) ->
    ok.
