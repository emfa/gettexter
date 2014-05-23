%% @doc Uses locale/se/LC_MESSAGES/default.po
-module(gettexter_test).

-define(GETTEXT_DOMAIN, default).

-include("shortcuts.hrl").

-include_lib("eunit/include/eunit.hrl").

loaded_test() ->
    {foreach, fun start_load/0, fun stop/1,
     [fun dpgettext_loaded/1,
      fun dnpgettext_loaded/1]}.

unloaded_test() ->
    {foreach, fun start/0, fun stop/1,
     [fun dpgettext_noloaded/1,
      fun dnpgettext_noloaded/1]}.

dpgettext_loaded(_) ->
    [?_assertEqual(<<"Hejsan">>, ?_(<<"Hello">>, <<"se">>)),
     ?_assertEqual(<<"NoTranslation">>, ?_(<<"NoTranslation">>, <<"se">>)),
     ?_assertEqual(<<"Tjena">>, ?P_(<<"Context">>, <<"Hello">>, <<"se">>)),
     ?_assertEqual(<<"NoTranslation">>, ?P_(<<"Context">>, <<"NoTranslation">>, <<"se">>))].

%% Calls to dpgettext/4 without loaded translations
dpgettext_noloaded(_) ->
     [?_assertEqual(<<"Hello">>, ?_(<<"Hello">>, <<"se">>)),
      ?_assertEqual(<<>>, ?_(<<>>, <<"se">>)),
      ?_assertEqual(<<"Hello">>, ?P_(<<"Context">>, <<"Hello">>, <<"se">>)),
      ?_assertEqual(<<>>, ?P_(<<>>, <<>>, <<"se">>)),
      ?_assertEqual(<<>>, ?_(<<>>, <<>>)),
      ?_assertEqual(<<>>, ?P_(<<>>, <<>>, <<>>))].

dnpgettext_loaded(_) ->
    [?_assertEqual(<<"Fisk">>, ?N_(<<"Fish">>, <<"Fishes">>, 1, <<"se">>)),
     ?_assertEqual(<<"Fiskar">>, ?N_(<<"Fish">>, <<"Fishes">>, 2, <<"se">>)),
     ?_assertEqual(<<"NoTranslation">>, ?N_(<<"NoTranslation">>, <<"NoTranslations">>, 1, <<"se">>)),
     ?_assertEqual(<<"NoTranslations">>, ?N_(<<"NoTranslation">>, <<"NoTranslations">>, 2, <<"se">>)),
     ?_assertEqual(<<"Get">>, ?NP_(<<"Context">>, <<"Goat">>, <<"Goats">>, 1, <<"se">>)),
     ?_assertEqual(<<"Getter">>, ?NP_(<<"Context">>, <<"Goat">>, <<"Goats">>, 2, <<"se">>)),
     ?_assertEqual(<<"NoTranslation">>, ?NP_(<<"Context">>, <<"NoTranslation">>, <<"NoTranslations">>, 1, <<"se">>)),
     ?_assertEqual(<<"NoTranslations">>, ?NP_(<<"Context">>, <<"NoTranslation">>, <<"NoTranslations">>, 2, <<"se">>))].

%% Calls to dnpgettext/6 withour loaded translations
dnpgettext_noloaded(_) ->
    [?_assertEqual(<<"Fish">>, ?N_(<<"Fish">>, <<"Fishes">>, 1, <<"se">>)),
     ?_assertEqual(<<"Fishes">>, ?N_(<<"Fish">>, <<"Fishes">>, 2, <<"se">>)),
     ?_assertEqual(<<>>, ?N_(<<>>, <<"Fishes">>, 1, <<"se">>)),
     ?_assertEqual(<<>>, ?N_(<<"Fish">>, <<>>, 2, <<"se">>)),
     ?_assertEqual(<<"Goat">>, ?NP_(<<"Context">>, <<"Goat">>, <<"Goats">>, 1, <<"se">>)),
     ?_assertEqual(<<"Goats">>, ?NP_(<<"Context">>, <<"Goat">>, <<"Goats">>, 2, <<"se">>)),
     ?_assertEqual(<<>>, ?NP_(<<>>, <<>>, <<"Goats">>, 1, <<"se">>)),
     ?_assertEqual(<<>>, ?NP_(<<>>, <<"Goat">>, <<>>, 2, <<"se">>)),
     ?_assertEqual(<<>>, ?N_(<<>>, <<>>, 1, <<>>)),
     ?_assertEqual(<<>>, ?N_(<<>>, <<>>, 2, <<>>)),
     ?_assertEqual(<<>>, ?NP_(<<>>, <<>>, <<>>, 1, <<>>)),
     ?_assertEqual(<<>>, ?NP_(<<>>, <<>>, <<>>, 2, <<>>))].

start() ->
    {ok, Pid} = gettexter_sup:start_link(),
    Pid.

start_load() ->
    {ok, Pid} = gettexter_sup:start_link(),
    ok = gettexter:bindtextdomain(?GETTEXT_DOMAIN, "test/locale"),
    {ok, _} = gettexter:ensure_loaded(?GETTEXT_DOMAIN, lc_messages, <<"se">>),
    Pid.

stop(Pid) ->
    exit(Pid, kill).
