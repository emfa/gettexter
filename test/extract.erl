-module(extract).

-export([func/0]).

-define(DOMAIN, default).
-include("../include/shortcuts.hrl").

-define(LOC, <<"en">>).

func() ->
    ?_(<<"Hello">>, ?LOC),
    ?_("Bye", ?LOC),
    ?N_(<<"Fish">>, <<"Fishes">>, 1, ?LOC),
    ?N_("Horse", "Horses", 2, ?LOC),
    ?P_(<<"Weather">>, <<"Warm">>, ?LOC),
    ?P_("Liquid", "Warm", ?LOC),
    ?NP_(<<"Animal">>, <<"Goat">>, <<"Goats">>, 2, ?LOC),
    ?NP_("Plant", "Flower", "Flowers", 3, ?LOC).
