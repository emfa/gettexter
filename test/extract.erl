-module(extract).

-export([func/0]).

-include("../include/shortcuts.hrl").

-define(LOC, <<"en">>).

func() ->
    ?_(?LOC, <<"Hello">>),
    ?N_(?LOC, <<"Fish">>, <<"Fishes">>, 1),
    ?P_(?LOC, <<"Weather">>, <<"Warm">>),
    ?NP_(?LOC, <<"Animal">>, <<"Goat">>, <<"Goats">>, 2).
