-module(extract).

-export([func/0]).

-include("../include/shortcuts.hrl").

-define(LOC, <<"en">>).

func() ->
    ?_(?LOC, <<"Hello">>),
    ?N_(?LOC, <<"Fish">>, <<"Fishes">>, 1),
    ?P_(?LOC, <<"Weather">>, <<"Warm">>),
    ?NP_(?LOC, <<"Animal">>, <<"Goat">>, <<"Goats">>, 2),
    ?NO_(<<"AString">>),
    ?NO_(<<"AContext">>, <<"AnotherString">>),
    [?NO_(<<"apa">>),
     ?NO_(<<"bepa">>),
     {?NO_(<<"cepa">>), ?NO_(<<"depa">>)}].
