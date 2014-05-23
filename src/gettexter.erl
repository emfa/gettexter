%%% @author Sergey Prokhorov <me@seriyps.ru>
%%% @copyright (C) 2014, Sergey Prokhorov
%%% @doc
%%% Gettexter main interface module.
%%% @end
%%% Created : 25 Feb 2014 by Sergey Prokhorov <me@seriyps.ru>

-module(gettexter).
-export([dpgettext/4, dnpgettext/6]).

-export([bindtextdomain/2]).
-export([which_domains/1, which_locales/1, ensure_loaded/3]).

%% Lookup APIs

dpgettext(Domain, Context, MsgID, Locale) ->
    case gettexter_server:dpgettext(Domain, Context, Locale, MsgID) of
        undefined -> MsgID;
        MsgStr    -> MsgStr
    end.

dnpgettext(Domain, Context, Singular, Plural, N, Locale) ->
    case gettexter_server:dnpgettext(Domain, Context, Locale, Singular, Plural, N) of
        undefined when N == 1 -> Singular;
        undefined             -> Plural;
        MsgStr                -> MsgStr
    end.

%% Configuration APIs
bindtextdomain(Domain, LocaleDir) ->
    gettexter_server:bindtextdomain(Domain, LocaleDir).

%% @doc
%% Which domains are loaded for `Locale'.
which_domains(Locale) ->
    gettexter_server:which_domains(Locale).

%% @doc
%% Which locales are loaded for `Domain'.
which_locales(Domain) ->
    gettexter_server:which_locales(Domain).

%% @doc
%% Ensure, that locale is loaded from .mo file to gettexter server. If locale
%% isn't loaded, all `gettext' lookups to it will return default value `Msgid'.
%% This function may be called at application start-up or configuration time,
%% once for each supported locale.
ensure_loaded(TextDomain, Category=lc_messages, Locale) ->
    gettexter_server:ensure_loaded(TextDomain, Category, Locale).
