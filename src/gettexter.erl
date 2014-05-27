%%% @author Sergey Prokhorov <me@seriyps.ru>
%%% @copyright (C) 2014, Sergey Prokhorov
%%% @doc
%%% Gettexter main interface module.
%%% @end
%%% Created : 25 Feb 2014 by Sergey Prokhorov <me@seriyps.ru>

-module(gettexter).
-export([gettext/4, gettext/6]).

-export([bindtextdomain/2]).
-export([ensure_loaded/2, which_domains/1, which_locales/1]).

%% Lookup APIs

%% @doc
%% Retreive a translation for a binary string. 
%% The translation are looked up in an internal ets table. MsgCtxt
%% can be undefined if no context exists for this translation.
gettext(Domain, Locale, MsgCtxt, MsgID) ->
    case gettexter_server:gettext(Domain, Locale, MsgCtxt, MsgID) of
        undefined -> MsgID;
        MsgStr    -> MsgStr
    end.

%% @doc
%% Retrive a translation for a set of binary plural strings and a count.
%% First the compiled plural rule looked up and is then used to look up
%% the correct translation entry in the ets table. Same as for gettext/4
%% MsgCtxt should be undefined if no context exists.
gettext(Domain, Locale, MsgCtxt, MsgID, MsgIDPlural, N) ->
    case gettexter_server:gettext(Domain, Locale, MsgCtxt, MsgID, MsgIDPlural, N) of
        undefined when N == 1 -> MsgID;
        undefined             -> MsgIDPlural;
        MsgStr                -> MsgStr
    end.

%% Configuration APIs

%% @doc
%% Bind a given domain to a directory where the compiled translated
%% .mo-files are located. Should be called att configuration time,
%% otherwise the default directory priv/locale/ will be used.
bindtextdomain(Domain, LocaleDir) ->
    gettexter_server:bindtextdomain(Domain, LocaleDir).

%% @doc
%% Ensure, that locale is loaded from .mo file to gettexter server. If locale
%% isn't loaded, all `gettext' lookups to it will return default value `MsgID'.
%% This function may be called at application start-up or configuration time,
%% once for each supported locale.
ensure_loaded(Domain, Locale) ->
    gettexter_server:ensure_loaded(Domain, Locale).

%% @doc Which domains are loaded for `Locale'.
which_domains(Locale) ->
    gettexter_server:which_domains(Locale).

%% @doc Which locales are loaded for `Domain'.
which_locales(Domain) ->
    gettexter_server:which_locales(Domain).
