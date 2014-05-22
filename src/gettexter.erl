%%% @author Sergey Prokhorov <me@seriyps.ru>
%%% @copyright (C) 2014, Sergey Prokhorov
%%% @doc
%%% Gettexter main interface module.
%%% @end
%%% Created : 25 Feb 2014 by Sergey Prokhorov <me@seriyps.ru>

-module(gettexter).
-export([gettext/1, pgettext/2, dgettext/2, dpgettext/3]).
-export([ngettext/3, npgettext/4, dngettext/4, dnpgettext/5]).
-export([dpgettext/4, dnpgettext/6]).

-export([bindtextdomain/2]).
-export([setlocale/2, getlocale/1]).
-export([textdomain/0, textdomain/1]).
-export([bind_textdomain_codeset/2]).
-export([which_domains/1, which_locales/1, ensure_loaded/3, reset/0]).

%% Lookup APIs
gettext(Msgid) -> dpgettext(undefined, undefined, Msgid).
ngettext(Singular, Plural, N) -> dnpgettext(undefined, undefined, Singular, Plural, N).
pgettext(Context, Msgid) -> dpgettext(undefined, Context, Msgid).
npgettext(Context, Singular, Plural, N) -> dnpgettext(undefined, Context, Singular, Plural, N).

dgettext(Domain, Msgid) -> dpgettext(Domain, undefined, Msgid).
dngettext(Domain, Singular, Plural, N) -> dnpgettext(Domain, undefined, Singular, Plural, N).

dpgettext(Domain, Context, MsgID) ->
    Locale = getlocale(lc_messages),
    Domain1 = if Domain == undefined -> textdomain();
                 true -> Domain
              end,
    dpgettext(Domain1, Locale, Context, MsgID).

dpgettext(Domain, Context, MsgID, Locale) when (Context == undefined) orelse is_binary(Context),
                                               is_binary(MsgID),
                                               is_binary(Locale) ->
    case gettexter_server:dpgettext(Domain, Context, Locale, MsgID) of
        undefined -> MsgID;
        MsgStr    -> MsgStr
    end;
dpgettext(Domain, Context0, MsgID, Locale) ->
    Context = case Context0 of
                  undefined -> undefined;
                  _         -> list_to_binary(Context0)
              end,
    Bin = dpgettext(Domain, Context, list_to_binary(MsgID), list_to_binary(Locale)),
    binary_to_list(Bin).

dnpgettext(Domain, Context, Singular, Plural, N) ->
    Locale = getlocale(lc_messages),
    Domain1 = if Domain == undefined -> textdomain();
                  true -> Domain
              end,
    dnpgettext(Domain1, Context, Singular, Plural, N, Locale).
    
dnpgettext(Domain, Context, Singular, Plural, N, Locale) when (Context == undefined) orelse is_binary(Context),
                                                              is_binary(Singular),
                                                              is_binary(Plural),
                                                              is_binary(Locale) ->
    case gettexter_server:dnpgettext(Domain, Context, Locale, Singular, Plural, N) of
        undefined when N == 1 -> Singular;
        undefined             -> Plural;
        MsgStr                -> MsgStr
    end;
dnpgettext(Domain, Context0, Singular, Plural, N, Locale) ->
    Context = case Context0 of
                  undefined -> undefined;
                  _         -> list_to_binary(Context0)
              end,
    Bin = dnpgettext(Domain, Context, list_to_binary(Singular), list_to_binary(Plural), N, list_to_binary(Locale)),
    binary_to_list(Bin).

%% TODO: add `*gettext(..., Locale)' functions here (locale from args, not PD).

%% Configuration APIs
bindtextdomain(Domain, LocaleDir) ->
    gettexter_server:bindtextdomain(Domain, LocaleDir).

setlocale(Category=lc_messages, Locale) ->
    TextDomain = textdomain(),
    true = (TextDomain =/= undefined),          %assert
    put({?MODULE, locale, TextDomain, Category}, Locale),
    {ok, _} = gettexter_server:ensure_loaded(TextDomain, Category, Locale),
    ok.

getlocale(Category=lc_messages) ->
    TextDomain = textdomain(),
    get({?MODULE, locale, TextDomain, Category}).

textdomain(Domain) ->
    put({?MODULE, textdomain}, Domain).

textdomain() ->
    get({?MODULE, textdomain}).

bind_textdomain_codeset(_Domain, _Charset) ->
    error(not_implemented).

%% Proprietary APIs (not defined in GNU gettext, but may be usefull for Erlang apps).

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
ensure_loaded(TextDomain, Category=lc_messages, Locale) when is_binary(Locale) ->
    gettexter_server:ensure_loaded(TextDomain, Category, Locale);
ensure_loaded(TextDomain, Category, Locale) ->
    {ok, BinFileName} = ensure_loaded(TextDomain, Category, list_to_binary(Locale)),
    {ok, binary_to_list(BinFileName)}.

%% @doc
%% Remove all gettext stuff from process dictionary (but not from locale data storage).
-spec reset() -> ok.
reset() ->
    lists:foreach(fun({Key, _}) when element(1, Key) == ?MODULE -> erase(Key);
                     (_) -> ok
                  end,
                 get()),
    ok.
%% Internal
%% maybe add some abstraction around get/put here?
