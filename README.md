gettexter - gettext for erlang
==============================

Goals of this project is:

* To be more or less compatible with GNU gettext (API and tools).
* To be as fast as possible in concurrent environment.

You may use this app to translate libraries as well as your own business apps
and use them in single installation with no conflicts.
Libraries may ship their own translations inside their distribution.

Quick Start
-----------

You definitely should be familiar with [GNU gettext](http://www.gnu.org/software/gettext/manual/gettext.html)
before start using this library.

In Erlang code include `shortcuts.hrl` header file.

```erlang
-define(DOMAIN, my_domain).
-include_lib("gettexter/include/shortcuts.hrl").
```

If ?DOMAIN is not set it will set to default.

Mark your translatable strings with:

* `?_(Locale, <<"MsgID">>)` - `gettext` (regular)
* `?N_(Locale, <<"MsgID">>, <<"MsgIDPlural">>, N)` - `ngettext` (plural)
* `?P_(Locale, <<"MsgCtxt">>, <<"MsgID">>)` - `pgettext` (respecting msgctx)
* `?NP_(Locale, <<"MsgCtxt, <<"MsgID">>, <<"MsgIDPlural">>, N)` - `npgettext` (ngettext + pgettext)
* `?D_(Domain, Locale, <<"MsgID">>)` - `dgettext` 
* `?DN_(Domain, Locale, <<"MsgID">>, <<"MsgIDPlural">>, N)` - `dngettext`
* `?DP_(Domain, Locale, <<"MsgCtxt">>, <<"MsgID">>)` - `dpgettext`
* `?DNP_(Domain, Locale, <<"MsgCtxt">>, <<"MsgID">>, <<"MsgIDPlural">>, N)` - `dnpgettext`

```erlang
% file: my_app/src/my_module.erl
-module(my_module).
-define(DOMAIN, my_app).
-include_lib("gettexter/include/shortcuts.hrl").

main(Name, What, N) ->
    Locale = <<"se">>,
    gettexter:bindtextdomain(?DOMAIN, "/../locales"), % from where load locales
    gettexter:ensure_loaded(?DOMAIN, Locale),         % ensure that our locale is loaded

    Question = case What of
                 sleep -> ?_(Locale, "Wanna sleep?");
                 eat -> ?_(Locale, "Wanna eat?")
               end,
    Time = io_lib:format(?N_(Locale, "It's ~p hour", "It's ~p hours", N), [N]),
    io:format(?_(Locale, "Hello, ~p! ~ts. ~ts"), [Name, Time, ?_(Question)]).
```

Extraction
----------

TODO

Initialize new locale's .po file by `msginit`

```bash
mkdir -p locale/ru/LC_MESSAGES/
msginit -i locale/${APP}.pot -o locale/ru/LC_MESSAGES/${APP}.po --locale=ru
```

Or actualize existing locale's .po file by `msgmerge`

```bash
msgmerge -U locale/ru/LC_MESSAGES/${APP}.po locale/${APP}.pot
```

When translations are finished, generate locale's binary .mo files by `msgfmt`

```bash
msgfmt --check -o locale/ru/LC_MESSAGES/${APP}.mo locale/ru/LC_MESSAGES/${APP}.po
```
It's **strongly recommended** to not add .mo files to your repository! So, add
`*.mo` to .gitignore / .hgignore and generate them in compile-time (by rebar
post-compile hook or so).

API
---

### Gettext lookups

```erlang
gettexter:gettext(Domain :: atom(), Locale :: binary(), MsgCtxt :: undefined | binary(), MsgID :: binary()) -> binary().
```

```erlang
gettexter:gettext(Domain :: atom(), Locale :: binary(), MsgCtxt :: undefined | binary(), MsgID :: binary(), MsgIDPlural :: binary(), N :: non_neg_integer()) -> binary().
% and other 'gettexter:d{n,p,pn}gettext' plus '?D*_' macroses
```

### Gettext configuration

```erlang
gettexter:bindtextdomain(Domain :: atom(), LocaleDir :: file:filename()) -> ok.
```
Setup directory from which .mo files will be loaded like
`${LocaleDir}/${Locale}/LC_MESSAGES/${Domain}.mo`.

By default, `Domain` is `default` and `LocaleDir` is "priv/locale".

This function don't reload already loaded locales for `Domain`, so, should be called
before `ensure_loaded` calls.

This function usualy called only once at application initialization/configuration phase.

### Gettext status

```erlang
gettexter:which_domains(Locale) -> [atom()].
```
Which domains are loaded from .mo files to gettext server for `Locale`.

```erlang
gettexter:which_locales(Domain) -> [string()].
```
Which locales are loaded from .mo files to gettext server for `Domain`.

```erlang
gettexter:ensure_loaded(Domain, Locale) ->
    {ok, already} | {ok, MoFile :: file:filename()} | {error, term()}.
```
Ensure, that locale is loaded from .mo file to gettexter server. If locale
isn't loaded, all `gettext` lookups to it will return default value `MsgID`.
This function may be called at application start-up or configuration time,
once for each supported locale.
In case of error, all data for this combination of `Domain` and `Locale` will
be removed from gettexter server to avoid incomplete/broken data.

Glossary
--------

* Domain: namespace for translations. In practice, the name of .po/.mo file, which
          in most cases, named as your OTP application.
* Locale: not strictly speaking, just name of translation's language, like "en",
          "en_GB", "ru", "pt_BR", etc. Usualy Locale contains also rules of
          plural form calculation, date/time/number formatting, currency etc.
* LC_MESSAGES: locale category, which contains translated application's strings (in
          .mo/.po format).

TODO
----

### Custom locale loaders

Maybe allow `bindtextdomain/2` 2'nd argument be a `{M, F, A}` or `fun M:F/N` to
allow locale loading customization?

### Generate .beam module with compiled-in locales for extra-fast access.

Since ETS lookups require heap copying, smth like static .beam module with
compiled-in phrases and plural rules (!!!) may  be generated.
Pros: extra fast access speed; no memory copying; compiled plural rules.
Cons: slow update; hackish.
