-ifndef(GETTEXT_DOMAIN).
  %% regular gettext call
  -define(_(String), gettexter:gettext(String)).

  %% ngettext - plural
  -define(N_(Singular, Plural, N), gettexter:ngettext(Singular, Plural, N)).

  %% pgettext (with msgctx)
  -define(P_(Context, String), gettexter:pgettext(Context, String)).
  -define(NP_(Context, Singular, Plural, N), gettexter:npgettext(Context, Singular, Plural, N)).
-else.
  -define(_(String), ?D_(?GETTEXT_DOMAIN, String)).
  -define(_(String, Locale), ?D_(?GETTEXT_DOMAIN, String, Locale)).
  -define(N_(Singular, Plural, N), ?DN_(?GETTEXT_DOMAIN, Singular, Plural, N)).
  -define(N_(Singular, Plural, N, Locale), ?DN_(?GETTEXT_DOMAIN, Singular, Plural, N, Locale)).
  -define(P_(Context, String), ?DP_(?GETTEXT_DOMAIN, Context, String)).
  -define(P_(Context, String, Locale), ?DP_(?GETTEXT_DOMAIN, Context, String, Locale)).
  -define(NP_(Context, Singular, Plural, N), ?DNP_(?GETTEXT_DOMAIN, Context, Singular, Plural, N)).
  -define(NP_(Context, Singular, Plural, N, Locale), ?DNP_(?GETTEXT_DOMAIN, Context, Singular, Plural, N, Locale)).
-endif.

%% d*gettext - all the same, but with domain specified (should be used for library localization)
-define(D_(Domain, String), gettexter:dgettext(Domain, String)).
-define(D_(Domain, String, Locale), gettexter:dpgettext(Domain, undefined, String, Locale)).
-define(DN_(Domain, Singular, Plural, N), gettexter:dngettext(Domain, Singular, Plural, N)).
-define(DN_(Domain, Singular, Plural, N, Locale), gettexter:dnpgettext(Domain, undefined, Singular, Plural, N, Locale)).
-define(DP_(Domain, Context, String), gettexter:dpgettext(Domain, Context, String)).
-define(DP_(Domain, Context, String, Locale), gettexter:dpgettext(Domain, Context, String, Locale)).
-define(DNP_(Domain, Context, Singular, Plural, N), gettexter:dnpgettext(Domain, Context, Singular, Plural, N)).
-define(DNP_(Domain, Context, Singular, Plural, N, Locale), gettexter:dnpgettext(Domain, Context, Singular, Plural, N, Locale)).

%% gettext noop
-define(NO_(String), String).
