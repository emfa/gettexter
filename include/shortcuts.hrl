-ifndef(GETTEXT_DOMAIN).
  -define(GETTEXT_DOMAIN, default).
-endif.

-define(_(String, Locale), ?D_(?GETTEXT_DOMAIN, String, Locale)).
-define(N_(Singular, Plural, N, Locale), ?DN_(?GETTEXT_DOMAIN, Singular, Plural, N, Locale)).
-define(P_(Context, String, Locale), ?DP_(?GETTEXT_DOMAIN, Context, String, Locale)).
-define(NP_(Context, Singular, Plural, N, Locale), ?DNP_(?GETTEXT_DOMAIN, Context, Singular, Plural, N, Locale)).

-define(D_(Domain, String, Locale), gettexter:dpgettext(Domain, undefined, String, Locale)).
-define(DN_(Domain, Singular, Plural, N, Locale), gettexter:dnpgettext(Domain, undefined, Singular, Plural, N, Locale)).
-define(DP_(Domain, Context, String, Locale), gettexter:dpgettext(Domain, Context, String, Locale)).
-define(DNP_(Domain, Context, Singular, Plural, N, Locale), gettexter:dnpgettext(Domain, Context, Singular, Plural, N, Locale)).
