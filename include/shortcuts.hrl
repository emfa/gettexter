-ifndef(DOMAIN).
  -define(DOMAIN, default).
-endif.

-define(_(Locale, MsgID), ?D_(?DOMAIN, Locale, MsgID)).
-define(N_(Locale, MsgID, MsgIDPlural, N), ?DN_(?DOMAIN, Locale, MsgID, MsgIDPlural, N)).
-define(P_(Locale, MsgCtxt, MsgID), ?DP_(?DOMAIN, Locale, MsgCtxt, MsgID)).
-define(NP_(Locale, MsgCtxt, MsgID, MsgIDPlural, N), ?DNP_(?DOMAIN, Locale, MsgCtxt, MsgID, MsgIDPlural, N)).

-define(NO_(MsgID), ?DNO_(?DOMAIN, undefined, MsgID)).
-define(NO_(MsgCtxt, MsgID), ?DNO_(?DOMAIN, MsgCtxt, MsgID)).

-define(D_(Domain, Locale, MsgID), gettexter:gettext(Domain, Locale, undefined, MsgID)).
-define(DN_(Domain, Locale, MsgID, MsgIDPlural, N), gettexter:gettext(Domain, Locale, undefined, MsgID, MsgIDPlural, N)).
-define(DP_(Domain, Locale, MsgCtxt, MsgID), gettexter:gettext(Domain, Locale, MsgCtxt, MsgID)).
-define(DNP_(Domain, Locale, MsgCtxt, MsgID, MsgIDPlural, N), gettexter:gettext(Domain, Locale, MsgCtxt, MsgID, MsgIDPlural, N)).

%% Identity functions, used to mark out translatable strings
-define(DNO_(Domain, MsgCtxt, MsgID), gettexter:mark(Domain, MsgCtxt, MsgID)).
