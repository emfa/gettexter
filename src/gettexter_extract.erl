-module(gettexter_extract).

-export([parse_transform/2]).

-define(TAB, ?MODULE).
-define(EPOT, "epot.dets").

-define(ATOM(Atom), {atom, _, Atom}).
-define(STR(Line, String), {string, Line, String}).
-define(BIN(Line, String), {bin, _, [{bin_element, _, ?STR(Line, String), default, default}]}).
-define(GETTEXT(Args), {call, _, {remote, _, ?ATOM(gettexter), ?ATOM(gettext)}, Args}).

parse_transform(Forms, Opts) ->
    case lists:member(gettext, Opts) of
        true ->
            Tab = ets:new(?TAB, []),
            true = ets:insert(Tab, {file, parse_trans:get_file(Forms)}),
            Dump = fun(Form) -> do_dump(Form, Tab) end,
            parse_trans:plain_transform(Dump, Forms),
            true = ets:delete(Tab, file),
            {ok, D} = dets:open_file(?EPOT, [{file, ?EPOT}]),
            ok = dets:insert(D, ets:tab2list(Tab)),
            true = ets:delete(Tab),
            ok = dets:close(D),
            Forms;
        false ->
            Forms
    end.

do_dump(?GETTEXT([?ATOM(Domain), _Locale, ?ATOM(undefined), ?BIN(Line, MsgID)]) = Call, Tab) ->
    dump(Domain, {simple, undefined, MsgID}, Line, Tab),
    Call;
do_dump(?GETTEXT([?ATOM(Domain), _Locale, ?BIN(Line, MsgCtxt), ?BIN(_, MsgID)]) = Call, Tab) ->
    dump(Domain, {simple, MsgCtxt, MsgID}, Line, Tab),
    Call;
do_dump(?GETTEXT([?ATOM(Domain), _Locale, ?ATOM(undefined),
                  ?BIN(Line, MsgID), ?BIN(_, MsgIDPlural), _N]) = Call, Tab) ->
    dump(Domain, {plural, undefined, MsgID, MsgIDPlural}, Line, Tab),
    Call;
do_dump(?GETTEXT([?ATOM(Domain), _Locale, ?BIN(Line, MsgCtxt),
                  ?BIN(_, MsgID), ?BIN(_, MsgIDPlural), _N]) = Call, Tab) ->
    dump(Domain, {plural, MsgCtxt, MsgID, MsgIDPlural}, Line, Tab),
    Call;
do_dump(_, _) ->
    continue.

%% @doc Dump Entry from Domain into the epot file with info of where the string
%%      where located
dump(Domain, Entry, LineNr, Tab) ->
    Key = {Domain, Entry},
    [{_, FileName}] = ets:lookup(Tab, file),
    FileInfo = case ets:lookup(Tab, Key) of
                   []           -> [];
                   [{_, FInfo}] -> FInfo
               end,
    ets:insert(Tab, {Key, [{FileName, LineNr}|FileInfo]}).
