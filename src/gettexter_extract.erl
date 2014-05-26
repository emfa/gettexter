-module(gettexter_extract).

-export([extract/1, extract/2, extract2pots/2]).

-define(CURRENT_FILE, current_file).

extract(Files) ->
    Tab = ets:new(dump_table, []),
    extract(Files, Tab),
    L = ets:tab2list(Tab),
    ets:delete(Tab),
    L.

extract(Files, Tab) ->
    Dump = fun(File) -> dump_entries(File, Tab) end,
    lists:map(Dump, Files),
    ets:delete(Tab, ?CURRENT_FILE).

extract2pots(Files, OutDir) ->
    gettexter_po_writer:write(extract(Files), OutDir).

dump_entries(File, Tab) ->
    BaseName = filename:basename(File),
    ets:insert(Tab, {?CURRENT_FILE, BaseName}),
    {ok, [], Form} = compile:file(File, [binary, 'P']),
    file:delete(filename:rootname(BaseName) ++ ".P"),
    traverse(Form, Tab).

%% @doc Recursively walk down the abstract syntax tree and find all
%%      function calls that we want to extract strings/binaries from.
traverse([H|T], Tab) when is_list(H) ->
    traverse_list(H, Tab),
    traverse(T, Tab);
traverse({call, _, _, _} = Call, Tab) ->
    traverse_call(Call, Tab);
traverse([{call,_,_,_} = Call | T], Tab) ->
    traverse_call(Call, Tab),
    traverse(T, Tab);
traverse([{block, _, B} | T], Tab) ->
    traverse(B, Tab),
    traverse(T, Tab);
traverse([H|T], Tab) when is_tuple(H) ->
    while(size(H), H, Tab),
    traverse(T, Tab);
traverse([_|T], Tab) ->
    traverse(T, Tab);
traverse(T, Tab) when is_tuple(T) ->
    while(size(T), T, Tab);
traverse(_, _) -> ok.

traverse_list(List, Tab) ->
    lists:map(fun(X) -> traverse(X, Tab) end, List).

while(_, {block, _, B}, Tab) ->
    traverse(B, Tab);
while(N, T, Tab) when N > 0 ->
    traverse(element(N, T), Tab),
    while(N-1, T, Tab);
while(0 ,_ ,_) -> ok. 

-define(ATOM(Atom), {atom, _, Atom}).
-define(STR(Line, String), {string, Line, String}).
-define(BIN(Line, String), {bin, _, [{bin_element, _, ?STR(Line, String), default, default}]}).
-define(CALL(Module, Function, Args), {call, _, {remote, _, ?ATOM(Module), ?ATOM(Function)}, Args}).

%% @doc If we find a function call, check if it is any of the ones we can
%%      extract from and if so remove first arg (Domain) and call *_entry
%%      to retrive the entry that goes into the dump. 
%%      NOTE: Does not change anything the AST.
traverse_call(?CALL(gettexter, gettext, Args), Tab) ->
    dump_gettext(Args, Tab);
traverse_call(?CALL(gettexter, mark, Args), Tab) ->
    dump_mark(Args, Tab);
traverse_call(?CALL(_, _, Args), Tab) ->
    traverse_list(Args, Tab).

%% @doc Example:
%%      gettext(adomain, <<"se">>, undefined, <<"MsgID">>)
%%      gettext(adomain, <<"se">>, <<"MsgCtxt">>, <<"MsgID">>)
%%      gettext(adomain, <<"se">>, undefined, <<"MsgID">>, <<"MsgIDPlural">>, 2)
%%      gettext(adomain, <<"se">>, <<"MsgCtxt">>, <<"MsgID">>, <<"MsgIDPlural">>, 2)
dump_gettext([?ATOM(Domain), _Locale, ?ATOM(undefined), ?BIN(Line, MsgID)], Tab) ->
    dump(Domain, {simple, undefined, MsgID}, [Line], Tab);
dump_gettext([?ATOM(Domain), _Locale, ?BIN(Line1, MsgCtxt), ?BIN(Line2, MsgID)], Tab) ->
    dump(Domain, {simple, MsgCtxt, MsgID}, [Line1, Line2], Tab);
dump_gettext([?ATOM(Domain), _Locale, ?ATOM(undefined),
              ?BIN(Line1, MsgID), ?BIN(Line2, MsgIDPlural), _N], Tab) ->
    dump(Domain, {plural, undefined, MsgID, MsgIDPlural}, [Line1, Line2], Tab);
dump_gettext([?ATOM(Domain), _Locale, ?BIN(Line1, MsgCtxt),
              ?BIN(Line2, MsgID), ?BIN(Line3, MsgIDPlural), _N], Tab) ->
    dump(Domain, {plural, MsgCtxt, MsgID, MsgIDPlural}, [Line1, Line2, Line3], Tab);
dump_gettext(Args, Tab) ->
    traverse_list(Args, Tab).

dump_mark([?ATOM(Domain), ?ATOM(undefined), ?BIN(Line, MsgID)], Tab) ->
    dump(Domain, {simple, undefined, MsgID}, [Line], Tab);
dump_mark([?ATOM(Domain), ?BIN(Line1, MsgCtxt), ?BIN(Line2, MsgID)], Tab) ->
    dump(Domain, {simple, MsgCtxt, MsgID}, [Line1, Line2], Tab);
dump_mark(Args, Tab) ->
    traverse_list(Args, Tab).

%% @doc Dump Entry from Domain into the epot file with info of where the string
%%      where located
dump(Domain, Entry, LineNrs, Tab) ->
    Key = {Domain, Entry},
    [{_, FileName}] = ets:lookup(Tab, ?CURRENT_FILE),
    FileInfo = case ets:lookup(Tab, Key) of
                   []           -> [];
                   [{_, FInfo}] -> FInfo
               end,
    NewOb = {Key, [{FileName, nub_line_numbers(LineNrs)}|FileInfo]},
    ets:insert(Tab, NewOb).

%% @doc Remove duplicate line numbers and sort them 
nub_line_numbers(LineNrs) ->
    F = fun(LineNr, Acc) ->
                case lists:member(LineNr, Acc) of
                    true  -> Acc;
                    false -> [LineNr | Acc]
                end
        end,
    lists:reverse(lists:foldl(F, [], LineNrs)).
