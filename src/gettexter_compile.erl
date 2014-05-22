%% -------------------------------------------------------------------------
%% Permission is hereby granted, free of charge, to any person obtaining a
%% copy of this software and associated documentation files (the
%% "Software"), to deal in the Software without restriction, including
%% without limitation the rights to use, copy, modify, merge, publish,
%% distribute, sublicense, and/or sell copies of the Software, and to permit
%% persons to whom the Software is furnished to do so, subject to the
%% following conditions:
%%
%% The above copyright notice and this permission notice shall be included
%% in all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
%% OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
%% MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN
%% NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
%% DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
%% OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE
%% USE OR OTHER DEALINGS IN THE SOFTWARE.
%%
%% @copyright 2003 Torbjörn Törnkvist
%% @author Torbjörn Törnkvist <tobbe@tornkvist.org>
%% @author Emil Falk <emil.falk@textalk.se>
%% @doc Parse transform for gettexter and conversion from `epot'- to `pot'-files.

-module(gettexter_compile).
-export([parse_transform/2, epot2pots/0, epot2pots/2]).

-define(EPOT_TABLE, epot).
-define(EPOT_FILE, "epot.dets").
-define(DEFAULT_DIR, "locale").

-define(ENDCOL, 72).
-define(PIVOT, 4).
-define(SEP, $\s).

%%% --------------------------------------------------------------------
%%% From the Erlang po-Template file, create a GNU po-file for each domain.
%%% --------------------------------------------------------------------

%% @doc Assuming that some files has been compiled with the parse_transform
%%      enabled and the default options for where to write all the gettext-
%%      entries has been used(i.e locale/epot.dets). Use that file to create
%%      one .pot file for each domain that has been encountered during
%%      compilation and output these into the default directory (locale/).
epot2pots() ->
    epot2pots(filename:join([?DEFAULT_DIR, ?EPOT_FILE]), ?DEFAULT_DIR).

%% @doc Same as epot2pots/0 but both input file and output directory are given
%%      as arguments
epot2pots(Epot, OutputDir) ->
    open_epot_file(Epot),
    Domains = group_domains(get_epot_data()),
    close_epot_file(),
    [write_pot_file(OutputDir, Domain, Entries) || {Domain, Entries} <- Domains],
    ok.

%%% ---------------------------------------------------------------------------
%%% Serializing a .pot file
%%% ---------------------------------------------------------------------------

%% @doc Write a Domain.pot (i.e user.pot) into Dir and populated by Entries
write_pot_file(Dir, Domain, Entries) ->
    {ok, Fd} = open_pot_file(Dir, Domain),
    write_header(Fd),
    write_entries(Fd, Entries),
    file:close(Fd).

group_domains([]) ->
    [];
group_domains(Entries) ->
    {{Domain, _}, _} = hd(Entries), 
    {Same, NotSame} = lists:partition(fun({{D, _}, _}) -> D == Domain end, Entries),
    [{Domain, [{E, I} || {{_, E}, I} <- Same]} | group_domains(NotSame)].


open_pot_file(Dir, Domain) ->
    PotFile = filename:join([Dir, atom_to_list(Domain) ++ ".pot"]),
    filelib:ensure_dir(PotFile),
    file:delete(PotFile),
    file:open(PotFile, [write]).

is_multiline(Str) ->
    lists:member($\n, Str).

pretty_entry(Entry, Str) ->
    case is_multiline(Str) orelse length(Str) > ?ENDCOL of
        true  -> Entry ++ " \"\"\n" ++ pretty(Str);
        false -> Entry ++ " " ++ pretty(Str)
    end.

msgid(Str) ->
    pretty_entry("msgid", Str).

msgid_plural(Str) ->
    pretty_entry("msgid_plural", Str).

msgstr() ->
    "msgstr \"\"\n".

msgstr_i(I) ->
    "msgstr[" ++ integer_to_list(I) ++ "] \"\"\n".

msgstr_plural(Ixs) ->
    lists:concat(lists:map(fun msgstr_i/1, Ixs)).

msgctxt(Str) ->
    pretty_entry("msgctxt", Str).

write_entry(Fd, {simple, MsgID}) ->
    file:write(Fd, msgid(MsgID)),
    file:write(Fd, msgstr());
write_entry(Fd, {simple, Context, MsgID}) ->
    file:write(Fd, msgctxt(Context)),
    write_entry(Fd, {simple, MsgID});
write_entry(Fd, {plural, Singular, Plural}) ->
    file:write(Fd, msgid(Singular)),
    file:write(Fd, msgid_plural(Plural)),
    file:write(Fd, msgstr_plural(lists:seq(0,1)));
write_entry(Fd, {plural, Context, Singular, Plural}) ->
    file:write(Fd, msgctxt(Context)),
    write_entry(Fd, {plural, Singular, Plural}).

write_entries(Fd, Entries) ->
    WriteEntry = fun({Entry, _Refs}) ->
                         write_entry(Fd, Entry),
                         file:write(Fd, "\n")
                 end,
    lists:foreach(WriteEntry, Entries).

pretty(Str) ->
    pretty(Str, []).

pretty([], Acc) ->
    lists:concat(lists:reverse(Acc));
pretty(Str, Acc) when length(Str) =< ?ENDCOL ->
    pretty([], [pretty_string(Str)|Acc]);
pretty(Str, Acc) ->
    {Line, Rest} = get_line(Str),
    pretty(Rest, [pretty_string(Line)|Acc]).

pretty_string(Str) ->
    "\"" ++ escape_chars(Str) ++ "\"\n".

escape_chars(Str) ->
    F = fun($", Acc)  -> [$\\,$"|Acc];
           ($\\, Acc) -> [$\\,$\\|Acc];
           ($\n, Acc) -> [$\\,$n|Acc];
	   (C, Acc)   -> [C|Acc]
	end,
    lists:foldr(F, [], Str).

%%% Split the string into substrings,
%%% aligned around a specific column.
get_line(Str) ->
    get_line(Str, ?SEP, 1, ?ENDCOL, []).

%%% End of string reached.
get_line([], _Sep, _N, _End, Acc) ->
    {lists:reverse(Acc), []};
%%% Eat characters.
get_line([H|T], Sep, N, End, Acc) when N < End ->
    get_line(T, Sep, N+1, End, [H|Acc]);
%%% Ended with a Separator on the End boundary.
get_line([Sep|T], Sep, End, End, Acc) ->
    {lists:reverse([Sep|Acc]), T};
%%% At the end, try to find end of token within
%%% the given constraint, else backup one token.
get_line([H|T] = In, Sep, End, End, Acc) ->
    case find_end(T, Sep) of
	{true, Racc, Rest} ->
	    {lists:reverse(Racc ++ [H|Acc]), Rest};
	false ->
	    case reverse_tape(Acc, In) of
		{true, Bacc, Rest} ->
		    {lists:reverse(Bacc), Rest};
		{false,Str} ->
		    %%% Ugh...the word is longer than ENDCOL...
		    split_string(Str, ?ENDCOL)
	    end
    end.

find_end(Str, Sep) ->
    find_end(Str, Sep, 1, ?PIVOT, []).

find_end([Sep|T], Sep, N, Pivot, Acc) when N =< Pivot ->
    {true, [Sep|Acc], T};
find_end(_Str, _Sep, N, Pivot, _Acc) when N > Pivot ->
    false;
find_end([H|T], Sep, N, Pivot, Acc) ->
    find_end(T, Sep, N+1, Pivot, [H|Acc]);
find_end([], _Sep, _N, _Pivot, Acc) ->
    {true, Acc, []}.

reverse_tape(Acc, Str) ->
    reverse_tape(Acc, Str, ?SEP).

reverse_tape([Sep|_T] = In, Str, Sep) ->
    {true, In, Str};
reverse_tape([H|T], Str, Sep) ->
    reverse_tape(T, [H|Str], Sep);
reverse_tape([], Str, _Sep) ->
    {false, Str}.

split_string(Str, End) ->
    split_string(Str, End, 1, []).

split_string(Str, End, End, Acc) ->
    {lists:reverse(Acc), Str};
split_string([H|T], End, N, Acc) when N < End ->
    split_string(T, End, N+1, [H|Acc]);
split_string([], _End, _N, Acc) ->
    {lists:reverse(Acc), []}.

write_header(Fd) ->
    io:format(Fd, mk_header(), []).

%% @doc The pot header file
%%      TODO: Dynamic
mk_header() ->
    "# SOME DESCRIPTIVE TITLE.\n"
        "# Copyright (C) YEAR THE PACKAGE'S COPYRIGHT HOLDER\n"
        "# This file is distributed under the same license as the "
        "PACKAGE package.\n"
        "# FIRST AUTHOR <EMAIL@ADDRESS>, YEAR.\n"
        "#\n"
        "# NB: Consider using poEdit <http://poedit.sourceforge.net>\n"
        "#\n"
        "#\n"
        "#, fuzzy\n"
        "msgid \"\"\n"
        "msgstr \"\"\n"
        "\"Project-Id-Version: PACKAGE VERSION\\n\"\n"
        "\"POT-Creation-Date: 2003-10-21 16:45+0200\\n\"\n"
        "\"PO-Revision-Date: YEAR-MO-DA HO:MI+ZONE\\n\"\n"
        "\"Last-Translator: FULL NAME <EMAIL@ADDRESS>\\n\"\n"
        "\"Language-Team: LANGUAGE <LL@li.org>\\n\"\n"
        "\"Language: LC\\n\""
        "\"MIME-Version: 1.0\\n\"\n"
        "\"Content-Type: text/plain; charset=UTF-8\\n\"\n"
        "\"Content-Transfer-Encoding: 8bit\\n\"\n"
        "\"Plural-Forms: nplurals=2; plural=(n != 1);\\n\"\n\n".

%%% --------------------------------------------------------------------
%%% Parse transformation
%%% --------------------------------------------------------------------

%% @doc Parse transformation for extracting gettext strings from erlang code.
%%      Both strings and binary literals may be used, although not simultaneously.
%%      It probably won't work to mix them and that behaviour is not defined so
%%      use either only binaries or only strings. That goes for Locale, Context,
%%      MsgID and Singular/Plural. The parse transform does not check that all
%%      calls are well formed so be consistent (although it does some checks).
%%      For the extraction to work all arguments must be literals, binaries with
%%      multiple string literal elements are also supported.
%%      
%%      Options:
%%          gettext:      This flag must be set for the dumping to occur
%%          {epot, File}: Choose where to output the epot file
%%
%%      TODO:
%%      Use parse_trans lib to make the transform nicer?
%%      As of now strings are only extracted from the calls to dpgettext/4 and
%%      dnpgettext/6. So it is adviced to use macros to choose maybe a default
%%      domain and strings without context etc.
%%      In the future maybe we can pass in:
%%          {default_domain, Domain} and {default_locale, Locale}
%%      And thus be able to extract strings better from the functions that use
%%      the process dictionary.
parse_transform(Form, Opts) ->
    case proplists:get_value(gettext, Opts) of
        undefined -> Form;
        true ->
        %    {ok, F} = file:open("ast", [write]),
        %    io:fwrite(F, "~p", [Form]),
            EpotFile = proplists:get_value(epot, Opts, ?EPOT_FILE),
            file:delete(EpotFile),
            open_epot_file(EpotFile),
            try pt(Form, Opts) after close_epot_file() end,
            Form
    end.

%% @doc Recursively walk down the abstract syntax tree and find all
%%      function calls that we want to extract strings/binaries from.
pt(Form, Opts) ->
    put(fname, ""),
    pt(Form, Opts, undefined).

pt([H|T],Opts,Func) when is_list(H) ->
    F = fun (X) -> pt(X,Opts,Func) end,
    [lists:map(F,H)|pt(T,Opts,Func)];
pt({call, _, _, _} = Call, _Opts, _Func) ->
    pt_call(Call);
pt([{call,_,_,_} = Call | T], Opts, Func) ->
    [pt_call(Call) | pt(T, Opts, Func)];
%%% Retrieve module name from parametrized modules
pt([{attribute,L,module,{Mod,_Params}} | T], Opts, Func) ->
    pt([{attribute,L,module,Mod} | T], Opts, Func);
%%% Retrieve module name
pt([{attribute,_L,module,Mod} = H | T], Opts, Func) ->
    put(fname, to_list(Mod) ++ ".erl"),
    [H | pt(T, Opts, Func)];
pt([{attribute,_L,yawsfile,Fname} = H | T], Opts, Func) ->
    put(fname, to_list(Fname)),
    [H | pt(T, Opts, Func)];
pt([{block,N,B}|T], Opts, Func) ->
    Block = {block,N,pt(B,Opts,Func)},
    [Block|pt(T, Opts, Func)];
pt([H|T], Opts, Func) when is_tuple(H) ->
    [while(size(H), H, Opts, Func) | pt(T, Opts, Func)];
pt([H|T], Opts, Func) ->
    [H | pt(T, Opts, Func)];
pt(T, Opts, Func) when is_tuple(T) ->
    while(size(T), T, Opts, Func);
pt(X, _, _) ->
    X.

while(_,{block,N,B},Opts,Func) ->
    {block,N,pt(B,Opts,Func)};
while(N,T,Opts,Func) when N>0 ->
    NT = setelement(N,T,pt(element(N,T),Opts,Func)),
    while(N-1,NT,Opts,Func);
while(0,T,_,_) ->
    T.

-define(ATOM(Atom), {atom, _, Atom}).
-define(STR(Line, Str), {string, Line, Str}).
-define(BIN(Line, Elems), {bin, Line, Elems}).

%% @doc If we find a function call, check if it is any of the ones we can
%%      extract from and if so remove first arg (Domain) and call *_entry
%%      to retrive the entry that goes into the dump. 
%%      NOTE: Does not change anything the AST.
pt_call({call, _, {remote, _, ?ATOM(_), ?ATOM(Function)}, [?ATOM(Domain) | Args]} = Call) ->
    Res = case Function of
              dpgettext   -> dpgettext_entry(Args);
              dnpgettext  -> dnpgettext_entry(Args);
              _ -> no_dump
          end,
    case Res of
        no_dump -> ok;
        {Entry, Lines} -> dump(Domain, Entry, Lines)
    end,
    Call;
pt_call(Call) -> Call.

%% @doc Example:
%%      dpgettext(adomain, undefined, "AString", "alocale")
%%      dpgettext(adomain, undefined, <<"AString">>, <<"alocale">>)
dpgettext_entry([?ATOM(undefined), MsgID0, _Locale]) ->
    case MsgID0 of
        ?STR(L, MsgID) ->
            {{simple, MsgID}, [L]};
        ?BIN(L, MsgID1) -> 
            case concat_bin(MsgID1) of
                failed -> no_dump;
                MsgID -> {{simple, MsgID}, [L]}
            end
    end;
%% @doc Example:
%%      dpgettext(adomain, "AContext", "AString", "alocale")
dpgettext_entry([?STR(L1, MsgCtxt), ?STR(L2, MsgID), _Locale]) -> 
    {{simple, MsgCtxt, MsgID}, [L1, L2]};
%% @doc Example:
%%      dpgettext(adomain, <<"AContext">>, <<"AString">>, <<"alocale">>)
dpgettext_entry([?BIN(L1, MsgCtxt0), ?BIN(L2, MsgID0), _Locale]) ->
    case concat_bins([MsgCtxt0, MsgID0]) of
        failed -> no_dump;
        [MsgCtxt, MsgID] -> {{simple, MsgCtxt, MsgID}, [L1, L2]}
    end;
dpgettext_entry(_) -> no_dump.

%% @doc Example:
%%      dnpgettext(adomain, undefined, "Singular", "Plural", 2, "alocale")
dnpgettext_entry([?ATOM(undefined), ?STR(L1, Singular), ?STR(L2, Plural), _N, _Locale]) ->
    {{plural, Singular, Plural}, [L1, L2]};
%% @doc Example:
%%      dnpgettext(adomain, undefined, <<"Singular">>, <<"Plural">>, 2, <<"alocale">>)
dnpgettext_entry([?ATOM(undefined), ?BIN(L1, Singular0), ?BIN(L2, Plural0), _N, _Locale]) ->
    case concat_bins([Singular0, Plural0]) of
        failed -> no_dump;
        [Singular, Plural] -> {{plural, Singular, Plural}, [L1, L2]}
    end;
%% @doc Example:
%%      dnpgettext(adomain, "AContext", "Singular", "Plural", 2, "alocale")
dnpgettext_entry([?STR(L1, MsgCtxt), ?STR(L2, Singular), ?STR(L3, Plural), _N, _Locale]) ->
    {{plural, MsgCtxt, Singular, Plural}, [L1, L2, L3]};
%% @doc Example:
%%      dnpgettext(adomain, <<"AContext">>, <<"Singular">>, <<"Plural">>, 2, <<"alocale">>)
dnpgettext_entry([?BIN(L1, MsgCtxt0), ?BIN(L2, Singular0), ?BIN(L3, Plural0), _N, _Locale]) ->
    case concat_bins([MsgCtxt0, Singular0, Plural0]) of
        failed -> no_dump;
        [MsgCtxt, Singular, Plural] -> {{plural, MsgCtxt, Singular, Plural}, [L1, L2, L3]}
    end;
dnpgettext_entry(_) -> no_dump.

%% @doc Bins is a list of list of bin_elements from AST-binaries
%%      If they all contain only string literals we can concatenate
%%      these. So each element in Bins contain bin_elements from a specific
%%      binary. Returns failed all lists did not contain only string literals
concat_bins(Bins) ->
    concat_bins(Bins, []).

concat_bins([], Acc) ->
    lists:reverse(Acc);
concat_bins([Bin | Rest], Acc) ->
    case concat_bin(Bin) of
        failed -> failed;
        BinStr -> concat_bins(Rest, [BinStr|Acc])
    end.

%% @doc Concat all string literal binary elements if possible
%%      else return failed.
concat_bin(Bin) ->
    concat_bin(Bin, []).

concat_bin([], Acc) ->
    lists:concat(lists:reverse(Acc));
concat_bin([{bin_element, _, ?STR(_, Str), default, default} | Rest], Acc) ->
    concat_bin(Rest, [Str|Acc]);
concat_bin(_, _) ->
    failed.

%% @doc Dump Entry from Domain into the epot file with info of where the string
%%      where located
dump(Domain, Entry, Lines) ->
    Key = {Domain, Entry},
    Fname = get(fname),
    Finfo = get_file_info(Key),
    dets:insert(?EPOT_TABLE, {Key, [{Fname,Lines}|Finfo]}).

%% @doc Get the file info of a given Key
get_file_info(Key) ->
    case dets:lookup(?EPOT_TABLE, Key) of
	[]             -> [];
	[{_, Finfo}|_] -> Finfo
    end.

open_epot_file(EpotFile) ->
    filelib:ensure_dir(EpotFile),
    {ok, _} = dets:open_file(?EPOT_TABLE, [{file, EpotFile}]).

close_epot_file() ->
    dets:close(?EPOT_TABLE).

get_epot_data() ->
    dets:foldl(fun(E, Acc) -> [E|Acc] end, [], ?EPOT_TABLE).


to_list(A) when is_atom(A)    -> atom_to_list(A);
to_list(I) when is_integer(I) -> integer_to_list(I);
to_list(B) when is_binary(B)  -> binary_to_list(B);
to_list(L) when is_list(L)    -> L.
