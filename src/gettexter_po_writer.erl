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
%% @doc Write a po file

-module(gettexter_po_writer).
-export([write/2]).

-define(ENDCOL, 72).
-define(PIVOT, 4).
-define(SEP, $\s).

%% @doc Write one .pot file for each domain in entries
write(Entries, OutDir) ->
    Domains = group_domains(Entries),
    [write_pot_file(OutDir, Domain, Es) || {Domain, Es} <- Domains],
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
    [{Domain, [{Ent, Refs} || {{_, Ent}, Refs} <- Same]} | group_domains(NotSame)].

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

write_entry(Fd, {simple, undefined, MsgID}) ->
    file:write(Fd, msgid(MsgID)),
    file:write(Fd, msgstr());
write_entry(Fd, {simple, MsgCtxt, MsgID}) ->
    file:write(Fd, msgctxt(MsgCtxt)),
    write_entry(Fd, {simple, undefined, MsgID});
write_entry(Fd, {plural, undefined, MsgID, MsgIDPlural}) ->
    file:write(Fd, msgid(MsgID)),
    file:write(Fd, msgid_plural(MsgIDPlural)),
    file:write(Fd, msgstr_plural(lists:seq(0,1)));
write_entry(Fd, {plural, MsgCtxt, MsgID, MsgIDPlural}) ->
    file:write(Fd, msgctxt(MsgCtxt)),
    write_entry(Fd, {plural, undefined, MsgID, MsgIDPlural}).

write_refs(_Fd, []) -> ok;
write_refs(Fd, [{File, Line}|Refs]) ->
    file:write(Fd, "#: " ++ File ++ ":" ++ integer_to_list(Line) ++ "\n"),
    write_refs(Fd, Refs).

write_entry(Fd, Entry, Refs) ->
    write_refs(Fd, Refs),
    write_entry(Fd, Entry).

write_entries(Fd, Entries) ->
    WriteEntry = fun({Entry, Refs}) ->
                         write_entry(Fd, Entry, Refs),
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
    "#, fuzzy\n"
    "msgid \"\"\n"
    "msgstr \"\"\n"
    "\"Project-Id-Version: PACKAGE VERSION\\n\"\n"
    "\"POT-Creation-Date: 2003-10-21 16:45+0200\\n\"\n"
    "\"PO-Revision-Date: YEAR-MO-DA HO:MI+ZONE\\n\"\n"
    "\"Last-Translator: FULL NAME <EMAIL@ADDRESS>\\n\"\n"
    "\"Language-Team: LANGUAGE <LL@li.org>\\n\"\n"
    "\"Language: en\\n\""
    "\"MIME-Version: 1.0\\n\"\n"
    "\"Content-Type: text/plain; charset=UTF-8\\n\"\n"
    "\"Content-Transfer-Encoding: 8bit\\n\"\n"
    "\"Plural-Forms: nplurals=2; plural=(n != 1);\\n\"\n\n".
