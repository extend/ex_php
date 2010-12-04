%% Copyright (c) 2010, Dev:Extend
%% All rights reserved.
%%
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions are met:
%%
%%  * Redistributions of source code must retain the above copyright notice,
%%    this list of conditions and the following disclaimer.
%%  * Redistributions in binary form must reproduce the above copyright
%%    notice, this list of conditions and the following disclaimer in the
%%    documentation and/or other materials provided with the distribution.
%%  * Neither the name of Dev:Extend nor the names of its contributors may be
%%    used to endorse or promote products derived from this software without
%%    specific prior written permission.
%%
%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
%% AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
%% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
%% ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
%% LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
%% CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
%% SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
%% INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
%% CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
%% ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
%% POSSIBILITY OF SUCH DAMAGE.

%% @type value() = null | bool() | integer() | float() | array() | object().
%% @type array() = [{key(), value()}].
%% @type object() = {class(), object_data()}.
%% @type class() = label().
%% @type object_data() = iodata() | array().
%% @type key() = integer() | label().
%% @type label() = atom() | iodata().
%% @type iodata() = iolist() | binary().
%% @type iolist() = [char() | string() | binary()].

-module(ex_php).
-author('Anthony Ramine <nox@dev-extend.eu>').
-include_lib("eunit/include/eunit.hrl").

-export([serialize/1,
         serialize/2,
         unserialize/1,
         read_serialized/1]).

%% @spec serialize(value()) -> binary()
%% @equiv serialize(Value, 6)
%% @doc Serialize `Value' with the default float precision.
serialize(Value) ->
  serialize(Value, 6).

%% @spec serialize(Value::value(), Precision::integer()) -> binary()
%% @doc Serialize `Value' using `Precision' to write floats.
serialize(Integer, _Precision) when is_integer(Integer) ->
  write_integer(Integer);
serialize(Float, Precision) when is_float(Float) ->
  Format = lists:flatten([$~, $., integer_to_list(Precision), $f]),
  iolist_to_binary([$d, $:, io_lib:format(Format, [Float]), $;]);
serialize(Bin, _Precision) when is_binary(Bin) ->
  write_string(Bin);
serialize(List, Precision) when is_list(List) ->
  iolist_to_binary([<<"a:">>, write_pairs(List, Precision)]);
serialize({Class, Data}, Precision) ->
  write_object(Class, Data, Precision);
serialize(Atom, _Precision) when is_atom(Atom) ->
  write_atom(Atom).

%% @spec unserialize(iodata()) -> value()
%% @doc Unserialize `Data'.
unserialize(Data) ->
  {Term, <<>>} = read_serialized(Data),
  Term.

%% @spec read_serialized(iodata()) -> {value(), binary()}
%% @doc Same as unserialize/1, but include the remaining binary in the returned
%%      value.
read_serialized(<<"N;", Rest/binary>>) ->
  {null, Rest};
read_serialized(<<"b:0;", Rest/binary>>) ->
  {false, Rest};
read_serialized(<<"b:1;", Rest/binary>>) ->
  {true, Rest};
read_serialized(<<"d:", Rest/binary>>) ->
  {Double, <<";", Rest2/binary>>} = read_float(Rest),
  {Double, Rest2};
read_serialized(<<"a:", Rest/binary>>) ->
  {Fields, Rest2} = read_brackets(Rest, fun read_assocs/2),
  {{array, Fields}, Rest2};
read_serialized(<<"O:", Rest/binary>>) ->
  read_object(Rest, fun read_properties/2);
read_serialized(<<"C:", Rest/binary>>) ->
  read_object(Rest, fun read_raw/2);
read_serialized(Bin) when is_binary(Bin) ->
  read_key(Bin);
read_serialized(List) when is_list(List) ->
  read_serialized(iolist_to_binary(List)).


%% @spec write_object(class(), object_data(),
%%                    Precision::integer()) -> binary()
write_object(Class, Data, Precision) when is_list(Data) ->
  iolist_to_binary([$O, $:, write_label(Class), $:,
                    write_pairs(Data, Precision)]);
write_object(Class, Data, _Precision) when is_binary(Data) ->
  iolist_to_binary([$C, $:, write_label(Class), $:,
                    unsigned_to_binary(byte_size(Data)), $:,
                    ${, Data, $}]).

%% @spec write_pairs(Pairs::[pair()], Precision::integer()) -> iolist()
write_pairs(Pairs, Precision) ->
  write_pairs(Pairs, Precision, [], 0).
%% @hidden
write_pairs([], _Precision, Acc, Length) ->
  [integer_to_binary(Length), ":{", lists:reverse(Acc), $}];
write_pairs([{Key, Value} | Pairs], Precision, Acc, Length) ->
  PairBin = [write_key(Key), serialize(Value, Precision)],
  write_pairs(Pairs, Precision, [PairBin | Acc], Length + 1).

%% @spec write_key(Key::key()) -> binary()
write_key(Integer) when is_integer(Integer) ->
  write_integer(Integer);
write_key(Label) ->
  write_label(Label).

%% @spec write_integer(integer()) -> binary()
%% @doc Return `<<"i:Integer;">>'.
write_integer(Integer) ->
  <<"i:", (integer_to_binary(Integer))/binary, $;>>.

%% @spec write_atom(Atom::atom()) -> binary()
%% @doc Return `<<"N;">>' if `Atom' is `null', `<<"b:0;">>' if `Atom' is
%%      `false', `<<"b:1;">>' if `Atom' is `true'.
write_atom(null) ->
  <<"N;">>;
write_atom(false) ->
  <<"b:0;">>;
write_atom(true) ->
  <<"b:1;">>.

%% @spec write_string(Value::type()) -> binary()
%%       where type() = iodata() | atom()
%% @doc Return `<<"s:L(Value):\"Value\";">>'.
write_string(Value) ->
  <<"s:", (write_binary(Value))/binary, $;>>.

%% @hidden
write_label(List) when is_list(List) ->
  write_label(iolist_to_binary(List));
write_label(Atom) when is_atom(Atom) ->
  write_label(atom_to_binary(Atom, latin1));
write_label(Term) ->
  write_string(Term).

%% @spec write_binary(binary()) -> binary()
%% @doc Return `<<"L(Value):\"Value\"">>'.
write_binary(Bin) ->
  <<(unsigned_to_binary(byte_size(Bin)))/binary, $:, $", Bin/binary, $">>.

%% @spec integer_to_binary(integer()) -> binary()
integer_to_binary(Integer) when Integer < 0 ->
  <<$-, (integer_to_binary(Integer))/binary>>;
integer_to_binary(Integer) ->
  unsigned_to_binary(Integer).

%% @spec unsigned_to_binary(Integer::integer()) -> binary()
unsigned_to_binary(0) ->
  <<$0>>;
unsigned_to_binary(Integer) when Integer > 0 ->
  unsigned_to_binary(Integer, <<>>).
unsigned_to_binary(0, Acc) ->
  Acc;
unsigned_to_binary(Integer, Acc) ->
  unsigned_to_binary(Integer div 10, <<((Integer rem 10) + $0), Acc/binary>>).


%% @spec read_object(binary(), function()) -> {object(), binary()}
read_object(Bin, ReadFun) ->
  {Class, <<$:, Rest/binary>>} = read_string(Bin),
  {ObjectData, Rest2} = read_brackets(Rest, ReadFun),
  {{Class, ObjectData}, Rest2}.

%% @spec read_brackets(binary(), function()) -> {term(), binary()}
read_brackets(Bin, ReadFun) ->
  {Length, <<":{", Rest/binary>>} = read_unsigned(Bin),
  {Result, <<$}, Rest2/binary>>} = ReadFun(Rest, Length),
  {Result, Rest2}.

%% @spec read_properties(binary(), integer()) -> {[property()], binary()}
read_properties(Bin, Length) ->
  read_pairs(Bin, Length, fun read_string/1).

%% @spec read_raw(binary(), integer()) -> {raw(), binary()}
read_raw(Bin, Length) ->
  <<Data:Length/binary, Rest/binary>> = Bin,
  {{raw, Data}, Rest}.

%% @spec read_assocs(binary(), integer()) -> {[assoc()], binary()}
read_assocs(Bin, Length) ->
  read_pairs(Bin, Length, fun read_key/1).

%% @spec read_pairs(binary(), integer(), function()) -> {[field()], binary()}
read_pairs(Bin, Length, ReadKeyFun) ->
  read_pairs(Bin, Length, ReadKeyFun, []).
read_pairs(Bin, 0, _ReadKeyFun, Fields) ->
  {lists:reverse(Fields), Bin};
read_pairs(Bin, N, ReadKeyFun, Fields) ->
  {Key, <<$:, Rest/binary>>} = ReadKeyFun(Bin),
  {Value, Rest2} = read_serialized(Rest),
  read_pairs(Rest2, N - 1, ReadKeyFun, [{Key, Value}, Fields]).

%% @spec read_key(Bin::binary()) -> {integer() | binary(), binary()}
read_key(<<"i:", Rest/binary>>) ->
  {Integer, <<";", Rest2/binary>>} = read_integer(Rest),
  {Integer, Rest2};
read_key(Bin) ->
  read_string(Bin).

%% @spec read_string(Bin::binary()) -> {binary(), binary()}
read_string(<<"s:", Rest/binary>>) ->
  {String, <<$;, Rest2/binary>>} = read_binary(Rest, fun read_string/2),
  {String, Rest2}.
%% @hidden
read_string(Bin, Length) ->
  <<String:Length/binary, Rest/binary>> = Bin,
  {String, Rest}.

%% @spec read_binary(binary(), function()) -> {binary(), binary()}
read_binary(Bin, ReadFun) ->
  {Length, <<$:, $", Rest/binary>>} = read_unsigned(Bin),
  {String, <<$", Rest2/binary>>} = ReadFun(Rest, Length),
  {String, Rest2}.

%% @spec read_float(binary()) -> {float(), binary()}
read_float(Bin) ->
  {Int, <<$., Rest/binary>>} = read_integer(Bin),
  {Digits, <<$;, Rest2/binary>>} = read_digits(Rest),
  {Int + list_to_float("0." ++ Digits), Rest2}.

%% @spec read_integer(binary()) -> {integer(), binary()}
read_integer(<<$-, Rest/binary>>) ->
  -read_unsigned(Rest);
read_integer(Bin) ->
  read_unsigned(Bin).

%% @spec read_unsigned(binary()) -> {integer(), binary()}
read_unsigned(Bin) ->
  read_unsigned(Bin, 0, start).
%% @hidden
read_unsigned(<<C, Rest/binary>>, Acc, _State) when C >= $0, C =< $9 ->
  read_unsigned(Rest, Acc * 10 + C - $0, reading);
read_unsigned(Bin, Acc, reading) ->
  {Acc, Bin}.

%% @spec read_digits(binary()) -> {string(), binary()}
read_digits(Bin) ->
  read_digits(Bin, []).
%% @hidden
read_digits(<<C, Rest/binary>>, Acc) when C >= $0, C =< $9 ->
  read_digits(Rest, [C | Acc]);
read_digits(Bin, Acc) ->
  {lists:reverse(Acc), Bin}.


%% @hidden
basic_test_() ->
  Test = fun (Value) -> fun () -> unserialize(serialize(Value)) end end,
  [ Test(Value) || Value <- [null,
                             false,
                             true,
                             42,
                             <<"foobar">>,
                             [],
                             {<<"stdClass">>, []},
                             {<<"Foo">>, <<"bar">>}] ].
