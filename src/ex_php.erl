%% Copyright (c) 2011, Anthony Ramine <nox@dev-extend.eu>
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

-module(ex_php).
-author('Anthony Ramine <nox@dev-extend.eu>').
-include_lib("eunit/include/eunit.hrl").

-type input_value() :: value(atom() | binary()).
-type output_value() :: value(binary()).
-type value(LabelT) :: null
                     | boolean()
                     | integer()
                     | float()
                     | binary()
                     | php_array(LabelT)
                     | object(LabelT).

-type php_array(LabelT) :: [pair(LabelT)].

-type input_object() :: object(atom() | binary()).
-type output_object() :: object(binary()).
-type object(LabelT) :: {LabelT, object_data(LabelT)}.

-type output_object_data() :: object_data(binary()).
-type object_data(LabelT) :: php_array(LabelT) | binary().

-type input_pair() :: pair(atom() | binary()).
-type output_pair() :: pair(binary()).
-type pair(LabelT) :: {key(LabelT), value(LabelT)}.

-type input_key() :: key(atom() | binary()).
-type output_key() :: key(binary()).
-type key(LabelT) :: integer() | LabelT.

-type read_brackets_fun(T) :: fun((binary(), non_neg_integer()) ->
                                    {T, binary()}).

-export_type([input_value/0, output_value/0]).

-export([serialize/1,
         serialize/2,
         unserialize/1,
         read_serialized/1]).

-spec serialize(input_value()) -> iodata().
%% @equiv serialize(Value, 6)
serialize(Value) ->
  serialize(Value, 6).

-spec serialize(input_value(), non_neg_integer()) -> iodata().
serialize(Integer, _Precision) when is_integer(Integer) ->
  write_integer(Integer);
serialize(Float, Precision) when is_float(Float) ->
  Format = lists:flatten([$~, $., integer_to_list(Precision), $f]),
  [$d, $:, io_lib:format(Format, [Float]), $;];
serialize(Bin, _Precision) when is_binary(Bin) ->
  write_string(Bin);
serialize(List, Precision) when is_list(List) ->
  [<<"a:">> | write_pairs(List, Precision)];
serialize(Object = {_Class, _Data}, Precision) ->
  write_object(Object, Precision);
serialize(Atom, _Precision) ->
  write_atom(Atom).

-spec unserialize(iodata()) -> output_value().
unserialize(Data) ->
  {Term, <<>>} = read_serialized(Data),
  Term.

-spec read_serialized(iodata()) -> {output_value(), binary()}.
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
  read_brackets(Rest, fun read_pairs/2);
read_serialized(<<"O:", Rest/binary>>) ->
  read_object(Rest, fun read_pairs/2);
read_serialized(<<"C:", Rest/binary>>) ->
  read_object(Rest, fun read_raw/2);
read_serialized(Bin) when is_binary(Bin) ->
  read_key(Bin);
read_serialized(List) ->
  read_serialized(iolist_to_binary(List)).


-spec write_object(input_object(), non_neg_integer()) -> iolist().
write_object({Class, Data}, Precision) when is_list(Data) ->
  [<<"O:">>, write_label(Class), $: | write_pairs(Data, Precision)];
write_object({Class, Data}, _Precision) ->
  [<<"C:">>, write_label(Class), $:, unsigned_to_binary(byte_size(Data)), $:,
   ${, Data, $}].

-spec write_pairs([input_pair()], non_neg_integer()) -> iolist().
write_pairs(Pairs, Precision) ->
  write_pairs(Pairs, Precision, [], 0).

-spec write_pairs([input_pair()], Precision::non_neg_integer(),
                  iolist(), Length::non_neg_integer()) -> iolist().
write_pairs([], _Precision, Acc, Length) ->
  [integer_to_binary(Length), <<":{">>, lists:reverse(Acc), $}];
write_pairs([{Key, Value} | Pairs], Precision, Acc, Length) ->
  PairBin = [write_key(Key), serialize(Value, Precision)],
  write_pairs(Pairs, Precision, [PairBin | Acc], Length + 1).

-spec write_key(input_key()) -> iolist().
write_key(Integer) when is_integer(Integer) ->
  write_integer(Integer);
write_key(Label) ->
  write_label(Label).

-spec write_integer(integer()) -> iolist().
write_integer(Integer) ->
  [<<"i:">>, integer_to_binary(Integer), $;].

-spec write_atom(null | true | false) -> binary().
write_atom(null) ->
  <<"N;">>;
write_atom(false) ->
  <<"b:0;">>;
write_atom(true) ->
  <<"b:1;">>.

-spec write_label(atom() | binary()) -> iolist().
write_label(Atom) when is_atom(Atom) ->
  write_string(atom_to_binary(Atom, latin1));
write_label(Term) ->
  write_string(Term).

-spec write_string(binary()) -> iolist().
write_string(Bin) ->
  [<<"s:">>, unsigned_to_binary(byte_size(Bin)), <<":\"">>, Bin, <<"\";">>].

-spec integer_to_binary(integer()) -> binary().
integer_to_binary(Integer) when Integer < 0 ->
  [$-, unsigned_to_binary(-Integer)];
integer_to_binary(Integer) ->
  unsigned_to_binary(Integer).

-spec unsigned_to_binary(non_neg_integer()) -> binary().
unsigned_to_binary(0) ->
  <<$0>>;
unsigned_to_binary(Integer) when Integer > 0 ->
  unsigned_to_binary(Integer, <<>>).

-spec unsigned_to_binary(non_neg_integer(), binary()) -> binary().
unsigned_to_binary(0, Acc) ->
  Acc;
unsigned_to_binary(Integer, Acc) ->
  unsigned_to_binary(Integer div 10, <<((Integer rem 10) + $0), Acc/binary>>).


-spec read_object(binary(), read_brackets_fun(output_object_data())) ->
                   {output_object(), binary()}.
read_object(Bin, ReadFun) ->
  {Class, <<$:, Rest/binary>>} = read_string(Bin),
  {ObjectData, Rest2} = read_brackets(Rest, ReadFun),
  {{Class, ObjectData}, Rest2}.

-spec read_brackets(binary(), read_brackets_fun(T)) -> {T, binary()}.
read_brackets(Bin, ReadFun) ->
  {Length, <<":{", Rest/binary>>} = read_unsigned(Bin),
  {Result, <<$}, Rest2/binary>>} = ReadFun(Rest, Length),
  {Result, Rest2}.

-spec read_raw(binary(), non_neg_integer()) -> {binary(), binary()}.
read_raw(Bin, Length) ->
  <<Data:Length/binary, Rest/binary>> = Bin,
  {Data, Rest}.

-spec read_pairs(binary(), non_neg_integer()) -> {[output_pair()], binary()}.
read_pairs(Bin, Length) ->
  read_pairs(Bin, Length, []).

-spec read_pairs(binary(), non_neg_integer(), [output_pair()]) ->
                  {[output_pair()], binary()}.
read_pairs(Bin, 0, Acc) ->
  {lists:reverse(Acc), Bin};
read_pairs(Bin, N, Acc) ->
  {Key, <<$:, Rest/binary>>} = read_key(Bin),
  {Value, Rest2} = read_serialized(Rest),
  read_pairs(Rest2, N - 1, [{Key, Value} | Acc]).

-spec read_key(binary()) -> {output_key(), binary()}.
read_key(<<"i:", Rest/binary>>) ->
  {Integer, <<";", Rest2/binary>>} = read_integer(Rest),
  {Integer, Rest2};
read_key(Bin) ->
  read_string(Bin).

-spec read_string(binary()) -> {binary(), binary()}.
read_string(<<"s:", Rest/binary>>) ->
  {Length, <<$:, $", Rest2/binary>>} = read_unsigned(Rest),
  <<String:Length/binary, $", $;, Rest3/binary>> = Rest2,
  {String, Rest3}.

-spec read_float(binary()) -> {float(), binary()}.
read_float(Bin) ->
  {Int, <<$., Rest/binary>>} = read_integer(Bin),
  {Digits, <<$;, Rest2/binary>>} = read_digits(Rest),
  {Int + list_to_float("0." ++ Digits), Rest2}.

-spec read_integer(binary()) -> {integer(), binary()}.
read_integer(<<$-, Rest/binary>>) ->
  {Result, Rest} = read_unsigned(Rest),
  {-Result, Rest};
read_integer(Bin) ->
  read_unsigned(Bin).

-spec read_unsigned(binary()) -> {non_neg_integer(), binary()}.
read_unsigned(<<C, Rest/binary>>) when C >= $0, C =< $9 ->
  read_unsigned(Rest, C - $0).

-spec read_unsigned(binary(), non_neg_integer()) ->
                     {non_neg_integer(), binary()}.
read_unsigned(<<C, Rest/binary>>, Acc) when C >= $0, C =< $9 ->
  read_unsigned(Rest, Acc * 10 + C - $0);
read_unsigned(Bin, Acc) ->
  {Acc, Bin}.

-spec read_digits(binary()) -> {string(), binary()}.
read_digits(Bin) ->
  read_digits(Bin, []).

-spec read_digits(binary(), string()) -> {string(), binary()}.
read_digits(<<C, Rest/binary>>, Acc) when C >= $0, C =< $9 ->
  read_digits(Rest, [C | Acc]);
read_digits(Bin, Acc) ->
  {lists:reverse(Acc), Bin}.


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
