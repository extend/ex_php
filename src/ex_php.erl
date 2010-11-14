%% @type value() = null | bool() | integer() | float() | array()
%%               | object() | data().
%% @type array() = {array, [assoc()]}.
%% @type assoc() = pair(key()) | value().
%% @type key() = null | bool() | integer() | data().
%% @type object() = {object, class(), object_data()}.
%% @type class() = label().
%% @type label() = iodata().
%% @type object_data() = data() | [property()].
%% @type property() = pair(data()).
%% @type data() = iodata() | atom().
%% @type pair(T) = {T, value()}.

-module(ex_php).
-include_lib("eunit/include/eunit.hrl").

-export([serialize/1,
         serialize/2,
         unserialize/1,
         read_serialized/1]).

-define(is_digit(C), C >= $0, C =< $9).
-define(is_letter(C), C =:= $_;
                      C >= $A, C =< $Z;
                      C >= $a, C =< $z;
                      C >= 127).

%% @spec serialize(value()) -> binary()
%% @equiv serialize(Value, 6)
%% @doc Serialize `Value' with the default float precision.
serialize(Value) ->
  serialize(Value, 6).

%% @spec serialize(Value::value(), Precision::integer()) -> binary()
%% @doc Serialize `Value' using `Precision' to write floats.
%% @todo Document array indexing.
serialize(Integer, _Precision) when is_integer(Integer) ->
  write_integer(Integer);
serialize(Float, Precision) when is_float(Float) ->
  Format = lists:flatten([$~, $., integer_to_list(Precision), $f]),
  iolist_to_binary([$d, $:, io_lib:format(Format, [Float]), $;]);
serialize({array, Assocs}, Precision) when is_list(Assocs) ->
  AssocsIo =  write_pairs(Assocs, Precision, write_assoc_fun(0)),
  iolist_to_binary([<<"a:">>, AssocsIo]);
serialize({object, Class, Data}, Precision) ->
  write_object(Class, Data, Precision);
serialize(Atom, _Precision) when is_atom(Atom) ->
  write_atom(Atom);
serialize(Term, _Precision) ->
  write_string(Term).

%% @spec unserialize(iodata()) -> value()
%% @doc Unserialize `Data'.
%%      Array values are always qualified by their indices, object properties
%%      are always returned as binaries.
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
  read_object(Rest, fun read_data/2);
read_serialized(Bin) when is_binary(Bin) ->
  read_key(Bin);
read_serialized(List) when is_list(List) ->
  read_serialized(iolist_to_binary(List)).


%% @spec write_object(class(), object_data(),
%%                    Precision::integer()) -> binary()
write_object(Class, Data, Precision) when is_list(Data) ->
  iolist_to_binary([$O, $:, write_label(Class), $:,
                    write_pairs(Data, Precision, write_property_fun())]);
write_object(Class, Data, _Precision) ->
  DataBin = write_data(Data),
  iolist_to_binary([$C, $:, write_label(Class), $:,
                    unsigned_to_binary(byte_size(DataBin)), $:,
                    ${, DataBin, $}]).

%% @spec write_pairs(Pairs::[pair()], Precision::integer(),
%%                   function()) -> iolist()
write_pairs(Pairs, Precision, WriteFun) ->
  write_pairs(Pairs, Precision, WriteFun, [], 0).
write_pairs([], _Precision, _WriteFun, Acc, Length) ->
  [integer_to_binary(Length), ":{", lists:reverse(Acc), $}];
write_pairs([Field | Fields], Precision, WriteFun, Acc, Length) ->
  {FieldBin, NewWriteFun} = WriteFun(Field, Precision),
  write_pairs(Fields, Precision, NewWriteFun, [FieldBin | Acc], Length + 1).

%% @spec write_assoc_fun(integer()) -> function()
write_assoc_fun(Index) ->
  fun (Assoc, Precision) ->
        {AssocBin, NewIndex} = write_assoc(Assoc, Precision, Index),
        {AssocBin, write_assoc_fun(NewIndex)} end.

%% @spec write_assoc(Assoc::assoc(), Precision::integer(),
%%                   integer()) -> {iolist(), integer()}
write_assoc({Key, Value}, Precision, Index) ->
  {KeyBin, NewIndex} = write_key(Key, Index),
  {[KeyBin, serialize(Value, Precision)], NewIndex + 1};
write_assoc(Value, Precision, Index) ->
  write_assoc({Index, Value}, Precision, Index).

%% @spec write_key(Key::key(), Index::integer()) -> {binary(), integer()}
write_key(Integer, Index) when is_integer(Integer) ->
  {write_integer(Integer), max(Integer, Index + 1)};
write_key(Atom, Index) when Atom =:= null; Atom =:= false ->
  write_key(0, Index);
write_key(true, Index) ->
  write_key(1, Index);
write_key(Data, Index) ->
  {write_string(Data), Index}.

%% @spec write_property_fun() -> function()
write_property_fun() ->
  fun ({Name, Value}, Precision) ->
        {[write_string(Name), serialize(Value, Precision)],
         write_property_fun()} end.

%% @spec write_integer(integer()) -> binary()
%% @doc Return `<<"i:Integer;">>'.
write_integer(Integer) ->
  <<"i:", (integer_to_binary(Integer))/binary, $;>>.

%% @spec write_atom(Atom::atom()) -> binary()
%% @doc Return `<<"N;">>' if `Atom' is `null', `<<"b:0;">>' if `Atom' is
%%      `false', `<<"b:1;">>' if `Atom' is `true'; otherwise call
%%      write_string/1 with the binary representation of `Atom'.
write_atom(null) ->
  <<"N;">>;
write_atom(false) ->
  <<"b:0;">>;
write_atom(true) ->
  <<"b:1;">>;
write_atom(Atom) ->
  write_string(atom_to_binary(Atom, latin1)).

%% @spec write_string(Value::type()) -> binary()
%%       where type() = iodata() | atom()
%% @doc Return `<<"s:L(Value):\"Value\";">>'.
write_string(Value) ->
  <<"s:", (write_binary(Value))/binary, $;>>.

write_label(<<C, Rest/binary>>) when ?is_letter(C) ->
  write_label(Rest, [C]);
write_label(List) when is_list(List) ->
  write_label(iolist_to_binary(List));
write_label(Atom) when is_atom(Atom) ->
  write_label(atom_to_binary(Atom, latin1)).
write_label(<<C, Rest/binary>>, Acc) when ?is_letter(C); ?is_digit(C) ->
  write_label(Rest, Acc);
write_label(<<>>, Acc) ->
  write_binary(list_to_binary(lists:reverse(Acc))).

%% @spec write_binary(Value::data()) -> binary()
%% @doc Return `<<"L(Value):\"Value\"">>'.
write_binary(Value) ->
  Bin = write_data(Value),
  <<(unsigned_to_binary(byte_size(Bin)))/binary, $:, $", Bin/binary, $">>.

%% @spec write_data(Value::data()) -> binary()
%% @doc Return `Value' as a binary.
write_data(Bin) when is_binary(Bin) ->
  Bin;
write_data(List) when is_list(List) ->
  write_data(iolist_to_binary(List));
write_data(Atom) when is_atom(Atom) ->
  write_data(atom_to_binary(Atom, latin1)).

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
  {Class, <<$:, Rest/binary>>} = read_label(Bin),
  {ObjectData, Rest2} = read_brackets(Rest, ReadFun),
  {{object, Class, ObjectData}, Rest2}.

%% @spec read_brackets(binary(), function()) -> {term(), binary()}
read_brackets(Bin, ReadFun) ->
  {Length, <<":{", Rest/binary>>} = read_unsigned(Bin),
  {Result, <<$}, Rest2/binary>>} = ReadFun(Rest, Length),
  {Result, Rest2}.

%% @spec read_properties(binary(), integer()) -> {[property()], binary()}
read_properties(Bin, Length) ->
  read_pairs(Bin, Length, fun read_string/1).

%% @spec read_data(binary(), integer()) -> {binary(), binary()}
read_data(Bin, Length) ->
  <<Data:Length/binary, Rest/binary>> = Bin,
  {Data, Rest}.

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
read_string(Bin, Length) ->
  <<String:Length/binary, Rest/binary>> = Bin,
  {String, Rest}.

%% @spec read_label(binary()) -> {label(), binary()}
read_label(Bin) ->
  read_binary(Bin, fun read_label/2).
read_label(<<C, Rest/binary>>, Length) when ?is_letter(C) ->
  read_label(Rest, Length - 1, [C]).
read_label(<<C, Rest/binary>>, Length, Acc) when ?is_letter(C); ?is_digit(C) ->
  read_label(Rest, Length - 1, Acc);
read_label(Bin, 0, Acc) ->
  {list_to_binary(Acc), Bin}.

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
read_unsigned(<<C, Rest/binary>>, Acc, _State) when C >= $0, C =< $9 ->
  read_unsigned(Rest, Acc * 10 + C - $0, reading);
read_unsigned(Bin, Acc, reading) ->
  {Acc, Bin}.

%% @spec read_digits(binary()) -> {string(), binary()}
read_digits(Bin) ->
  read_digits(Bin, []).
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
                             {array, []},
                             {object, <<"stdClass">>, []},
                             {object, <<"Foo">>, <<"bar">>}] ].
