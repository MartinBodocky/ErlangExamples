%% File : sf.erl
%% Description: Share support functionality
-module(sf).
-export([guid_string/0]).

%% Utility functions

-spec guid_string() -> [any()].
guid_string() ->
	to_string(v4()).

-spec v4() -> <<_:128>>.
v4() ->
  v4(crypto:rand_uniform(1, round(math:pow(2, 48))) - 1, 
  	crypto:rand_uniform(1, round(math:pow(2, 12))) - 1, 
  	crypto:rand_uniform(1, round(math:pow(2, 32))) - 1, 
  	crypto:rand_uniform(1, round(math:pow(2, 30))) - 1).

-spec v4(integer(),integer(),integer(),integer()) -> <<_:128>>.
v4(R1, R2, R3, R4) ->
    <<R1:48, 4:4, R2:12, 2:2, R3:32, R4: 30>>.

-spec to_string(<<_:128>>) -> [any()].
% Returns a string representation of a binary UUID.
to_string(U) ->
    lists:flatten(io_lib:format("~8.16.0b-~4.16.0b-~4.16.0b-~2.16.0b~2.16.0b-~12.16.0b", get_parts(U))).

-spec get_parts(<<_:128>>) -> [non_neg_integer(),...].
% Returns the 32, 16, 16, 8, 8, 48 parts of a binary UUID.
get_parts(<<TL:32, TM:16, THV:16, CSR:8, CSL:8, N:48>>) ->
    [TL, TM, THV, CSR, CSL, N].