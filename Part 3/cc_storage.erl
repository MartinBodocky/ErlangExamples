%% File : cc_storage.erl
%% Description : Credit card provider persistance storage
-module(cc_storage).

-include("factory.hrl").
-export([open_storage/0, terminate_storage/0, delete_storage/0]).

-export([get_creditcard/1, add_creditcard/4, delete_creditcard/1, add_funds/2, 
	add_billingAddress/2, view_account/1, valid_creditcard/3, transaction/2]).

%% START - Maintenance and Operation API

-spec open_storage() -> any().
open_storage() ->
	StorageName = get_node_name(),
	ets:new(ccRam, [named_table, {keypos, #card.cardNumber}]),
	ets:new(ccIndex, [named_table]),
	dets:open_file(ccDisk, [{file, StorageName}, {keypos, #card.cardNumber}]),
	restore_storage(). 

-spec terminate_storage() -> 'ok' | {'error',_}.
terminate_storage() ->
	ets:delete(ccRam),
	ets:delete(ccIndex),
	dets:close(ccDisk).

-spec restore_storage() -> any().
restore_storage() ->
	Insert = fun(#card{ userName = UserName, cardNumber = CardNumber} = Card) ->
				ets:insert(ccRam, Card),
				ets:insert(ccIndex, {UserName, CardNumber}),
				continue
			end,
	dets:traverse(ccDisk, Insert).

-spec delete_storage() -> any().
delete_storage() ->
	StorageName = get_node_name(),
	ets:delete(ccRam),
	ets:delete(ccIndex),
	dets:close(ccDisk),
	file:delete(StorageName).

-spec update_creditcard(#card{}) -> ok | {error, _}.
update_creditcard(Card) ->
	ets:insert(ccRam, Card),
	dets:insert(ccDisk, Card).

%% END - Maintenance and Operation API

%% START - Client API
-spec get_creditcard(string()) -> any().
get_creditcard(UserName) ->
	case ets:lookup(ccIndex, UserName) of
		[{UserName, CardNumber}] -> CardNumber;
		_ -> {error, card_missing}
	end.

-spec add_creditcard(string(),integer(),number(),tuple()) -> any().
add_creditcard(UserName, CardNumber, Bilance, Expiration) ->
	case ets:lookup(ccIndex, UserName) of
		[{UserName, CardNumber}] -> CardNumber;
		_ ->
			%% create card record
			ets:insert(ccIndex, {UserName, CardNumber}),
			update_creditcard(#card{ userName = UserName, cardNumber = CardNumber, bilance = Bilance, expiration = Expiration}),
			CardNumber
	end.

-spec delete_creditcard(string()) -> any().
delete_creditcard(UserName) ->
	case ets:lookup(ccIndex, UserName) of
		[{UserName, CardNumber}] -> 
			ets:delete(ccIndex, UserName),
			ets:delete(ccIndex, CardNumber),
			dets:delete(ccDisk, CardNumber);
		_ -> {error, card_missing}
	end.

-spec add_funds(integer(),number()) -> ok | {error, card_missing}.
add_funds(CardNumber, Amount) ->
	case ets:lookup(ccRam, CardNumber) of
		[Card] ->
			NewBilance = Card#card.bilance + Amount,
			case NewBilance > 0 of
				true -> update_creditcard(Card#card{ bilance = NewBilance});
				false -> update_creditcard(Card#card{ bilance = 0})
			end;
		_ -> {error, card_missing}
	end.

-spec add_billingAddress(integer(),list()) -> ok | {error, card_missing}.
add_billingAddress(CardNumber, Address) ->
	case ets:lookup(ccRam ,CardNumber) of
		[Card] -> update_creditcard(Card#card{ address = Address});
		_ -> {error, card_missing}
	end.

-spec view_account(integer()) -> tuple().
view_account(CardNumber) ->
	case ets:lookup(ccRam, CardNumber) of
		[Card] -> Card;
		_ -> {error, card_missing}
	end.

-spec valid_creditcard(list(),integer(),{integer(),integer()}) -> boolean().
valid_creditcard(BillingAddress, CardNumber, {ExpMonth, ExpYear}) ->
	%% Check condition expiration time and right format of address
	case ets:lookup(ccRam, CardNumber) of
		[Card] ->
			if
				(BillingAddress == Card#card.address) and (Card#card.expiration == {ExpMonth, ExpYear}) -> true;
				true -> false
			end;
		_ -> false
	end.

-spec transaction(integer(),number()) -> {error, card_missing | insufficient_funds} | {ok,string()}.
transaction(CardNumber, Price) ->
	case ets:lookup(ccRam, CardNumber) of
		[Card] ->
			OldTransactions = Card#card.transactions,
			case Card#card.bilance >= Price of
				true ->
					TrxId = sf:guid_string(), 
					update_creditcard(Card#card{bilance = Card#card.bilance - Price, transactions = [{TrxId, Price} | OldTransactions ]}),
					{ok, TrxId};
				false -> {error, insufficient_funds}
			end;
		_-> {error, card_missing}
	end.

%% END - Client API

%% START - Support functions

get_node_name() ->
	case erlang:is_alive() of
		true -> atom_to_list(node()) ++ "_CC";
		false -> "CCFile"
	end.

%% END - Support functions