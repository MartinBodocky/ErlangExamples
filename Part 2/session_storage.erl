%% File : session_storage.erl
%% Descriptio : Module handles actions in session storage for keeping information about shopping cart for particular user/session
-module(session_storage).
-include("factory.hrl").

-export([open_storage/1, terminate_storage/1, delete_storage/1]).
-export([add_session/2, get_session/1, add_item/2, add_billingAddress/2, add_creditcard/3]).

%% START - Operation and Maintenance API

-spec open_storage(string()) -> ok.
open_storage(RefId) ->
	%% Data storage for keep list of current sessions
	RamTable = ets:new(sessionRam, [{keypos, #session.referenceId}]),
	dets:open_file(RefId, [{file, RefId}, {keypos, #session.referenceId}]),
	restore_storage(RamTable, RefId),
	RamTable.

-spec terminate_storage({pid(), string()}) -> ok.
terminate_storage({RamTable, RefId}) ->
	ets:delete(RamTable),
	dets:close(RefId).

-spec delete_storage({pid(), string()}) -> ok.
delete_storage({RamTable, RefId}) ->
	ets:delete(RamTable),
	dets:close(RefId),
	file:delete(RefId).

-spec restore_storage(pid(), string()) -> any().
restore_storage(RamTable, RefId) ->
	InsertSession = fun(#session{} = Session) ->
				ets:insert(RamTable, Session),
				continue
			end,
	dets:traverse(RefId, InsertSession).

-spec update_session(#session{}, string(), pid()) -> ok.
update_session(Session, RefId, RamTable) ->
	ets:insert(RamTable, Session),
	dets:insert(RefId, Session).

%% END - Operation and Maintenance API

%% START - Client API
-spec add_session(string(), {pid(), string()}) -> ok.
add_session(UserName, {RamTable, RefId}) ->
	case ets:lookup(RamTable, RefId) of
		[_Session] -> ok;
		_ -> update_session(#session{referenceId = RefId, userName = UserName}, RefId, RamTable)
	end.

-spec get_session({pid(), string()}) -> {'error','missing_user'} | #session{}.	
get_session({RamTable, RefId}) ->
	[Session] = ets:lookup(RamTable, RefId),
	Session.
	
-spec add_item({atom(),number()}, {pid(), string()}) -> {'error' | [{'added',_} | {'removed',_} | {'total',_},...],'missing_user' | {_,_}}.
add_item({Item, Count}, {RamTable, RefId}) ->
	[Session] =  ets:lookup(RamTable, RefId),
	OldCount = get_count(Item, Session#session.cart),
	NewCart = add_list({Item, Count}, Session#session.cart),
	NewCount = get_count(Item, NewCart),
	update_session(Session#session{cart = NewCart, price = total_price(NewCart)}, RefId, RamTable),
	if Count >= 0 ->
		[{added, Count}, {total, NewCount}];
			true ->
			ActualChange = abs(OldCount - NewCount),
		[{removed, ActualChange}, {total, NewCount}]
	end.

-spec add_billingAddress([any()], {pid(), string()}) -> 'ok' | {'error',_}.
add_billingAddress(Address, {RamTable, RefId}) ->
	if (is_list(Address) and (length(Address) == 4)) ->
		[Session] = ets:lookup(RamTable, RefId),
		update_session(Session#session{ billingAddress = Address}, RefId, RamTable);
		true -> {error, invalid_address}
	end.

-spec add_creditcard(number(),{number(),number()}, {pid(), string()}) -> 'ok' | {'error',_}.
add_creditcard(CardNumber, {ExpMonth, ExpYear}, {RamTable, RefId}) ->
	{Year, Month, _Day} = erlang:date(), 
	if (Year =< ExpYear) and (Month =< ExpMonth) ->
		[Session] = ets:lookup(RamTable, RefId),
		update_session(Session#session{ cardNumber = CardNumber, expiration = {ExpMonth, ExpYear}}, RefId, RamTable);
		true -> {error, card_invalid}
	end.

%% END - Client API

%% START - Support Functions
-spec tag_price('bike' | 'skateboard' | 'ski' | 'surfboard') -> 50 | 150 | 175.
tag_price(ski) -> 150;
tag_price(skateboard) -> 50;
tag_price(bike) -> 175;
tag_price(surfboard) -> 175.

-spec get_count(_,nonempty_maybe_improper_list()) -> any().
get_count(Item, [{Item, Count} | _Tail]) -> Count;
get_count(Item, [_Head | Tail]) -> get_count(Item, Tail).

-spec total_price([{'bike',number()} | {'skateboard',number()} | {'ski',number()} | {'surfboard',number()}]) -> number().
total_price([{Item,Count} | Tail])-> tag_price(Item) * Count + total_price(Tail);
total_price([]) -> 0.

-spec add_list(_,maybe_improper_list()) -> nonempty_maybe_improper_list().
add_list({Item, Count}, [{Item, PreviousCount} | Tail]) ->
	case (PreviousCount + Count) > 0 of
		true -> [{Item, PreviousCount + Count} | Tail];
		false -> [{Item, 0} | Tail]
	end;
add_list(Item, [_Head | Tail]) -> [_Head |add_list(Item, Tail)];
add_list(Item, []) -> [Item].

%% END - Support Functions