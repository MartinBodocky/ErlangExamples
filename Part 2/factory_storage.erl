%% File : factory_storage.erl
%% Description : It saves collection of bought shopping carts and list of current processes
-module(factory_storage).
-include("factory.hrl").
-include_lib("/usr/local/lib/erlang/lib/stdlib-1.19.4/include/ms_transform.hrl").

-export([open_storage/0, terminate_storage/0, delete_storage/0, update_session/1, remove_session/1]).
-export([get_session/1, get_session_userName/1, get_sessions/0, add_session/1, update_sessionList/1,
	add_transaction/5, user_history/1, get_sessionByPid/1]).

%% START - Operation and Maintenance API

-spec open_storage() -> ok.
open_storage() ->
	%% Data storage for keep list of current sessions
	ets:new(factoryRam, [named_table, {keypos, #sRef.referenceId}]),
	dets:open_file(factoryDisk, [{file, "SessionList"}, {keypos, #sRef.referenceId}]),
	%% Data storage for keep list of successful transactions
	ets:new(historyRam, [named_table, duplicate_bag, {keypos, #uHis.referenceId}]),
	dets:open_file(historyDisk, [{file, "TransactionList"}, {type, duplicate_bag}, {keypos, #uHis.referenceId}]),
	%% restore storage
	restore_storage().

-spec terminate_storage() -> ok.
terminate_storage() ->
	ets:delete(factoryRam),
	dets:close(factoryDisk),
	ets:delete(historyRam),
	dets:close(historyDisk).

-spec delete_storage() -> ok.
delete_storage() ->
	ets:delete(factoryRam),
	dets:close(factoryDisk),
	file:delete("SessionList"),
	ets:delete(historyRam),
	dets:close(historyDisk),
	file:delete("TransactionList").

-spec restore_storage() -> any().
restore_storage() ->
	InsertFactory = fun(#sRef{} = SRef) ->
				ets:insert(factoryRam, SRef),
				continue
			end,
	InsertHistory = fun(#uHis{} = History) ->
				ets:insert(historyRam, History),
				continue
			end,
	dets:traverse(factoryDisk, InsertFactory),
	dets:traverse(historyDisk, InsertHistory).

-spec update_session(record()) -> any().
update_session(SRef) ->
	ets:insert(factoryRam, SRef),
	dets:insert(factoryDisk, SRef).

-spec remove_session(string()) -> any().
remove_session(RefId) ->
	ets:delete(factoryRam, RefId),
	dets:delete(factoryDisk, RefId).

%% END - Operation and Maintenance API

%% START - Client API
-spec get_session(string()) -> record() | {error, missing_session}.
get_session(RefId) ->
	case ets:lookup(factoryRam, RefId) of
		[SRef] -> SRef;
		_ -> {error, missing_session}
	end.

-spec get_session_userName(string()) -> record() | {error, missing_session}.
get_session_userName(UserName) ->
	Select = ets:fun2ms(fun(#sRef{} = SRef) when SRef#sRef.userName == UserName -> SRef end),
	%% get name of app to restart
	case ets:select(factoryRam, Select) of
		[SRef] -> {ok, SRef#sRef.referenceId};
		_ -> {error, missing_session}
	end.

-spec get_sessionByPid(string()) -> record() | {error, missing_session}.
get_sessionByPid(Pid) ->
	Select = ets:fun2ms(fun(#sRef{} = SRef) when SRef#sRef.sessionPid == Pid -> SRef end),
	%% get name of app to restart
	case ets:select(factoryRam, Select) of
		[SRef] -> SRef;
		_ -> {error, missing_session}
	end.

-spec get_sessions() -> any().
get_sessions() ->
	get_sessions_int(ets:first(factoryRam)).

-spec add_session(record()) -> ok.
add_session(SRef) ->
	update_session(SRef).

-spec update_sessionList(list()) -> ok.
update_sessionList([]) -> ok;
update_sessionList([SRef | Tail]) ->
	update_session(SRef), update_sessionList(Tail).

-spec add_transaction(string(),string(),list(),list(),number()) -> 'ok' | {'error',_}.
add_transaction(RefId, TransactionId, Date, Cart, Price) ->
	HisItem = #uHis{ referenceId = RefId, date = Date, transactionId = TransactionId, price = Price, cart = Cart },
	ets:insert(historyRam, HisItem),
	dets:insert(historyDisk, HisItem).

-spec user_history(string) -> {error, missing_session} | list().
user_history(RefId) ->
	case ets:lookup(historyRam, RefId) of
		[] -> {error, missing_session};
		List -> List
	end.

%% END - Client API

%% START - Support functions
get_sessions_int('$end_of_table') -> [];	
get_sessions_int(Key) ->
	[SRef] = ets:lookup(factoryRam, Key),
	[SRef | get_sessions_int(ets:next(factoryRam, Key))].

%% END - Support functions