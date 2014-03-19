%% File: log_storage.erl
%% Description: Persistance log storage 
-module(log_storage).
-include("factory.hrl").

-export([create_storage/0,terminate_storage/0, delete_storage/0, update_log/1, remove_log/1]).
-export([add_log/2, get_log/1, get_allLogs/0]).

%% START - Maintenance and Operation API

-spec create_storage() -> ok.
create_storage() ->
	ets:new(logRam, [named_table, {keypos, #log.application}]),
	dets:open_file(logDisk, [{file, "LogStorageFile"}, {keypos, #log.application}]),
	restore_storage(). 

-spec terminate_storage() -> ok.
terminate_storage() ->
	ets:delete(logRam),
	dets:close(logDisk).

-spec restore_storage() -> any().
restore_storage() ->
	Insert = fun(#log{} = Log) ->
				ets:insert(logRam, Log),
				continue
			end,
	dets:traverse(logDisk, Insert).

-spec delete_storage() -> ok.
delete_storage() ->
	ets:delete(logRam),
	dets:close(logDisk),
	file:delete("LogStorageFile").

-spec update_log(record()) -> ok | {error, _}.
update_log(Log) ->
	ets:insert(logRam, Log),
	dets:insert(logDisk, Log).

-spec remove_log(string()) -> any().
remove_log(AppName) ->
	ets:delete(logRam, AppName),
	dets:delete(logDisk, AppName).

%% END - Maintenance and Operation API

%% START - Client API
-spec add_log(string(), list()) -> ok.
add_log(AppName, Reason) ->
	case ets:lookup(logRam, AppName) of
		[AppLog] ->
			update_log(#log{application = AppName, 
				reasonList = [{Reason, erlang:localtime()} | AppLog#log.reasonList]});
		_ -> update_log(#log{application = AppName, reasonList = [{Reason, erlang:localtime()}]})
	end.

-spec get_log(string()) -> ok.
get_log(AppName) ->
	case ets:lookup(logRam, AppName) of
		[AppLog] -> AppLog;
		_ -> {error, applog_missing}
	end.

-spec get_allLogs() -> ok.
get_allLogs() ->
	get_allLogs_int(ets:first(logRam)).
%% END - Client API

%% START - Support functions
get_allLogs_int('$end_of_table') -> [];
get_allLogs_int(Key) ->
	[AppLog] = ets:lookup(logRam, Key),
	[AppLog | get_allLogs_int(ets:next(logRam, Key))].
%% END - Support functions