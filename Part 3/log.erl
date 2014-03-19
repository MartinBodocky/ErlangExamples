%% File: log.erl
%% Description: Log service which log each crashed process in solution

-module(log).
-include("factory.hrl").

-export([start/0, stop/0, delete/0]).
-export([add_log/2, get_log/1, remove_log/1, get_allLogs/0]).
-export([init/1]).

%% START - Operation and Maitenance API

-spec start() -> 'ok' | {'error','timeout'}.
start() ->
	case whereis(?MODULE) of
		undefined -> 
			register(?MODULE, spawn(?MODULE, init, [self()])),
			receive
				started -> ok
			after 
				?TIMEOUT -> {error, timeout}
			end;
		_Pid -> ok
	end.

-spec stop() -> any().
stop() ->
	call(stop).

-spec delete() -> any().
delete() ->
	call(delete).

%% END - Operation and Maitenance API

%% START - Client API

-spec add_log(string(), list()) -> any().
add_log(AppName, Reason) ->
	call({add_log, AppName, Reason}).

-spec get_log(string()) -> any().
get_log(AppName) ->
	call({get_log, AppName}).

-spec remove_log(string()) -> any().
remove_log(AppName) ->
	call({remove_log, AppName}).

-spec get_allLogs() -> any().
get_allLogs() ->
	call(get_allLogs).

%% END - Client API

%% START - Operation and maintenance API
-spec init(pid()) -> ok.
init(Pid) ->
	log_storage:create_storage(),
	Pid ! started,
	loop().

-spec loop() -> ok.
loop() ->
	receive
		{request, From, delete} ->
			reply(From, log_storage:delete_storage());
		{request, From, stop} ->
			reply(From, log_storage:terminate_storage());
		{request, From, Request} ->
			reply(From, request(Request)),
			loop()
	end.

%% END - Operation and maintenance API

%% START - Messaging functions
-spec call(tuple()) -> any().
call(Request) ->
	case whereis(?MODULE) of
		undefined -> {error, instance};
		_Pid -> 
			Ref = make_ref(),
			?MODULE ! {request, {self(), Ref}, Request},
			receive
				{reply, Ref, Reply} -> Reply
			after
				?TIMEOUT -> {error, timeout}
			end
	end.

-spec reply({pid(), reference()},tuple()) -> {'reply',reference(),tuple()}.
reply({From, Ref}, Reply) ->
	From ! {reply, Ref, Reply}.

%% END - Messaging functions

%% START - Handling client requests

request({add_log, AppName, Reason}) ->
	log_storage:add_log(AppName, Reason);

request({remove_log, AppName}) ->
	log_storage:remove_log(AppName);

request({get_log, AppName}) ->
	log_storage:get_log(AppName);

request(get_allLogs) ->
	log_storage:get_allLogs().

%% END - Handling client requests