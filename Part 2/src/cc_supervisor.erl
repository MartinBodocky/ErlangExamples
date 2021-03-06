%% File : cc_supervisor.erl
%% Description : Module create supervisor process witch restart credit card provider
-module(cc_supervisor).
-include("factory.hrl").

-export([start/0, stop/0]).
-export([init/1]).

start() ->
	register(?MODULE, spawn(?MODULE, init, [self()])),
	receive
		started -> ok
	after
		?TIMEOUT -> {error, timeou}
	end.

stop() ->
	call(stop).

%% START - Operation and maintenance API
-spec init(pid()) -> ok.
init(Pid) ->
	process_flag(trap_exit, true),
	register(cc, spawn_link(cc, init, [self()])),
	receive
		started -> 
			Pid ! started,
			loop()
	after
		?TIMEOUT -> {error, timeou}
	end.

-spec loop() -> ok.
loop() ->
	receive
		{'EXIT', _From, normal} -> ok;
		{'EXIT', From, Reason} ->
			restart(From, Reason),
			loop();
		{request, _From, stop} -> ok
	end.

%% END - Operation and maintenance API

%% START - Restart functions
restart(_From, Reason) ->
	%% in this case we are just restart and write reason to log service
	log:add_log("CC", Reason),
	register(cc, spawn_link(cc, init, [self()])),
	receive
		started -> ok
	after
		?TIMEOUT -> {error, timeou}
	end.

%% END - Restart

%% START - Messaging functions

-spec call(tuple()) -> any().
call(Request) ->
	Ref = make_ref(),
	?MODULE ! {request, {self(), Ref}, Request},
	receive
		{reply, Ref, Reply} -> Reply
	after
		?TIMEOUT -> {error, timeout}
	end.

%% END - Messaging functions