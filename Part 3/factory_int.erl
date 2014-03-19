%% File: factory_int.erl
%% Description: It handles all communication among sessions and CC (it is created on demand)
-module(factory_int).
-include("factory.hrl").

-export([init/1]).

%% START - Messaging functions

-spec call(pid(), tuple()) -> any().
call(SessionPid, Request) ->
	Ref = make_ref(),
	%% we are calling session process
	SessionPid ! {request, {self(), Ref}, Request},
	receive
		{reply, Ref, Reply} -> Reply
	after 
		?TIMEOUT -> {error, timeout}
	end.

call_from_list([], _Request) -> ok;
call_from_list([SRef | Tail], Request) ->
	Ref = make_ref(),
	Pid = SRef#sRef.sessionPid,
	Pid ! {request, {self(), Ref}, Request},
	receive
		{reply, Ref, Reply} -> Reply
	after 
		?TIMEOUT -> ok
	end,
	call_from_list(Tail, Request).

-spec reply({pid(), reference()},tuple()) -> {'reply',reference(),tuple()}.
reply({From, Ref}, Reply) ->
	From ! {reply, Ref, Reply}.

%% END - Messaging functions

%% START - Internal Server Functions

-spec init(pid()) -> {'reply',reference(),tuple()}.
init(Pid) ->
	process_flag(trap_exit, true),
	%% open factory storage
	factory_storage:open_storage(),
	%% spawn all sessions which are in storage
	ListSessions = factory_storage:get_sessions(),
	CreatedSessions = create_sessions(ListSessions),
	factory_storage:update_sessionList(CreatedSessions),	
	Pid ! started,
	loop().

-spec loop() -> {'reply',reference(),tuple()}.
loop() ->
	receive
		{'EXIT', _From, shutdown} -> 
			loop();
		{'EXIT', _From, normal} -> 
			loop();
		{'EXIT', From, Reason} ->
			restart(From, Reason),
			loop();
		{request, From, delete} ->
			ListSessions = factory_storage:get_sessions(),
			call_from_list(ListSessions, delete),
			factory_storage:delete_storage(),
			cc:delete(),
			log:delete(),
			reply(From, ok);
		{request, From, stop} ->
			ListSessions = factory_storage:get_sessions(),
			call_from_list(ListSessions, stop),
			factory_storage:terminate_storage(),
			cc:stop(),
			log:stop(),
			reply(From, ok);
		{request, From, Request} ->
			reply(From, request(Request)),
			loop()
	end.

%% END - Internal Server Functions

%% START - Restart functions

restart(From, Reason) ->
	%% in this case we are just restart and write reason to log service
	SRef = factory_storage:get_sessionByPid(From),
	RefIdName = SRef#sRef.referenceId,
	log:add_log(RefIdName, Reason),
	AppLog = log:get_log(RefIdName),
	case length(AppLog#log.reasonList) rem 4 of
		0 -> exit(Reason);
		_ ->
			NewPid = spawn_link(session, init, [SRef#sRef.referenceId, SRef#sRef.userName, self()]),
			receive
				started -> 
					factory_storage:add_session(SRef#sRef{ sessionPid = NewPid})
			after
				?TIMEOUT -> {error, timeout}
			end
	end.

%% END - Restart functions

%% START - Handle Requests

request(all_sessions) ->
	factory_storage:get_sessions();

request({add_user, UserName, RefIdNew}) ->
	case factory_storage: get_session_userName(UserName) of
		{ok, RefId} -> {ok, RefId};
		{error, missing_session} ->
			SessionPid = create_session({RefIdNew, UserName}),
			factory_storage:add_session(#sRef{ userName = UserName, referenceId = RefIdNew, sessionPid = SessionPid}),
			{ok, RefIdNew}
	end;

request({add_item, RefId, {Type, Count}}) ->
	SRef = factory_storage:get_session(RefId),
	call(SRef#sRef.sessionPid, {add_item, {Type, Count}});

request({get_session, RefId}) ->
	SRef = factory_storage:get_session(RefId),
	call(SRef#sRef.sessionPid, get_session);

request({add_billingAddress, RefId, Address}) ->
	SRef = factory_storage:get_session(RefId),
	call(SRef#sRef.sessionPid, {add_billingAddress, Address});

request({add_creditcard, RefId, CardNumber, Expiration}) ->
	SRef = factory_storage:get_session(RefId),
	call(SRef#sRef.sessionPid, {add_creditcard, CardNumber, Expiration});	

request({get_session_pid, RefId}) ->
	SRef = factory_storage:get_session(RefId),
	SRef#sRef.sessionPid;

request({get_sessionByPid, Pid}) ->
	factory_storage:get_sessionByPid(Pid);

request({remove_session, RefId}) ->
	SRef = factory_storage:get_session(RefId),
	call(SRef#sRef.sessionPid, delete),
	factory_storage:remove_session(RefId);

request({user_history, RefId}) ->
	factory_storage:user_history(RefId);

request({add_transaction, RefId, TransactionId, Date, Cart, Price}) ->
	factory_storage:add_transaction(RefId, TransactionId, Date, Cart, Price);

request({buy, _RefId, Session}) ->
	%% validation of credict card and address in Credit provider
	case cc:is_valid(Session#session.billingAddress, Session#session.cardNumber,Session#session.expiration) of
		true -> cc:transaction(Session#session.billingAddress, 
				Session#session.cardNumber,Session#session.expiration,
				Session#session.price);
		false -> {error, card_invalid}
	end.

%% END - Handle Requests

%% START - Helper functions
create_session({RefId, UserName}) ->
	Pid  = spawn_link(session, init, [RefId, UserName, self()]),
	receive
		started -> ok
	after
		?TIMEOUT -> ok
	end,
	Pid.

create_sessions([SRef | Tail]) ->
	Pid = spawn_link(session, init, [SRef#sRef.referenceId, SRef#sRef.userName, self()]),
	receive
		started -> ok
	after 
		?TIMEOUT -> ok
	end,
	[SRef#sRef{ sessionPid = Pid} | create_sessions(Tail)];
create_sessions([]) -> [].

%% END - Helper functions