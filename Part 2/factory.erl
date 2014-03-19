%% File: factory.erl
%% Description: It handles all communication among sessions and CC (it is created on demand)
-module(factory).
-include("factory.hrl").

-export([start_link/1, skateboard/2, surfboard/2, ski/2, bike/2, view_cart/1, get_session/1, billing_address/2,
	credit_card/3, user_history/1, buy/1, stop/0, delete/0, get_allSessions/0, get_session_pid/1]).

-export([init/1]).
-export([get_sessionByPid/1]).

warmup() ->
	% spawn process if is not already spawned
	case whereis(factory) of
		undefined ->
			% Start log and cc services
			log:start(),
			cc:start(),
			% Create factory supervisor
			register(factory_supervisor, spawn(factory_supervisor, init, [self()])),
			receive
				started -> ok
			after 
				?TIMEOUT -> {error, timeout}
			end;
		_Pid ->
			% All already si running, we are doing nothing here.
			ok
	end.

%% START - Client Service API

-spec start_link(string()) -> any().
start_link(UserName) ->
	% Warm up factory process
	warmup(),
	call({add_user, UserName}).

-spec skateboard(string(),integer()) -> any().
skateboard(RefId, Count) ->
	SessionPid = call({get_session_pid, RefId}),
	call(SessionPid, {add_item,{skateboard,Count}}).

-spec surfboard(string(),integer()) -> any().
surfboard(RefId, Count) ->
	SessionPid = call({get_session_pid, RefId}),
	call(SessionPid, {add_item,{surfboard,Count}}).

-spec ski(string(),integer()) -> any().
ski(RefId, Count) ->
	SessionPid = call({get_session_pid, RefId}),
	call(SessionPid, {add_item, {ski, Count}}).

-spec bike(string(),integer()) -> any().
bike(RefId, Count) ->
	SessionPid = call({get_session_pid, RefId}),
	call(SessionPid, {add_item, {bike, Count}}).

-spec view_cart(string()) -> any().
view_cart(RefId) ->
	SessionPid = call({get_session_pid, RefId}),
	Session = call(SessionPid, get_session),
	print_list(Session#session.cart),
	io:format("Total Price: ~p~n",[Session#session.price]).

-spec get_session(string()) -> any().
get_session(RefId) ->
	SessionPid = call({get_session_pid, RefId}),
	call(SessionPid, get_session).

-spec billing_address(string(),list()) -> any().
billing_address(RefId, Address) ->
	SessionPid = call({get_session_pid, RefId}),
	call(SessionPid, {add_billingAddress, Address}).

-spec credit_card(integer(),string(),tuple()) -> any().
credit_card(RefId, CardNumber, Expiration) ->
	case is_integer(CardNumber) of
		true ->
			SessionPid = call({get_session_pid, RefId}),
			call(SessionPid, {add_creditcard, CardNumber, Expiration});
		_ -> {error, card_invalid}
	end.

-spec user_history(string()) -> any().
user_history(RefId) ->
	call({user_history, RefId}).

-spec buy(string()) -> any().
buy(RefId) ->
	SessionPid = call({get_session_pid, RefId}),
	Session = call(SessionPid, get_session),
	CartCount = get_cart_count(Session#session.cart),
	if CartCount == 0 -> {error, empty_cart};
		true ->
			case call({buy, RefId, Session}) of
				{ok, TrxId} ->
					%% save to history 
					call({add_transaction, RefId, TrxId, erlang:localtime(), Session#session.cart, Session#session.price}),
					%% delete session
					call(SessionPid, delete),
					%% delete in factory storage
					call({remove_session, RefId}),
					{ok, TrxId};
				{error, Reason} -> {error, Reason}
			end
	end.

stop() ->
	call(stop).

delete() ->
	call(delete).

get_session_pid(RefId) ->
	call({get_session_pid, RefId}).

get_sessionByPid(Pid) ->
	call({get_sessionByPid, Pid}).

get_allSessions() ->
	call(all_sessions).

%% END - Client Service API

%% START - Messaging functions

-spec call(tuple()) -> any().
call(Request) ->
	Ref = make_ref(),
	factory ! {request, {self(), Ref}, Request},
	receive
		{reply, Ref, Reply} -> Reply
	after
		?TIMEOUT -> {error, timeout}
	end.

-spec call(pid(), tuple()) -> any().
call(SessionPid, Request) ->
	Ref = make_ref(),
	%% now we are calling session process
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
			reply(From, ok);
		{request, From, stop} ->
			ListSessions = factory_storage:get_sessions(),
			call_from_list(ListSessions, stop),
			factory_storage:terminate_storage(),
			cc:stop(),
			reply(From, ok);
		{request, From, Request} ->
			reply(From, request(Request)),
			loop()
	end.

%% END - Internal Server Functions

%% START - Restart functions

restart(From, Reason) ->
	%% we are restaring three times and then restart whole tree
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

request({add_user, UserName}) ->
	case factory_storage: get_session_userName(UserName) of
		{ok, RefId} -> {ok, RefId};
		{error, missing_session} ->
			RefId = sf:guid_string(),
			SessionPid = create_session({RefId, UserName}),
			factory_storage:add_session(#sRef{ userName = UserName, referenceId = RefId, sessionPid = SessionPid}),
			{ok, RefId}
	end;	

request({get_session_pid, RefId}) ->
	SRef = factory_storage:get_session(RefId),
	SRef#sRef.sessionPid;

request({get_sessionByPid, Pid}) ->
	factory_storage:get_sessionByPid(Pid);

request({remove_session, RefId}) ->
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
	%Pid  = spawn(session, init, [RefId, UserName, self()]),
	receive
		started -> ok
	after
		?TIMEOUT -> ok
	end,
	Pid.

create_sessions([SRef | Tail]) ->
	Pid = spawn_link(session, init, [SRef#sRef.referenceId, SRef#sRef.userName, self()]),
	%Pid = spawn(session, init, [SRef#sRef.referenceId, SRef#sRef.userName, self()]),
	receive
		started -> ok
	after 
		?TIMEOUT -> ok
	end,
	[SRef#sRef{ sessionPid = Pid} | create_sessions(Tail)];
create_sessions([]) -> [].

print_list([{Item, Count} | Tail]) -> io:format("~p: ~p ~n", [Item, Count]), print_list(Tail);
print_list([]) -> ok.

get_cart_count([{_Item, Count} | Tail]) -> Count + get_cart_count(Tail);
get_cart_count([]) -> 0.

%% END - Helper functions