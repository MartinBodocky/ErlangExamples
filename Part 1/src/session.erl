%% File : session.erl
%% Description : Module describes session process for each shopping cart, when it is sent to factory to buy it will destroy it self
-module(session).
-include("factory.hrl").

-export([start/2, stop/0, delete/0]).

-export([add_session/1, get_session/0, add_item/1, add_billingAddress/1, 
	add_creditcard/2, close_session/0, delete_sesion/0]).

-export([init/3]).

%% START - Operation and Maitenance API

-spec start(string(), string()) -> 'ok' | {'error','timeout'}.
start(UserName, RefId) ->
	register(?MODULE, spawn(?MODULE, init, [RefId, UserName, self()])),
	receive
		started -> ok
	after 
		?TIMEOUT -> {error, timeout}
	end.

-spec stop() -> any().
stop() ->
	call(stop).

-spec delete() -> any().
delete() ->
	call(delete).

%% END - Operation and Maitenance API

%% START - Client API
-spec add_session(string()) -> any().
add_session(UserName) ->
	call({add_session, UserName}).

-spec get_session() -> any().
get_session() ->
	call(get_session).

-spec add_item({atom(), number()}) -> any().
add_item({Type, Count}) ->
	call({add_item, {Type, Count}}).

-spec add_billingAddress({number(), list()}) -> any().
add_billingAddress(Address) ->
	call({add_billingAddress, Address}).

-spec add_creditcard(number(), {number(), number()}) -> any().
add_creditcard(CardNumber, Expiration) ->
	call({add_creditcard, CardNumber, Expiration}).

-spec close_session() -> any().
close_session() ->
	call(stop).

-spec delete_sesion() -> any().
delete_sesion() ->
	call(delete).

%% END - Client API

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

-spec reply({pid(), reference()},tuple()) -> {'reply',reference(),tuple()}.
reply({From, Ref}, Reply) ->
	From ! {reply, Ref, Reply}.

%% END - Messaging functions

%% START - Internal Server Functions
-spec init(string(), string(), pid()) -> any().
init(RefId, UserName, ParrentPid) ->
	RamTable = session_storage:open_storage(RefId),
	session_storage:add_session(UserName, {RamTable, RefId}),
	ParrentPid ! started,
	loop({RamTable, RefId}).

-spec loop({pid(), string()}) -> any().
loop({_RamTable, _RefId} = Data) ->
	receive
		{request, From, delete} ->
			reply(From, session_storage:delete_storage(Data));
		{request, From, stop} ->
			reply(From, session_storage:terminate_storage(Data));
		{request, From, Request} ->
			reply(From, request(Request, Data)),
			loop(Data)
	end.

%% END - Internal Server Functions

%% START - Handling client requests
request({add_session, UserName}, Data) ->
	session_storage:add_session(UserName, Data);

request(get_session, Data) ->
	session_storage:get_session(Data);

request({add_item,{Type, Count}}, Data) ->
	session_storage:add_item({Type, Count}, Data);

request({add_creditcard, CardNumber, Expiration}, Data) ->
	session_storage:add_creditcard( CardNumber, Expiration, Data);

request({add_billingAddress, Address}, Data) ->
	session_storage:add_billingAddress(Address, Data).

%% END - Handling client requests