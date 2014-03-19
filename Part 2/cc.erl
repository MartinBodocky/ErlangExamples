%% File : cc.erl
%% Description : Credit card service API
-module(cc).
-include("factory.hrl").

-export([start/0, stop/0, delete/0]).
-export([get_creditcard/1, add_creditcard/4, delete_creditcard/1, add_funds/2, 
	add_billingAddress/2, view_account/1, is_valid/3, transaction/4]).
-export([init/1]).


%% START - Operation and Maitenance API
-spec start() -> 'ok' | {'error','timeout'}.
start() ->
	case whereis(?MODULE) of
		undefined -> 
			%% start log service
			log:start(),
			register(cc_supervisor, spawn(cc_supervisor, init, [self()])),
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
-spec get_creditcard(string()) -> any().
get_creditcard(UserName) ->
	call({get_creditcard, UserName}).

-spec add_creditcard(string(),integer(),integer(),tuple()) -> any().
add_creditcard(UserName, CardNumber, Bilance, Expiration) ->
	call({add_creditcard, UserName, CardNumber, Bilance, Expiration}).

-spec delete_creditcard(string()) -> any().
delete_creditcard(UserName) ->
	call({delete_creditcard, UserName}).

-spec add_funds(integer(),integer()) -> any().
add_funds(CardNumber, Amount) ->
	call({add_funds, CardNumber, Amount}).

-spec add_billingAddress(integer(),list()) -> any().
add_billingAddress(CardNumber, Address) ->
	call({add_billingAddress, CardNumber, Address}).

-spec view_account(integer()) -> any().
view_account(CardNumber) ->
	call({view_account, CardNumber}).

-spec is_valid(list(),integer(),tuple()) -> any().
is_valid(BillingAddress, CardNumber, Expiration) ->
	call({is_valid, BillingAddress, CardNumber, Expiration}).

-spec transaction(list(),integer(),tuple(),integer()) -> any().
transaction(BillingAddress, CardNumber, Expiration, Price) ->
	call({transaction, BillingAddress, CardNumber, Expiration, Price}).

%% END - Client API

%% START - Messaging functions
-spec call(tuple()) -> any().
call(Request) ->
	Ref = make_ref(),
	%% need to wait for recreating process
	case whereis(?MODULE) of
		undefined -> timer:sleep(1000);
		_ -> ok
	end,
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
-spec init(pid()) -> {'reply',reference(),tuple()}.
init(Pid) ->
	cc_storage:open_storage(),
	Pid ! started,
	loop().

-spec loop() -> {'reply',reference(),tuple()}.
loop() ->
	receive
		{request, From, delete} ->
			reply(From, cc_storage:delete_storage());
		{request, From, stop} ->
			reply(From, cc_storage:terminate_storage());
		{request, From, Request} ->
			reply(From, request(Request)),
			loop()
	end.

%% END - Internal Server Functions

%% START - Handling client requests
-spec request({'get_creditcard',string()} | {'view_account',integer()} | 
	{'add_billingAddress',integer(),list()} | 
	{'add_funds',integer(),integer()} | 
	{'is_valid',list(),integer(), tuple()} | 
	{'add_creditcard',string(),integer(),number(),tuple()} | 
	{'delete_creditcard',string()} | 
	{'transaction',list(),integer(),tuple(),number()}) -> any().
request({get_creditcard, UserName}) ->
	cc_storage:get_creditcard(UserName);

request({add_creditcard,UserName, CardNumber, Bilance, Expiration}) ->
	cc_storage:add_creditcard(UserName, CardNumber, Bilance, Expiration);

request({delete_creditcard, UserName}) ->
	cc_storage:delete_creditcard(UserName);

request({add_funds, CardNumber, Amount}) ->
	cc_storage:add_funds(CardNumber, Amount);

request({add_billingAddress, CardNumber, Address}) ->
	cc_storage:add_billingAddress(CardNumber, Address);

request({view_account, CardNumber}) ->
	cc_storage:view_account(CardNumber);

request({is_valid, BillingAddress, CardNumber, Expiration}) ->
	cc_storage:valid_creditcard(BillingAddress, CardNumber, Expiration);

request({transaction, BillingAddress, CardNumber, Expiration, Price}) ->
	case cc_storage:valid_creditcard(BillingAddress, CardNumber, Expiration) of
		true -> cc_storage:transaction(CardNumber, Price);
		false -> {error, card_invalid}
	end.

%% END - Handling client requests