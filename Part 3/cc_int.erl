%% File : cc_int.erl
%% Description : Internal Credit card API
-module(cc_int).
-include("factory.hrl").

-export([init/1]).

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

-spec reply({pid(), reference()},tuple()) -> {'reply',reference(),tuple()}.
reply({From, Ref}, Reply) ->
	From ! {reply, Ref, Reply}.

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