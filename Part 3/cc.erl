%% File : cc.erl
%% Description : Credit card service API
-module(cc).
-include("factory.hrl").

-export([start/0, stop/0, delete/0]).

-export([get_creditcard/1, add_creditcard/4, delete_creditcard/1, add_funds/2, 
	add_billingAddress/2, view_account/1, is_valid/3, transaction/4]).

-export([init/3]).

%% START - Operation and Maitenance API
-spec start() -> 'ok' | {'error','timeout'}.
start() ->
	% spawn process if is not already spawned
	case whereis(cc) of
		undefined ->
			% Create credit card provider process
			register(cc, spawn(cc, init, [?NODE3, ?NODE4, self()])),
			receive
				started -> ok
			after 
				?TIMEOUT -> {error, timeout}
			end;
		_Pid ->
			% All is already running, we are doing nothing here.
			ok
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

%% START - Internal Server Functions
init(PrimaryNode, SecondaryNode, Pid) ->
	spawn(PrimaryNode, cc_supervisor, init, [self()]),
	receive
		started -> ok
	after 
		?TIMEOUT -> ok
	end,
	spawn(SecondaryNode, cc_supervisor, init, [self()]),
	receive
		started -> ok
	after 
		?TIMEOUT -> ok
	end,
	Pid ! started,
	loop(PrimaryNode, SecondaryNode).

loop(PrimaryNode, SecondaryNode) ->
	receive
		{request, From, stop} ->
			remote_call(stop, PrimaryNode),
			remote_call(stop, SecondaryNode),
			reply(From, ok);
		{request, From, delete} ->
			remote_call(delete, PrimaryNode),
			remote_call(delete, SecondaryNode),
			reply(From, ok);
		{request, From, Request} ->
			case remote_call(Request, PrimaryNode) of
				{ok, Reply} -> 
					reply(From, Reply),
					%% send the same call to secondaryNode for reference
					remote_call(Request, SecondaryNode),
					loop(PrimaryNode, SecondaryNode);
				{nodedown, Node} -> 
					% need to swap primary node for secondary one
					case remote_call(Request, SecondaryNode) of
						{ok, Reply} -> reply(From, Reply),
										loop(SecondaryNode, PrimaryNode);
						{nodedown, Node} -> {error, nodedown}
					end;
				{error, timeout} -> {error, timeout}
			end;			
		{nodedown, Node} ->
			case Node =:= PrimaryNode of
				true -> loop(SecondaryNode, PrimaryNode);
				false -> loop(PrimaryNode, SecondaryNode)
			end
	end.

%% END - Internal Server Functions

%% START - Messaging functions
-spec call(tuple()) -> any().
call(Request) ->
	case whereis(cc) of
		undefined -> {error, instance};
		_Pid ->
			Ref = make_ref(),
			{cc, node()}! {request, {self(), Ref}, Request},
			receive
				{reply, Ref, Reply} -> Reply
			after
				?TIMEOUT -> {error, timeout}
			end
	end.

remote_call(Request, Node) ->
	case net_adm:ping(Node) of
		pong ->	
			%% it's alive you can sand message
			spawn(Node, cc_supervisor, init, [self()]),
			receive
				started -> ok
			after 
				?TIMEOUT -> ok
			end,
			monitor_node(Node, true),
			Ref = make_ref(),
			{cc_int, Node} ! {request, {self(), Ref}, Request},
			receive
				{reply, Ref, Reply} -> 
					monitor_node(Node, false),
					{ok, Reply};
				{nodedown, Node} -> {nodedown, Node}
			after
				?TIMEOUT -> {error, timeout}
			end;
		pang ->
			{nodedown, Node}
			%% it's dead we can do nothing for now
	end.

-spec reply({pid(), reference()},tuple()) -> {'reply',reference(),tuple()}.
reply({From, Ref}, Reply) ->
	From ! {reply, Ref, Reply}.

%% END - Messaging functions