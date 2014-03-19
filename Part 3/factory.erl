-module(factory).
-include("factory.hrl").

-export([start_link/1, skateboard/2, surfboard/2, ski/2, bike/2, view_cart/1, get_session/1, billing_address/2,
	credit_card/3, user_history/1, buy/1, stop/0, delete/0, get_allSessions/0, get_session_pid/1, get_sessionByPid/1]).

-export([init/3]).

%% START - Client Service API
-spec start_link(string()) -> any().
start_link(UserName) ->
	% Warm up factory process
	warmup(),
	RefId = sf:guid_string(),
	call({add_user, UserName, RefId}).

-spec skateboard(string(),integer()) -> any().
skateboard(RefId, Count) ->
	call({add_item, RefId, {skateboard,Count}}).

-spec surfboard(string(),integer()) -> any().
surfboard(RefId, Count) ->
	call({add_item, RefId, {skateboard,Count}}).

-spec ski(string(),integer()) -> any().
ski(RefId, Count) ->
	call({add_item, RefId, {ski, Count}}).

-spec bike(string(),integer()) -> any().
bike(RefId, Count) ->
	call({add_item, RefId, {bike, Count}}).

-spec view_cart(string()) -> any().
view_cart(RefId) ->
	Session = call({get_session, RefId}),
	print_list(Session#session.cart),
	io:format("Total Price: ~p~n",[Session#session.price]).

-spec get_session(string()) -> any().
get_session(RefId) ->
	call({get_session, RefId}).

-spec billing_address(string(),list()) -> any().
billing_address(RefId, Address) ->
	if (is_list(Address) and (length(Address) == 4)) ->
		call({add_billingAddress, RefId, Address});
		true -> {error, invalid_address}
	end.

-spec credit_card(integer(),string(),tuple()) -> any().
credit_card(RefId, CardNumber, {ExpMonth, ExpYear}) ->
	{Year, Month, _Day} = erlang:date(),
	case is_integer(CardNumber) and (Year =< ExpYear) and (Month =< ExpMonth) of
		true ->
			call({add_creditcard, RefId, CardNumber, {ExpMonth, ExpYear}});
		_ -> {error, card_invalid}
	end.

-spec user_history(string()) -> any().
user_history(RefId) ->
	call({user_history, RefId}).

-spec buy(string()) -> any().
buy(RefId) ->
	Session = call({get_session, RefId}),
	CartCount = get_cart_count(Session#session.cart),
	if CartCount == 0 -> {error, empty_cart};
		true ->
			case call({buy, RefId, Session}) of
				{ok, TrxId} ->
					%% save to history 
					call({add_transaction, RefId, TrxId, erlang:localtime(), Session#session.cart, Session#session.price}),
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

call(Request) ->
	case whereis(factory) of
		% When process is not existed, it returns error token
		undefined -> {error, instance};
		% Process is running we can call it
		_Pid ->
			Ref = make_ref(),
			{factory , node()} ! {request, {self(), Ref}, Request},
			receive
				{reply, Ref, Reply} -> Reply
			after
				?TIMEOUT -> {error, timeout}
			end
	end.

remote_call(Request, Node) ->
	case net_adm:ping(Node) of
		pong ->	
			%% it's alive you can send message
			spawn(Node, factory_supervisor, init, [self()]),
			receive
				started -> ok
			after 
				?TIMEOUT -> ok
			end,
			monitor_node(Node, true),
			Ref = make_ref(),
			{factory_int, Node} ! {request, {self(), Ref}, Request},
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

reply({From, Ref}, Reply) ->
	From ! {reply, Ref, Reply}.

%% END - Messaging functions

%% START - Internal Server Functions
warmup() ->
	% spawn process if is not already spawned
	case whereis(factory) of
		undefined ->
			cc:start(),
			% Register factory process
			register(factory, spawn(factory, init, [?NODE1, ?NODE2, self()])),
			receive
				started -> ok
			after 
				?TIMEOUT -> {error, timeout}
			end;
		_Pid ->
			% All is already running, we are doing nothing here.
			ok
	end.

% @doc Initialize factory supervisors on nodes
-spec init(pid(), pid(), pid()) -> ok | {error, timeout}.
init(PrimaryNode, SecondaryNode, Pid) ->
	% Wait for each node to start
	spawn(PrimaryNode, factory_supervisor, init, [self()]),
	receive
		started -> ok
	after 
		?TIMEOUT -> ok
	end,
	spawn(SecondaryNode, factory_supervisor, init, [self()]),
	receive
		started -> ok
	after 
		?TIMEOUT -> ok
	end,
	Pid ! started,
	loop(PrimaryNode, SecondaryNode).

% @doc Loop function with reference to both nodes
-spec loop(pid(), pid()) -> any().
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

%% START - Helper functions

print_list([{Item, Count} | Tail]) -> io:format("~p: ~p ~n", [Item, Count]), print_list(Tail);
print_list([]) -> ok.

get_cart_count([{_Item, Count} | Tail]) -> Count + get_cart_count(Tail);
get_cart_count([]) -> 0.

%% END - Helper functions	