-module(tests).
-compile(export_all).
-include("factory.hrl").
-include_lib("eunit/include/eunit.hrl").


factory_supervisor2_test() ->
	{ok, Ref1} = factory:start_link("Maria"),
	{ok, Ref2} = factory:start_link("Martin"),
	{ok, Ref3} = factory:start_link("Jan"),
	Added = factory:skateboard(Ref1, 2),
	?assertEqual([{added, 2}, {total, 2}], Added),
	Added2 = factory:bike(Ref2, 2),
	?assertEqual([{added, 2}, {total, 2}], Added2),
	Added3 = factory:surfboard(Ref3, 2),
	?assertEqual([{added, 2}, {total, 2}], Added3),
	Ref1Pid  = factory:get_session_pid(Ref1),
	?assertEqual(true, exit(Ref1Pid, kill)),
	%% mystery for now
	timer:sleep(2000),
	Cart1 = factory:get_session(Ref1),
	?assertEqual([{ski, 0}, {skateboard, 2}, {surfboard, 0}, {bike, 0}], Cart1#session.cart),
	?assertEqual(100, Cart1#session.price),
	Cart2 = factory:get_session(Ref2),
	?assertEqual([{ski, 0}, {skateboard, 0}, {surfboard, 0}, {bike, 2}], Cart2#session.cart),
	?assertEqual(350, Cart2#session.price),
	Cart3 = factory:get_session(Ref3),
	?assertEqual([{ski, 0}, {skateboard, 0}, {surfboard, 2}, {bike, 0}], Cart3#session.cart),
	?assertEqual(350, Cart3#session.price),
	AppLog = log:get_log(Ref1),
	?assertEqual(1, length(AppLog#log.reasonList)),
	[{Reason, Date}] = AppLog#log.reasonList,
	?assertEqual({killed, Date}, {Reason, Date}),
	?assertEqual(ok, log:delete()),
	?assertEqual(ok, factory:delete()).


factory_supervisor1_test() ->
	{ok, Ref1} = factory:start_link("Maria"),
	{ok, Ref2} = factory:start_link("Martin"),
	{ok, Ref3} = factory:start_link("Jan"),
	Added = factory:skateboard(Ref1, 2),
	?assertEqual([{added, 2}, {total, 2}], Added),
	Added2 = factory:bike(Ref2, 2),
	?assertEqual([{added, 2}, {total, 2}], Added2),
	Added3 = factory:surfboard(Ref3, 2),
	?assertEqual([{added, 2}, {total, 2}], Added3),
	?assertEqual(true, exit(whereis(factory), kill)),
	%% mystery for now
	timer:sleep(1000),
	Cart1 = factory:get_session(Ref1),
	?assertEqual([{ski, 0}, {skateboard, 2}, {surfboard, 0}, {bike, 0}], Cart1#session.cart),
	?assertEqual(100, Cart1#session.price),
	Cart2 = factory:get_session(Ref2),
	?assertEqual([{ski, 0}, {skateboard, 0}, {surfboard, 0}, {bike, 2}], Cart2#session.cart),
	?assertEqual(350, Cart2#session.price),
	Cart3 = factory:get_session(Ref3),
	?assertEqual([{ski, 0}, {skateboard, 0}, {surfboard, 2}, {bike, 0}], Cart3#session.cart),
	?assertEqual(350, Cart3#session.price),
	AppLog = log:get_log("Factory"),
	?assertEqual(1, length(AppLog#log.reasonList)),
	[{Reason, Date}] = AppLog#log.reasonList,
	?assertEqual({killed, Date}, {Reason, Date}),
	?assertEqual(ok, log:delete()),
	?assertEqual(ok, factory:delete()).

cc_supervisor1_test() ->
	?assertEqual(ok, cc:start()),
	CardNumber = 123456789,
	?assertEqual(CardNumber, cc:add_creditcard("Maria", 123456789, 1900, {11,2016})),
	?assertEqual(CardNumber, cc:get_creditcard("Maria")),
	?assertEqual(true, exit(whereis(cc), kill)),
	%% mystery for now
	timer:sleep(1000),
	?assertEqual(CardNumber, cc:get_creditcard("Maria")),
	AppLog = log:get_log("CC"),
	?assertEqual(1, length(AppLog#log.reasonList)),
	[{Reason, Date}] = AppLog#log.reasonList,
	?assertEqual({killed, Date}, {Reason, Date}),
	?assertEqual(ok, log:delete()),
	?assertEqual(ok, cc:delete()).

cc_supervisor2_test() ->
	?assertEqual(ok, cc:start()),
	CardNumber = 123456789,
	?assertEqual(CardNumber, cc:add_creditcard("Maria", 123456789, 1900, {11,2016})),
	?assertEqual(CardNumber, cc:get_creditcard("Maria")),
	?assertEqual(true, exit(whereis(cc), bye)),
	%% mystery for now
	timer:sleep(1000),
	?assertEqual(CardNumber, cc:get_creditcard("Maria")),
	AppLog = log:get_log("CC"),
	?assertEqual(1, length(AppLog#log.reasonList)),
	[{Reason, Date}] = AppLog#log.reasonList,
	?assertEqual({bye, Date}, {Reason, Date}),
	?assertEqual(ok, log:delete()),
	?assertEqual(ok, cc:delete()).

log_start_test() ->
	?assertEqual(ok, log:start()),
	?assertEqual(ok, log:delete()).

log_addLog1_test() ->
	AppName = "Factory",
	?assertEqual(ok, log:start()),
	?assertEqual(ok, log:add_log(AppName, {crash,"FileXYZ"})),
	AppLog = log:get_log(AppName),
	?assertEqual(AppLog#log.application, AppName),
	?assertEqual(1, length(AppLog#log.reasonList)),
	?assertEqual(ok, log:delete()).

log_addLog2_test() ->
	AppName = "Factory",
	?assertEqual(ok, log:start()),
	?assertEqual(ok, log:add_log(AppName, {crash,"FileXYZ"})),
	?assertEqual(ok, log:add_log(AppName, {crash,"LoteryAA"})),
	AppLog = log:get_log(AppName),
	?assertEqual(AppLog#log.application, AppName),
	?assertEqual(2, length(AppLog#log.reasonList)),
	?assertEqual(ok, log:delete()).

log_addLog3_test() ->
	AppName = "Factory",
	?assertEqual(ok, log:start()),
	?assertEqual(ok, log:add_log(AppName, {crash,"FileXYZ"})),
	?assertEqual(ok, log:add_log(AppName, {crash,"LoteryAA"})),
	?assertEqual(ok, log:add_log(AppName, {crash,"LoteryTT"})),
	AppLog = log:get_log(AppName),
	?assertEqual(AppLog#log.application, AppName),
	?assertEqual(3, length(AppLog#log.reasonList)),
	?assertEqual(ok,log:remove_log(AppName)),
	?assertEqual({error, applog_missing}, log:get_log(AppName)),
	?assertEqual(ok, log:delete()).

factory_startlink_test() ->
	{ok, Ref} = factory:start_link("Maria"),
	[SRef] = factory:get_allSessions(),
	?assertEqual(SRef#sRef.referenceId, Ref),
	?assertEqual(ok, factory:delete()).

factory_startlink_multi_test() ->
	{ok, Ref1} = factory:start_link("Maria"),
	{ok, Ref2} = factory:start_link("Martin"),
	{ok, Ref3} = factory:start_link("Jan"),
	List = factory:get_allSessions(),
	?assertEqual(3, length(List)),
	?assertEqual(ok, factory:delete()).

factory_startlink_reload_test() ->
	{ok, Ref} = factory:start_link("Maria"),
	factory:stop(),
	?assertEqual({ok, Ref} , factory:start_link("Maria")),
	?assertEqual(ok, factory:delete()).

factory_startlink_delete_test() ->
	{ok, Ref} = factory:start_link("Maria"),
	?assertEqual(ok, factory:delete()),
	{ok, Ref2} = factory:start_link("Maria"),
	?assert(Ref =/= Ref2),
	?assertEqual(ok, factory:delete()).

factory_addItem1_test() ->
	{ok, Ref} = factory:start_link("Maria"),
	Added = factory:bike(Ref, 2),
	?assertEqual([{added, 2}, {total, 2}], Added),
	Session = factory:get_session(Ref),
	?assertEqual([{ski, 0}, {skateboard, 0}, {surfboard, 0}, {bike, 2}], Session#session.cart),
	?assertEqual(350, Session#session.price),
	?assertEqual(ok, factory:delete()).

factory_addItem2_test() ->
	{ok, Ref} = factory:start_link("Maria"),
	Added = factory:skateboard(Ref, 2),
	?assertEqual([{added, 2}, {total, 2}], Added),
	Added2 = factory:bike(Ref, 2),
	?assertEqual([{added, 2}, {total, 2}], Added2),
	Session = factory:get_session(Ref),
	?assertEqual([{ski, 0}, {skateboard, 2}, {surfboard, 0}, {bike, 2}], Session#session.cart),
	?assertEqual(450, Session#session.price),
	?assertEqual(ok, factory:delete()).

factory_addItem3_test() ->
	{ok, Ref} = factory:start_link("Maria"),
	Added = factory:skateboard(Ref, 2),
	?assertEqual([{added, 2}, {total, 2}], Added),
	Added2 = factory:bike(Ref, 2),
	?assertEqual([{added, 2}, {total, 2}], Added2),
	Added3 = factory:bike(Ref, 2),
	?assertEqual([{added, 2}, {total, 4}], Added3),
	Session = factory:get_session(Ref),
	?assertEqual([{ski, 0}, {skateboard, 2}, {surfboard, 0}, {bike, 4}], Session#session.cart),
	?assertEqual(800, Session#session.price),
	?assertEqual(ok, factory:delete()).

factory_addItem4_test() ->
	{ok, Ref} = factory:start_link("Maria"),
	Added = factory:skateboard(Ref, 2),
	?assertEqual([{added, 2}, {total, 2}], Added),
	Added2 = factory:bike(Ref, 2),
	?assertEqual([{added, 2}, {total, 2}], Added2),
	Removed = factory:bike(Ref, -2),
	?assertEqual([{removed, 2}, {total, 0}], Removed),
	Cart = factory:get_session(Ref),
	?assertEqual([{ski, 0}, {skateboard, 2}, {surfboard, 0}, {bike, 0}], Cart#session.cart),
	?assertEqual(100, Cart#session.price),
	?assertEqual(ok, factory:delete()).

factory_billingAddress_test() ->
	{ok, Ref} = factory:start_link("Maria"),
	Address = [{address, {76, "Leander Way"}}, {name, "Maria Ivanova"}, {city, "London"}, {country, "UK"}],
	AddressFake = [{address, {76, "Leander Way"}}, {city, "London"}, {country, "UK"}],
	?assertEqual(ok, factory:billing_address(Ref, Address)),
	?assertEqual({error, invalid_address}, factory:billing_address(Ref, AddressFake)),
	Session = factory:get_session(Ref),
	?assertEqual(Address, Session#session.billingAddress),
	?assertEqual(ok, factory:delete()).

factory_creditcard_test() ->
	{ok, Ref} = factory:start_link("Maria"),
	CardNumber = 361623751735127,
	Expiration = {12, 2017},
	ExpirationOld = {12,2012},
	?assertEqual({error, card_invalid}, factory:credit_card(Ref, {123123,312312,31231}, Expiration)),
	?assertEqual({error, card_invalid}, factory:credit_card(Ref, CardNumber, ExpirationOld)),
	?assertEqual(ok, factory:credit_card(Ref, CardNumber, Expiration)),
	Session = factory:get_session(Ref),
	?assertEqual(CardNumber, Session#session.cardNumber),
	?assertEqual(ok, factory:delete()).

factory_buy1_test() ->
	{ok, Ref} = factory:start_link("Maria"),
	Address = [{address, {76, "Leander Way"}}, {name, "Maria Ivanova"}, {city, "London"}, {country, "UK"}],
	CardNumber = 361623751735127,
	Expiration = {12, 2017},
	?assertEqual(CardNumber, cc:add_creditcard("Maria", CardNumber, 1900, Expiration)),
	?assertEqual(ok, cc:add_billingAddress(CardNumber, Address)),
	?assertEqual(ok, factory:credit_card(Ref, CardNumber, Expiration)),
	?assertEqual(ok, factory:billing_address(Ref, Address)),
	Added = factory:bike(Ref, 2),
	?assertEqual([{added, 2}, {total, 2}], Added),
	Session = factory:get_session(Ref),
	?assertEqual(350, Session#session.price),
	{ok, TrxId} = factory:buy(Ref),
	?assertEqual(ok, factory:delete()).

factory_buy2_test() ->
	{ok, Ref} = factory:start_link("Maria"),
	Address = [{address, {76, "Leander Way"}}, {name, "Maria Ivanova"}, {city, "London"}, {country, "UK"}],
	CardNumber = 361623751735127,
	Expiration = {12, 2017},
	?assertEqual(CardNumber, cc:add_creditcard("Maria", CardNumber, 1900, Expiration)),
	?assertEqual(ok, cc:add_billingAddress(CardNumber, Address)),
	?assertEqual(ok, factory:credit_card(Ref, CardNumber, Expiration)),
	?assertEqual(ok, factory:billing_address(Ref, Address)),
	Added = factory:bike(Ref, 20),
	?assertEqual([{added, 20}, {total, 20}], Added),
	Session = factory:get_session(Ref),
	?assertEqual(3500, Session#session.price),
	?assertEqual({error, insufficient_funds}, factory:buy(Ref)),
	factory:bike(Ref, -10),
	{ok, TrxId} = factory:buy(Ref),
	[History] = factory:user_history(Ref),
	?assertEqual(History#uHis.transactionId, TrxId),
	?assertEqual(ok, factory:delete()).

session_add_session_test() ->
	?assertEqual(ok, session:start("Maria", "ueoquw-test")),
	Session = session:get_session(),
	?assertEqual("Maria", Session#session.userName),
	?assertEqual(ok, session:delete()).

session_add_item_test() ->
	?assertEqual(ok, session:start("Maria", "ueoquw-test")),
	Added = session:add_item({bike,2}),
	Session = session:get_session(),
	?assertEqual([{added, 2}, {total, 2}], Added),
	?assertEqual([{ski, 0}, {skateboard, 0}, {surfboard, 0}, {bike, 2}], Session#session.cart),
	?assertEqual(ok, session:delete()).

session_add_billingAddress_test() ->
	?assertEqual(ok, session:start("Maria", "ueoquw-test")),
	Address = [{address, {76, "Leander Way"}}, {name, "Maria Ivanova"}, {city, "London"}, {country, "UK"}],
	?assertEqual(ok, session:add_billingAddress(Address)),
	Session = session:get_session(),
	?assertEqual(Address, Session#session.billingAddress),
	?assertEqual(ok, session:delete()).

session_add_creditcard_test() ->
	?assertEqual(ok, session:start("Maria", "ueoquw-test")),
	?assertEqual({error, card_invalid}, session:add_creditcard(123456789, {2014, 4})),
	?assertEqual(ok, session:add_creditcard(123456789, {11, 2016})),
	Session = session:get_session(),
	?assertEqual(123456789, Session#session.cardNumber),
	?assertEqual(ok, session:delete()).

cc_add_creditcard_test() ->
	?assertEqual(ok, cc:start()),
	CardNumber = 123456789,
	?assertEqual(CardNumber, cc:add_creditcard("Maria", 123456789, 1900, {11, 2014})),
	?assertEqual(CardNumber, cc:get_creditcard("Maria")),
	?assertEqual(ok, cc:delete()).

cc_delete_creditcard_test() ->
	?assertEqual(ok, cc:start()),
	CardNumber = 123456789,
	?assertEqual(CardNumber, cc:add_creditcard("Maria", 123456789, 1900, {11,2016})),
	?assertEqual(CardNumber, cc:get_creditcard("Maria")),
	?assertEqual(ok, cc:delete_creditcard("Maria")),
	?assertEqual({error, card_missing}, cc:get_creditcard("Maria")),
	?assertEqual(ok, cc:delete()).

cc_add_funds_test() ->
	?assertEqual(ok, cc:start()),
	CardNumber = 123456789,
	?assertEqual(CardNumber, cc:add_creditcard("Maria", 123456789, 1900, {11, 2017})),
	?assertEqual(ok, cc:add_funds(CardNumber, 1000)),
	Card = cc:view_account(CardNumber),
	?assertEqual(2900,Card#card.bilance),
	?assertEqual(ok, cc:delete()).

cc_add_billingAddress_test() ->
	?assertEqual(ok, cc:start()),
	CardNumber = 123456789,
	?assertEqual(CardNumber, cc:add_creditcard("Maria", 123456789, 1900, {12,2015})),
	Address = [{address, {76, "Leander Way"}}, {name, "Maria Ivanova"}, {city, "London"}, {country, "UK"}],
	?assertEqual(ok, cc:add_billingAddress(CardNumber, Address)),
	Card = cc:view_account(CardNumber),
	?assertEqual(Address, Card#card.address),
	?assertEqual(ok, cc:delete()).

cc_is_valid_test() ->
	?assertEqual(ok, cc:start()),
	CardNumber = 123456789,
	?assertEqual(CardNumber, cc:add_creditcard("Maria", 123456789, 1900, {9, 2013})),
	Address = [{address, {76, "Leander Way"}}, {name, "Maria Ivanova"}, {city, "London"}, {country, "UK"}],
	?assertEqual(ok, cc:add_billingAddress(CardNumber, Address)),
	?assertEqual(true, cc:is_valid(Address, CardNumber, {9, 2013})),
	?assertEqual(false, cc:is_valid(Address, CardNumber, {9, 2018})),
	?assertEqual(ok, cc:delete()).

cc_transaction_test() ->
	?assertEqual(ok, cc:start()),
	CardNumber = 123456789,
	?assertEqual(CardNumber, cc:add_creditcard("Maria", 123456789, 1900, {9, 2013})),
	Address = [{address, {76, "Leander Way"}}, {name, "Maria Ivanova"}, {city, "London"}, {country, "UK"}],
	?assertEqual(ok, cc:add_billingAddress(CardNumber, Address)),
	{ok, _TrxId} = cc:transaction(Address, CardNumber, {9, 2013}, 1000),
	Card = cc:view_account(CardNumber),
	?assertEqual(900, Card#card.bilance),
	?assertEqual({error, insufficient_funds}, cc:transaction(Address, CardNumber, {9, 2013}, 1000)),
	?assertEqual({error, card_invalid}, cc:transaction(Address, CardNumber, {12, 2013}, 1000)),
	?assertEqual(ok, cc:delete()).
