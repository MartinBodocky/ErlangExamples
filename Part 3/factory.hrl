%%% factory.hrl
%%% Include records for Shopping cart and for Credit Card provider
%% cover:compile_directory().

-include_lib("stdlib/include/ms_transform.hrl").

% Define timeout for receive block
-define(TIMEOUT, 10000).
% Define names for all nodes
-define(NODE1, 'factory1@local').
-define(NODE2, 'factory2@local').
-define(NODE3, 'cc1@local').
-define(NODE4, 'cc2@local').
-define(DEBUG, false).

%% Session record
-record(session, {userName, 
			referenceId, 
			cardNumber = 0, 
			cart = [{ski, 0}, {skateboard, 0}, {surfboard, 0}, {bike, 0}], 
			price = 0, 
			billingAddress = '',
			expiration = {00, 00}}).

%% Session Reference
-record(sRef, {userName, 
			referenceId, 
			sessionPid}).

%% User's history record
-record(uHis, {
			referenceId,
			cart = [], 
			price = 0, 
			date,
			transactionId}).

%% Credit card record
-record(card, {cardNumber, 
			userName, 
			address = [], 
			bilance = 0,
			expiration,
			transactions = []}).

% log record
-record(log, {application, reasonList}).
