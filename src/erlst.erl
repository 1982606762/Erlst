-module(erlst).
-behaviour(gen_server).
% You are allowed to split your Erlang code in as many files as you
% find appropriate.
% However, you MUST have a module (this file) called erlst.

% Export at least the API:
-export([launch/0,
         shutdown/1,
         open_account/2,
         account_balance/1,
         make_offer/2,
         rescind_offer/2,
         add_trader/2,
         remove_trader/2,
         show/1
        ]).

% You may have other exports as well
-export([init/1,terminate/2,handle_call/3,handle_cast/2]).
-export([removeoffer/3,run_strategy/2,check_offer_exists/1,run_all_traders/2,run_all_Map_Res/4
    ,transfer_stock/2,change_stock/4]).

-type stock_exchange() :: term().
-type account_id() :: term().
-type offer_id() :: term().
-type trader_id() :: term().
-type stock() :: atom().
-type isk() :: non_neg_integer().
-type stock_amount() :: pos_integer().
-type holdings() :: {isk(), [{stock(), stock_amount()}]}.
-type offer() :: {stock(), isk()}.
-type decision() :: accept | reject.
-type trader_strategy() :: fun((offer()) -> decision()).

% --------------------------user API----------------
-spec launch() -> {ok, stock_exchange()} | {error, term()}.
launch() ->
    gen_server:start_link(?MODULE, [], []).

-spec shutdown(S :: stock_exchange()) -> non_neg_integer().
shutdown(S) ->
    Ret = gen_server:call(S, shutdown),
    Ret.

-spec open_account(S :: stock_exchange(), holdings()) -> account_id().
open_account(S,Holdings) ->
    gen_server:call(S, {open_account, Holdings,S}).

-spec account_balance(Acct :: account_id()) -> holdings().
account_balance(Acct) ->
    case Acct of
        {Server,Id} ->
            gen_server:call(Server, {account_balance, Id});
        _ ->
            {error, badAcct}
    end.

-spec make_offer(Acct :: account_id(), Terms :: offer()) -> {ok, offer_id()} | {error, term()}.
make_offer(Acct, Offer) ->
    case check_Account_exist(Acct) of
        true ->
            case Acct of
                {Server,Id} ->
                    gen_server:call(Server, {make_offer, Offer, Id});
                _ ->
                    {error, badAcct}
            end;
        false ->
            {error, notexist}
    end.

-spec rescind_offer(Acct :: account_id(), Offer :: offer_id()) -> ok.
rescind_offer(Acct, Offer) ->
    case check_Account_exist(Acct) of
        true ->
            case Acct of
                {Server,Id} ->
                    gen_server:cast(Server, {rescind_offer, Offer, Id});
                _ ->
                    {error, badAcct}
            end;
        false ->
            {error, notexist}
    end.

-spec add_trader(Acct :: account_id(), Strategy :: trader_strategy()) -> trader_id().
add_trader(Acct, Strategy) ->
    case check_Account_exist(Acct) of
        true ->
            case Acct of
                {Server,Id} ->
                    gen_server:call(Server, {add_trader, Strategy, Id});
                _ ->
                    {error, badAcct}
            end;
        false ->
            {error, notexist}
    end.
    

-spec remove_trader(Acct :: account_id(), Trader :: trader_id()) -> ok.
remove_trader(Acct, Trader) ->
    case check_Account_exist(Acct) of
        true ->
            case Acct of
                {Server,Id} ->
                    gen_server:cast(Server, {remove_trader, Trader, Id});
                _ ->
                    {error, badAcct}
            end;
        false ->
            {error, notexist}
    end.

show(S) ->
    gen_server:call(S, show).

% --------------------------server API----------------
% State is [Num,Accounts, Offers, Traders]
% Num is the number of trades
% Accounts is [{{S,Id},{isk,[{stock,amount}.....]}}....]
% Offers is [{OfferId,AccountId,{stock,isk}}....]
% Traders is [{TraderId,AccountId,Strategy}....]
% Strategy = fun((Offer) -> Decision), i.e. fun({"Erlang Inc", Price}) when Price =< 42 -> accept; (_) -> reject end.
init([]) ->
    {ok, [0,[],[],[]]}.

handle_call({open_account, Holdings,Server}, _From, [Num, Accounts, Offers, Traders]) ->
    {H,M,S} = erlang:now(),
    AccountId = lists:concat(['A',H,M,S]),
    case checkHoldings(Holdings) of 
        true ->
            {reply, {Server,AccountId}, [Num, [{{Server,AccountId},Holdings}|Accounts], Offers, Traders]};
        false ->
            {reply, {error, badHoldings}, [Num, Accounts, Offers, Traders]}
    end;
handle_call(show, _From, [Num, Accounts, Offers, Traders]) ->
    io:format("Num: ~p~n",[Num]),
    io:format("Accounts: ~p~n",[Accounts]),
    io:format("Offers: ~p~n",[Offers]),
    io:format("Traders: ~p~n",[Traders]),
    {reply,{Accounts,Offers,Traders}, [Num, Accounts, Offers, Traders]};
handle_call({make_offer,Offer,Id}, _From, [Num, Accounts, Offers, Traders]) ->
    case Offer of
        {_, Price} ->
            if Price >= 0 ->
                {H,M,S} = erlang:now(),
                OfferId = lists:concat(['O',H,M,S]),
                NewOffers = [{OfferId,Id,Offer}|Offers],

                % Run Traders
                Map_Reses = run_all_traders(NewOffers, Traders),
                {NewAccounts, NewOffers1,NewNum} = run_all_Map_Res(Map_Reses, Accounts, NewOffers,Num),
                {reply, {ok, OfferId}, [NewNum, NewAccounts, NewOffers1, Traders]};
            true ->
                {reply, {error, badPrice}, [Num, Accounts, Offers, Traders]}
            end;
        _ ->
            {reply, {error, badOffer}, [Num, Accounts, Offers, Traders]}
    end;
handle_call({add_trader,Strategy,Id}, _From, [Num, Accounts, Offers, Traders]) ->
    {H,M,S} = erlang:now(),
    TraderId = lists:concat(['T',H,M,S]),
    NewTraders = [{TraderId, Id, Strategy}|Traders],

    % Run Traders
    Map_Reses = run_all_traders(Offers, NewTraders),
    {NewAccounts, NewOffers,NewNum} = run_all_Map_Res(Map_Reses, Accounts, Offers,Num),
    {reply, TraderId, [NewNum, NewAccounts, NewOffers, NewTraders]};
handle_call(shutdown, _From, [Num, Accounts, Offers, Traders]) ->
    {stop, normal, Num, [Num, Accounts, Offers, Traders]};
handle_call({account_balance,Id}, _From, [Num, Accounts, Offers, Traders]) ->
    Balance = getBalance(Id, Accounts),
    {reply, Balance, [Num, Accounts, Offers, Traders]}.

handle_cast({rescind_offer,OfferId,Id}, [Num, Accounts, Offers, Traders]) ->
    NewOffers = removeoffer(OfferId, Id, Offers),
    {noreply,[Num, Accounts, NewOffers, Traders]};
handle_cast({remove_trader,TraderId,Id}, [Num, Accounts, Offers, Traders]) ->
    NewTraders = removetrader(TraderId, Id, Traders),
    {noreply,[Num, Accounts, Offers, NewTraders]}
.

terminate(_Reason, _State) ->
    ok.

% --------------------------helper functions----------------------------
checkHoldings(Holdings) ->
    case Holdings of
        {ISK, STOCKS} ->
            if ISK >= 0 ->
                checkStocks(STOCKS);
            true ->
                false
            end;
        _ ->
            false
    end.

% adasd
checkStocks(STOCKS) ->
    case STOCKS of
        [] ->
            true;
        {_, Amount} ->
            if Amount >= 0 ->
                true;
            true ->
                false
            end;
        [{_,Amount}|STOCKS1] ->
            if Amount >= 0 ->
                checkStocks(STOCKS1);
            true ->
                false
            end
    end.


getBalance(Id, Accounts) ->
    case Accounts of
        [] ->
            {error, empty};
        [{{_,Id},{Balance,Share}}|_] ->
            {Balance,Share};
        [_|T] ->
            getBalance(Id, T)
    end.

removeoffer(OfferId, Id, Offers) ->
    case Offers of
        [] ->
            [];
        [{OfferId, Id, A}|_] ->
            Offers -- [{OfferId, Id, A}];
        [_|T] ->
            removeoffer(OfferId, Id, T)
    end.

removetrader(TraderId, Id, Traders) ->
    case Traders of
        [] ->
            [];
        [{TraderId, Id, A}|_] ->
            Traders -- [{TraderId, Id, A}];
        [_|T] ->
            removetrader(TraderId, Id, T)
    end.


check_Account_exist(Id) ->
    Balance = account_balance(Id),
    case Balance of
        {error, empty} ->
            false;
        _ ->
            true
    end.

% -----------------------------Trader helper functions--------------------------------
% condition1
check_offer_exists(Offers) ->
    case Offers of
        [] ->
            false;
        _ ->
            true
    end.

% Use Strategy handle one offer
run_strategy(Strategy, Offer) ->
    Me = self(),
    spawn(fun() ->
        case Strategy(Offer) of
            accept ->
                Me ! accept;
            reject ->
                Me ! reject;
            _ ->
                Me ! reject
        end
    end),
    receive
        accept ->
            accept;
        reject ->
            reject
    end.

% One trider run all offer
map_offer(Offers, Trader) ->
    {_, Buyer, Strategy} = Trader,
    case check_offer_exists(Offers) of
        true ->
            lists:map(fun(Offer) ->
                {_,_,Temp} = Offer,
                Res = run_strategy(Strategy,Temp),
                {Res,Buyer,Offer}
            end, Offers);
        false ->
            []
    end.

% get all trader result
run_all_traders(Offers, Traders) ->
    lists:map(fun(Trader) ->
        map_offer(Offers, Trader)
    end, Traders).


% get new Offers
remove_offer(Map_Res,Offers) ->
    case Map_Res of
        [] ->
            Offers;
        [Res|T] ->
            case Res of
                {accept,_,{OfferId,Id,_}} ->
                    removeoffer(OfferId,Id,Offers);
                _ ->
                    remove_offer(T,Offers)
            end
    end.

% get new Map_Res and Accounts,return is {Map_Res,Accounts}
transfer_stock(Map_Res, Accounts) ->
    transfer_stock(Map_Res, Accounts, []).
transfer_stock([], Accounts, NewMap_Res) ->
    {NewMap_Res, Accounts};
transfer_stock(Map_Res, Accounts, NewMap_Res) ->
    case Map_Res of
        [{accept,Buyer,{OfferId,Seller,{Stock,Price}}}|T] ->
            Acc1 = change_ISK(add, Seller, Price, Accounts),
            case change_ISK(remove, Buyer, Price, Acc1) of
                {error,not_enough_ISK} ->
                    NewRes = {reject,Buyer,{OfferId,Seller,{Stock,Price}}},
                    transfer_stock(T, Accounts, [NewRes|NewMap_Res]);
                Acc2 ->
                    Acc3 = change_stock(add, Buyer, Stock, Acc2),

                    case change_stock(remove, Seller, Stock, Acc3) of
                        {error, aaa} ->
                            NewRes = {reject,Buyer,{OfferId,Seller,{Stock,Price}}},
                            transfer_stock(T, Accounts,[NewRes|NewMap_Res]);
                        Acc4 ->
                            NewRes = {accept,Buyer,{OfferId,Seller,{Stock,Price}}},
                            transfer_stock(T, Acc4, [NewRes|NewMap_Res])
                    end
            end;
        [{reject,B,O}|T] ->
            NewRes = {reject,B,O},
            transfer_stock(T, Accounts, [NewRes|NewMap_Res])
    end.



change_ISK(Label, Id, Price, Accounts) ->
    case Accounts of
        [] ->
            [];
        [{{S,Id}, {ISK, Holdings}}|T] ->
            case Label of
                add ->
                    [{{S,Id}, {ISK+Price, Holdings}}|T];
                remove ->
                    case ISK-Price < 0 of
                        true ->
                            {error, not_enough_ISK};
                        false ->
                            [{{S,Id}, {ISK-Price, Holdings}}|T]
                    end
            end;
        [H|T] ->
            [H|change_ISK(Label, Id, Price, T)]
    end.

change_stock(_, _, _, []) ->
    [];
% qwqw
change_stock(_,_,_,{error,_})->
    [];
change_stock(Label, Id, Stock, [{{S,Id}, {ISK, Holdings}}|T]) ->
    case Label of
        add ->
            NewHoldings = add_stock(Stock, Holdings),
            [{{S,Id}, {ISK, NewHoldings}}|T];
        remove ->
            case remove_stock(Stock, Holdings) of
                {error, not_enough_stock} ->
                    {error, aaa};
                NewHoldings ->
                    [{{S,Id}, {ISK,NewHoldings}}|T]
            end
    end;
change_stock(Label, Id, Stock, [H|T]) ->
    case change_stock(Label, Id, Stock, T) of
        {error, aaa} ->
            {error, aaa};
        NewT ->
            [H|NewT]
    end.

remove_stock(Stock, Holdings) ->
    case Holdings of
        [] ->
            {error, not_enough_stock};
        [H|T] ->
            case H of
                {Stock, Amount} ->
                    case Amount of
                        1 ->
                            T;
                        _ ->
                            [{Stock, Amount-1}|T]
                    end;
                _ ->
                    [H|remove_stock(Stock, T)]
            end;
        _ ->
            {error, not_enough_stock}
    end.

add_stock(Stock, Holdings) ->
    case Holdings of
        [] ->
            [{Stock,1}];
        {Stock, Amount} ->
            {Stock, Amount+1};
        {_, _} ->
            [{Stock,1}|Holdings];
        [H|T] ->
            case H of
                {Stock, Amount} ->
                    [{Stock, Amount+1}|T];
                _ ->
                    [H|add_stock(Stock, T)]
            end
    end.

% get final Accounts and Offers
% Map_Reses = [[{accept,AccountId,Offer}...],...]
run_all_Map_Res(Map_Reses,Accounts,Offers,Num) ->
    case Map_Reses of
        [] ->
            {Accounts,Offers,Num};
        [Map_Res|T] ->
            OldNum = length(Offers),
            {NewMap_Res,NewAccounts}= transfer_stock(Map_Res,Accounts),
            NewOffers = remove_offer([NewMap_Res],Offers),
            NewNum = length(NewOffers),
            Num2 = Num + OldNum - NewNum,
            run_all_Map_Res(T,NewAccounts,NewOffers,Num2)
    end.
