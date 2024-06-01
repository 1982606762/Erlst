-module(test_erlst).

-export([test_all/0, test_everything/0]).
% -import(test_erlst_unit, [test_all/0, test_everything/0]).
% Remember to export the other functions from Q2.2
-export([prop_value_preservation/0, mkstrategy/1, reliable_strategy/0,prop_total_trades/0]).

% -compile(export_all).

-include_lib("eqc/include/eqc.hrl").

test_all() ->
    eqc:quickcheck(prop_value_preservation()),
    eqc:quickcheck(prop_total_trades()),
    test_erlst_unit:test_everything().

test_everything() ->
    test_all().

mkstrategy(Opr) ->
    case Opr of
        buy_everything ->
            fun({_, _}) -> accept end;
        {buy_cheap, Num} ->
            fun({_, Price}) ->
                if
                    Price =< Num -> accept;
                    true -> reject
                end
            end;
        {buy_only, Stocks} ->
            fun({Stock, _}) ->
                case lists:member(Stock, Stocks) of
                    true -> accept;
                    false -> reject
                end
            end;
        {both, S1, S2} ->
            fun(Offer) ->
                Str1 = mkstrategy(S1),
                Str2 = mkstrategy(S2),
                case {Str1(Offer), Str2(Offer)} of
                    {accept, accept} -> accept;
                    {reject, reject} -> reject;
                    _ -> reject
                end
            end
    end.

stock_name() -> eqc_gen:oneof([a,b,c,d,e,f]).
stock_names() -> eqc_gen:list(stock_name()).

stock_list() -> 
    ?SUCHTHAT({_,Price},
        {stock_name(), eqc_gen:int()},
        Price > 0).

holdings() ->
    ?SUCHTHAT({Money, _},
              {eqc_gen:int(), stock_list()},
              Money > 0).

offer() ->
    ?SUCHTHAT({_,Price},
          {stock_name(), eqc_gen:int()},
          Price >= 0).

reliable_strategy() ->
    oneof([
            {call, test_erlst, mkstrategy, [buy_everything]},
            {call, test_erlst, mkstrategy, [{buy_cheap, int()}]},
            {call, test_erlst, mkstrategy, [{buy_only, stock_names()}]},
            {call, test_erlst, mkstrategy, [{both,{buy_cheap,int()}, {buy_only, stock_names()}}]}
    ]).


calc_Total_Money(Accts) ->
    case Accts of
        [] -> 0;
        [{Balance,_}|T] -> case Balance+0 of
            0 -> calc_Total_Money(T);
            _ -> Balance+calc_Total_Money(T)
        end
    end.

calc_Total_Stock(Accts) ->
    case Accts of
        [] -> 0;
        {_,Holdings} -> Holdings;
        [{_,Holdings}|T] -> case Holdings of
            [] -> calc_Total_Stock(T);
            [{_,Num}|T2] -> Num+calc_Total_Stock(T);
            {_,Num} -> Num
        end
    end.

prop_value_preservation() ->
    ?FORALL(
        {Strategy, Holdings1,Holdings2,Offer},
        {reliable_strategy(), holdings(),holdings(), offer()},
        begin
            {ok,S} = erlst:launch(),
            A1 = erlst:open_account(S, Holdings1),
            A2 = erlst:open_account(S, Holdings2),
            erlst:make_offer(A1, Offer),
            erlst:make_offer(A1, Offer),
            erlst:make_offer(A1, Offer),
            erlst:make_offer(A1, Offer),
            erlst:make_offer(A2, Offer),
            erlst:make_offer(A2, Offer),
            erlst:make_offer(A2, Offer),
            erlst:make_offer(A2, Offer),
            A1Holding = erlst:account_balance(A1),
            A2Holding = erlst:account_balance(A2),
            Money1 = calc_Total_Money([A1Holding, A2Holding]),
            Stock1 = calc_Total_Stock([A1Holding, A2Holding]),
            erlst:add_trader(A1, eval(Strategy)),
            erlst:add_trader(A1, eval(Strategy)),
            erlst:add_trader(A1, eval(Strategy)),
            erlst:add_trader(A1, eval(Strategy)),
            erlst:add_trader(A2, eval(Strategy)),
            erlst:add_trader(A2, eval(Strategy)),
            erlst:add_trader(A2, eval(Strategy)),
            erlst:add_trader(A2, eval(Strategy)),
            A1Holding1 = erlst:account_balance(A1),
            A2Holding1 = erlst:account_balance(A2),
            Money2 = calc_Total_Money([A1Holding1, A2Holding1]),
            Stock2 = calc_Total_Stock([A1Holding1, A2Holding1]),
            eqc:equals(Money1, Money2),
            eqc:equals(Stock1, Stock2)
        end
    ).

prop_total_trades() ->
    ?FORALL(
        {Strategy, Holdings1,Holdings2, Offer},
        {mkstrategy(buy_everything), holdings(), holdings(), offer()},
        begin
            {ok,Pid} = erlst:launch(),
            A1 = erlst:open_account(Pid, Holdings1),
            A2 = erlst:open_account(Pid, Holdings2),
            erlst:make_offer(A1, Offer),
            erlst:make_offer(A1, Offer),
            erlst:make_offer(A1, Offer),
            erlst:make_offer(A1, Offer),
            erlst:make_offer(A2, Offer),
            erlst:make_offer(A2, Offer),
            erlst:make_offer(A2, Offer),
            erlst:make_offer(A2, Offer),
            erlst:add_trader(A2, Strategy),
            erlst:add_trader(A2, Strategy),
            erlst:add_trader(A2, Strategy),
            erlst:add_trader(A2, Strategy),
            erlst:add_trader(A1, Strategy),
            erlst:add_trader(A1, Strategy),
            erlst:add_trader(A1, Strategy),
            erlst:add_trader(A1, Strategy),
            Num = erlst:shutdown(Pid),
            Num =< 8
        end
    ).
