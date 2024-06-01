-module(test_erlst_unit).

-import(erlst, [launch/0, open_account/2, make_offer/2, add_trader/2, show/1, run_all_traders/2]).
-export([test_all/0, test_everything/0]).
-export([]). % Remember to export the other functions from Q2.2

-include_lib("eunit/include/eunit.hrl").
% You are allowed to split your testing code in as many files as you
% think is appropriate, just remember that they should all start with
% 'test_'.
% But you MUST have a module (this file) called test_erlst.


test_all() ->
    [
        {"EUnit", spawn, [
            test_start_server(),
            test_shutdown(),
            test_open_account(),
            test_account_balance(),
            test_make_offer(),
            test_rescind_offer(),
            test_add_trader()
        ]}
    ].

test_everything() ->
    eunit:test(test_all(), [verbose]).

test_start_server() ->
  {"Start server",
    fun() ->
      ?assertMatch({ok, _}, erlst:launch())
    end}.

test_shutdown() ->
  {"Shutdown server",
    fun() ->
      {ok,S} = erlst:launch(),
      ?assertMatch(0, erlst:shutdown(S))
    end}.

test_open_account() ->
    [
        {"Open account acc1 and check balance",
        fun() ->
          {ok, S} = erlst:launch(),
          Acc1 = erlst:open_account(S, {2000, [{"a", 10}]}),
          B = erlst:account_balance(Acc1),
          ?assertMatch({2000,[{"a",10}]} , B)
        end},
        {"Open acc with Holdings and check balance", fun() ->
        {ok, S} = erlst:launch(),
        Holdings = {100, [{"a", 10}, {"b", 20}]},
        Acc1 = erlst:open_account(S, Holdings),
        B = erlst:account_balance(Acc1),
        ?assertMatch(Holdings , B)
        end},
      {"Open multipal accs", fun() ->
        {ok, S} = erlst:launch(),
        Holdings = {100, [{"a", 10}, {"b", 20}]},
        Acc1 = erlst:open_account(S, Holdings),
        Acc2 = erlst:open_account(S, Holdings),
        Acc3 = erlst:open_account(S, Holdings),
        B1 = erlst:account_balance(Acc1),
        B2 = erlst:account_balance(Acc2),
        B3 = erlst:account_balance(Acc3),
        erlst:show(S),
        ?assertMatch(Holdings , B1),
        ?assertMatch(Holdings , B2),
        ?assertMatch(Holdings , B3)
        end},
      {"Check negative ISK", fun() ->
          {ok, S} = erlst:launch(),
          Holdings = {-100, [{"a", 10}, {"b", 20}]},
          ?assertMatch({error, badHoldings}, erlst:open_account(S, Holdings))
      end},
      {"Check negative num", fun() ->
          {ok, S} = erlst:launch(),
          Holdings = {100, [{"a", -10}, {"b", 20}]},
          ?assertMatch({error, badHoldings}, erlst:open_account(S, Holdings))
      end}
    ].
  
test_account_balance() ->
    {"Check balance of absent user", fun() ->
      erlst:launch(),
      B = erlst:account_balance({{1,2}}),
      ?assertMatch({error,badAcct} , B)
    end}.

test_make_offer() ->
  [{"Check make offer",
    fun() ->
      {ok, S} = erlst:launch(),
      Acc2 = erlst:open_account(S, {2000, []}),
      ?assertMatch({ok,_}, erlst:make_offer(Acc2, {"a", 50}))
    end
    },
    { "Check make offer with negative num",
      fun() ->
        {ok, S} = erlst:launch(),
        Acc2 = erlst:open_account(S, {2000, []}),
        ?assertMatch({error, badPrice}, erlst:make_offer(Acc2, {"a", -50}))
      end
    },
    { "Check make offer with absent user",
      fun() ->
        ?assertMatch({error, badAcct}, erlst:make_offer({{1,2}}, {"a", 50}))
      end
    },
    {"Check make offer with not own stock",
      fun() ->
        {ok, S} = erlst:launch(),
        Acc2 = erlst:open_account(S, {2000, [{"b", 50}]}),
        ?assertMatch({ok, _}, erlst:make_offer(Acc2, {"a", 50}))
      end
    },
    {"Check make offer and reciend it",
      fun() ->
        {ok, S} = erlst:launch(),
        Acc2 = erlst:open_account(S, {2000, [{"a", 50}]}),
        Offer = erlst:make_offer(Acc2, {"a", 50}),
        ?assertMatch({ok, _}, Offer),
        ?assertMatch(ok, erlst:rescind_offer(Acc2, Offer))
      end
    },
    {"Check make offer and add trader and run it",
      fun() ->
        {ok, S} = erlst:launch(),
        Acc1 = erlst:open_account(S, {2000, [{"a", 50}]}),
        Acc2 = erlst:open_account(S, {2000, [{"a", 50}]}),
        Strategy = fun({"a", Price}) ->
            if
                Price < 100 -> accept;
                true -> reject
            end
        end,
        erlst:add_trader(Acc1, Strategy),
        Offer = erlst:make_offer(Acc2, {"a", 50}),
        ?assertMatch({ok, _}, Offer),
        timer:sleep(1000),
        ?assertMatch({1950, [{"a", 51}]}, erlst:account_balance(Acc1)),
        ?assertMatch({2050, [{"a", 49}]}, erlst:account_balance(Acc2))
      end
    }].

test_add_trader() ->
  {"Add trader and check trader",
    fun() ->
      {ok, S} = erlst:launch(),
      Acc3 = erlst:open_account(S, {2000, [{"a", 2}]}),
      Acc4 = erlst:open_account(S, {1000, []}),
      Strategy = fun({"a", Price}) ->
            if
                Price < 100 -> accept;
                true -> reject
            end
        end,
      erlst:make_offer(Acc3, {"a", 50}),
      erlst:add_trader(Acc4, Strategy),
      timer:sleep(1000),
      ?assertMatch({2050,[{"a",1}]},erlst:account_balance(Acc3)),
      ?assertMatch({950,[{"a",1}]} ,erlst:account_balance(Acc4))
    end
    }.

test_rescind_offer() ->
  {"Rescind offer",
    fun() ->
      {ok, S} = erlst:launch(),
      Acc2 = erlst:open_account(S, {2000, [{"a", 50}]}),
      Offer = erlst:make_offer(Acc2, {"a", 50}),
      ?assertMatch({ok, _}, Offer),
      ?assertMatch(ok, erlst:rescind_offer(Acc2, Offer))
    end
    }.