%% @author Karl Marklund <karl.marklund@it.uu.se>

%% @doc A small collection of utility functions. 


-module(utils). 

-export([seqs/1, filter/2, split/2, split/3, digit_to_ascii/1, intersperse/2, replace/3, repeat/2, pad_list/4, make_equal_length/3]).

%% To use EUnit we must include this.
-include_lib("eunit/include/eunit.hrl").

-compile(export_all). 




%% @doc Generates a list of lists of increasing sequences of integers
%% starting with the empty list and ending with [1,2, ..., N].
%% === Example ===
%% <div class="example">```
%% > utils:seqs(5).
%% [[],[1],[1,2],[1,2,3],[1,2,3,4],[1,2,3,4,5]]'''
%% </div>
-spec seqs(N::integer()) -> [[integer()]].

seqs(N) ->
    %% NOTE: Simply using a list comprehension such as [[]] ++
    %% [lists:seq(1,M) || M <- lists:seq(1,N)] will be quite slow
    %% since each sequence is generated from scratch. Hence, lets
    %% re-use the last sequnece and add a new element when
    %% constructing the next sequence.
    
    F = fun(X,[H|T]) -> [[X|H],H|T] end,
    lists:foldl(F, [[]], lists:seq(1,N)),
    lists:reverse([lists:reverse(L) || L <- lists:foldl(F, [[]], lists:seq(1,N))]).

		
%% @doc Each list in List2 contains the elements Elem in List1 for
%% which one of the Pred(Elem) returns true. The order of the lists in
%% List2 is the same as the order of the predicates. In each list in
%% List2, the relative order of the elements are the same as in the
%% original List1. 
%% 
%% === Example ===
%% <div class="example">```
%% 1> L = [1,2,3,4,5,6,7,8,9,10].
%% [1,2,3,4,5,6,7,8,9,10]
%% 2> P1 = fun(X) -> X rem 2 == 1 end.
%% #Fun<erl_eval.6.111823515>  
%% 3> P2 = fun(X) -> not P1(X) end. 
%% #Fun<erl_eval.6.111823515>
%% 4> P3 = fun(X) -> X > 3 andalso X < 7 end. 
%% #Fun<erl_eval.6.111823515>
%% 5> utils:filter([P1,P2,P3], L).
%% [[1,3,5,7,9],[2,4,6,8,10],[4,5,6]]'''
%% </div>
-spec filter(Preds, List1) -> List2 when
      Preds :: [Pred],
      Pred :: fun((Elem :: T) -> boolean()),
      List1 :: [T],
      List2 :: [[T]],
      T :: term().

filter(Predicates, List) ->
    Collect = self(),
    [spawn(fun() -> Collect!{I,lists:filter(P,List)} end) ||
	{I, P} <- lists:zip(lists:seq(1, length(Predicates)), Predicates)],
    
    filter_collect(length(Predicates), []).

filter_collect(0,R) ->
    [L || {_,L} <- lists:sort(R)];
filter_collect(N,R) ->
    receive
	{I, L} -> filter_collect(N-1, [{I,L}|R])
    end.

lqr(L, N) ->
    Len = length(L),

    %% Quotient
    Q = Len div N, 
    
    %% Reminder
    R = Len rem N, 
    
    {Len, Q, R}. 

%% @doc Split List into N Lists such that all Lists have approximately the same number of elements. 
%% 
%% Let Len = length(List), Q = Len div N and R = Len rem N. 
%% 
%% If R = 0, then all of the lists in Lists will be of length Q. 
%% 
%% If R =/= 0, then R of the lists in Lists will have
%% lenght Q + 1. 
%% 
%% === Example ===
%% 
%% <div class="example">```
%% 1> L = [1,2,3,4,5,6,7,8,9,10].
%% [1,2,3,4,5,6,7,8,9,10]
%% 2> utils:split(L, 4).
%% [[1,2],[3,4],[5,6,7],[8,9,10]]
%% 3> lists:concat(utils:split(L,3)).
%% [1,2,3,4,5,6,7,8,9,10]'''
%% </div>
-spec split(List, N) -> Lists when
      List :: [T],
      Lists :: [List],
      T :: term(),
      N :: integer().


split(L, N) ->
    Len = length(L),
    Q = Len div N,
    R = Len rem N,
    split(L, Q, R).

split([],_,_) ->
    [];
split(L,Q,0) ->
    {First, Rest} = lists:split(Q, L),
    [First | split(Rest,Q,0)];    
split(L,Q,R) ->
    {First, Rest} = lists:split(Q+1, L),
    [First | split(Rest,Q,R-1)].


%% @doc Converts a digit to it's ascii representation.
%%
%% === Example ===
%%
%% <div class="example">```
%% 1> digit_to_ascii(0).
%% $0
%% 2> digit_to_ascii(9).
%% $9'''
%% </div>
-spec digit_to_ascii(Digit) -> Ascii when
      Digit :: integer(),
      Ascii :: integer().

digit_to_ascii(X) when (X >= 0) and (X =< 10) ->
    $0 + X.

%% @doc Intersperses a list with a given element
%%
%% === Example ===
%%
%% <div class="example">```
%% 1> L = ["l", "l", "l"]
%% 2> intersperse("o", L).
%% ["l", "o", "l", "o", "l"]
%% 3> intersperse("a", ["h"].
%% ["h"]
%% 4> intersperse("a", []).
%% []'''
%% </div>
-spec intersperse(Elem, List) -> New_list when
      Elem :: _,
      List :: [_],
      New_list :: [_].
intersperse(_, []) ->
    [];
intersperse(_, [H]) ->
    [H];
intersperse(X, [H|T]) ->
    [H | [X | intersperse(X, T)]].


%% @doc Replaces every occurrence of an element X in a list with a given element Y.
%%
%% === Example ===
%%
%% <div class="example">```
%% 1> L = ["h", "e", "h", "e"]
%% 2> replace("e", "a", L).
%% ["h", "a", "h", "a"]'''
%% </div>
-spec replace(Elem, Elem2, List) -> New_list when
      Elem :: _,
      Elem2 :: _,
      List :: [_],
      New_list :: [_].

replace(_, _, []) ->
    [];
replace(X, Y, [X|T]) ->
    [Y | replace(X, Y, T)];
replace(X, Y, [H|T]) ->
    [H | replace(X, Y, T)].

%% @doc Creates a list containing N repetitions of a given element.
%% 
%% === Example ===
%% <div class="example">```
%% 1> repeat("A", 3).
%% ["A","A","A"]
%% 2> repeat("A", 0).
%% []
%% 3> repeat("B", 1).
%% ["B"]'''
%% </div>
repeat(Elem, N) ->  
    [Elem || _ <- lists:seq(1,N)].

%% @doc Pads a list to the right or left N times with a given element 
%% 
%% === Example ===
%% <div class="example">```
%% 1> L = [1, 1, 1].
%% 2> padList(left, 0, 2, L).
%% [0, 0, 1, 1, 1]
%% 3> padList(right, 0, 3, L).
%% [1, 1, 1, 0, ,0 ,0]'''
%% </div>
-spec pad_list(Side, Elem, N, List) -> New_list when
      Side :: atom(),
      Elem :: _,
      N :: integer(),
      List :: list(),
      New_list :: list().

pad_list(Side, X, N, List) ->
    case Side of
	left -> 
	    repeat(X, N) ++ List;
	right ->
	    List ++ repeat(X, N)
    end.

%% @docs Makes two lists the same length by prepending an element to the shortest list sufficiently many times.
%% === Example ===
%% <div class="example">```
%% 1> A = [1, 1, 1].
%% 2> B = [1, 1, 1, 1, 1].
%% 3> make_equal_length(A, B, 0).
%% {[0, 0, 1, 1, 1], [1, 1, 1, 1, 1]}
-spec make_equal_length(A, B, Elem) -> {New_A, New_B} when
      A :: list(),
      B :: list(),
      Elem :: _,
      New_A :: list(),
      New_B :: list().

make_equal_length(A, B, Elem) ->

    Longest = max(length(A),length(B)),
    New_A = utils:pad_list(left, Elem, Longest - length(A), A),
    New_B = utils:pad_list(left, Elem, Longest - length(B), B),
    {New_A, New_B}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                          %%
%%			   Eunit Test Cases                                 %%
%%                                                                          %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
digit_to_ascii_test() ->
    L = [$0, $1, $2, $3, $4, $5, $6, $7, $8, $9],
    ?assertEqual(L, [digit_to_ascii(N) || N <-lists:seq(0, 9)]).

intersperse_test() ->
    L = ["l", "l", "l"],
    ? assertEqual(intersperse("o", L), ["l", "o", "l", "o", "l"]).
intersperse_single_test() ->
    ?assertEqual(intersperse("a", ["h"]), ["h"]).
intersperse_empty_test() ->
    ?assertEqual(intersperse("a", []), []).

repeat_test() ->
    Test1 = ?assertEqual(["A","A","A"], repeat("A",3)),
    Test2 = ?assertEqual(["B"], repeat("B", 1)),
    [Test1, Test2].

repeat_empty_test() ->
    ?assertEqual([], repeat("A", 0)).

pad_list_right_test() ->
    ?assertEqual(pad_list(right, $5, 2, "234"), "23455").

pad_list_left_test() ->
    ?assertEqual("1234", pad_list(left, $1, 1, "234")).

make_equal_length_test() ->
    Test1 = ?assertEqual({"00", "11"}, make_equal_length("", "11", $0)),
    Test2 = ?assertEqual({"11", "00"}, make_equal_length("11", "", $0)),
    [Test1, Test2].
make_equal_length_same_test() ->
    ?assertEqual({"00", "11"}, make_equal_length("00", "11", $0)).

seqs_length_test_() ->
    %% The list [[], [1], [1,2], ..., [1,2, ..., N]] will allways have
    %% length N+1.

    [?_assertEqual(N+1, length(seqs(N))) || N <- lists:seq(1, 55)].

seqs_test_() ->
    %% A small collection of expected results {N, seqs(N)}.
    
    Data = [{0, [[]]}, {1, [[], [1]]}, {2, [[], [1], [1,2]]}, 
	    {7, [[],
		 [1],
		 [1,2],
		 [1,2,3],
		 [1,2,3,4],
		 [1,2,3,4,5],
		 [1,2,3,4,5,6],
		 [1,2,3,4,5,6,7]]}
	   ],
    
    [?_assertEqual(L, seqs(N)) || {N, L} <- Data].
    
filter_test_() ->
    [?_assertEqual([], filter([], L)) || L <- seqs(10)].
    
filter_true_false_test_() ->
    P1 = fun(_) -> false end,
    P2 = fun(_) -> true end,
    P3 = fun(X) -> X rem 2 == 0 end,
    
    Expected = fun(L) -> [lists:filter(P,L) || P <- [P1,P2,P3]] end,

    [?_assertEqual(Expected(L), filter([P1,P2,P3], L) ) || L <- seqs(10) ].
				       
filter_test() ->
    L = lists:seq(1,10),

    P1 = fun(X) -> X rem 2 == 0 end,
    P2 = fun(X) -> X rem 2 == 1 end,
    P3 = fun(X) -> X > 3 end,

    %%E = [[2,4,6,8,10],[1,3,5,7,9],[4,5,6,7,8,9,10]],
    E = [lists:filter(P,L) || P <- [P1,P2,P3]],
    
    ?assertEqual(E, filter([P1,P2,P3], L)).
    
split_concat_test_() ->
    %% Make sure the result of concatenating the sublists equals the
    %% original list.
    
    L = lists:seq(1,99),
    [?_assertEqual(L, lists:concat(split(L,N))) || N <- lists:seq(1,133)].

split_n_test_() ->
    %% Make sure the correct number of sublists are generated. 
    
    M = 99,
    L = lists:seq(1,M),
    Num_of_lists = fun(List, N) when N =< length(List) ->
			   N;
		      (List, _) ->
			   length(List)
		   end,
    [?_assertEqual(Num_of_lists(L,N), length(split(L,N))) || N <- L].    


expected_stat(L, N) when N =< length(L) ->
    %% When spliting a list L into N sublists, we know there will only by two possible
    %% lengths of the sublists.

    
    %% Quotient and reminder when dividing length of L with N. 
    {_, Q, R} = lqr(L, N),

    %% There will allways be R sublists of length Q+1 and N-R sublists
    %% of length Q.
    
    {{R, Q+1}, {N-R, Q}};

expected_stat(L, _N) ->
    %% N greater than the length of L, hence all sublists will have
    %% length 1.

    {{length(L), 1}, {0,0}}.

stat(N, M, LL) ->
    %% Return a tuple {{Num_N, N}, {Num_M, M}} where Num_N is the
    %% number of lists of length N in LL and Num_M is the number of
    %% lists of length M in LL.
    
    S = filter([fun(X) -> X == N end, fun(X) -> X == M end], [length(L) || L <- LL]),

    [Num_N, Num_M] = [length(L) || L <- S],
    
    {{Num_N, N}, {Num_M, M}}.

split_stat_test_() ->
    %% Assure the list of sublists contains the correct number of
    %% lists of the two expected lengths.
	
    Assert = fun(L,N) ->
		     {_, Q, _} = lqr(L,N), 
		     ?_assertEqual(expected_stat(L,N), stat(Q+1, Q, split(L,N))) 
	     end,
	
    %% Generators can depend on other generator expressions, here N
    %% depends on the length of L.
    
    [Assert(L,N) ||  L <- seqs(33), N <- lists:seq(1,length(L)+5)].
