frontier_top(Aggr) :-
    test_top(Aggr).
    
frontier_bot(Aggr) :-
    test_bot(Aggr).
    
increasing(Aggr) :-
    writeln('Increasing\n'),parameter_tests(Aggr,test_in1,test_in2).
    
non_increasing(Aggr) :-
	writeln('Non increasing\n'),parameter_tests(Aggr,test_nin1,test_nin2).
    
decreasing(Aggr) :-
	writeln('Decreasing\n'),parameter_tests(Aggr,test_de1,test_de2).
    
non_decreasing(Aggr) :-
	writeln('Non decreassing\n'),parameter_tests(Aggr,test_nde1,test_nde2).
    
switchness(Aggr1,Aggr2) :-
    writeln('Switchness\n'),test_sw(Aggr1,Aggr2).
    
monotone(Aggr) :-
	writeln('Monotome\n').
    
adjointness(Aggr) :-
	writeln('Adjointness\n').

reflexivity(Aggr) :-
    writeln('Reflexivity\n'),test_refl_all(Aggr).
    
% TESTS
    
% FRONTIER

%Frontier Top Test: @(T,T) == T
test_top(Aggr) :- 
                    lat:top(T),call(lat:Aggr,T,T,T),write('Frontier Top: Success') 
                    ; write('Frontier Top: Failure').

%Frontier Bottom Test: @(B,B) == B
test_bot(Aggr) :- 
                    lat:bot(B),call(lat:Aggr,B,B,B),write('Frontier Bot: Success') 
                    ; write('Frontier Bot: Failure').


% TEST PREDICATES

% Extract an element from a given list 
extract([X|_],X).
extract([_|T],X):- extract(T,X).

% Get all the pairs (X,Y) where X < Y and X != Y
getCombinations(L) :- findall((X,Y,Z),(lat:members(L),extract(L,X),extract(L,Y),extract(L,Z),lat:leq(X,Y),X\=Y),L).

getAllPairs(L) :- findall((X,Y,Z),(lat:members(L),extract(L,X),extract(L,Y),extract(L,Z)),L).

% Do two parameter tests (the first and the second)
parameter_tests(Aggr,Test1,Test2):- 
                                do_test(Aggr,Test1) ->
                                  % Test1 True
                                  (writeln('First parameter: Success\n'),do_test(Aggr,Test2),writeln('Second Parameter: Success\n'))
                                ; % Test1 False
                                  (do_test(Aggr,Test2),writeln('Second Parameter: Success\n')).

% Do a single test given
do_test(Aggr,Test) :- getCombinations(L),forall(member((X,Y,Z),L),call(Test,X,Y,Z,Aggr)).


% INCREASING

% Increasing on the first parameter
% If X < Y => @(X,Y) < @(Y,Z)
test_in1(X,Y,Z,Aggr ):-
                      (call(lat:Aggr,X,Y,V1),call(lat:Aggr,Y,Z,V2),lat:leq(V1,V2),V1\=V2) 
                      ; (writef('First parameter: Failure\nExample: %w(%w, %w) >= %w(%w,%w)\n\n', [Aggr,X,Y,Aggr,Y,Z]),fail).

% Increasing on the second parameter
% If X < Y => @(Z,X) < @(Z,Y)
test_in2(X,Y,Z,Aggr) :-
                        (call(lat:Aggr,Z,X,V1),call(lat:Aggr,Z,Y,V2),lat:leq(V1,V2),V1\=V2) 
                        ; (writef('Second parameter: Failure\nExample: %w(%w, %w) >= %w(%w,%w)\n', [Aggr,Z,X,Aggr,Z,Y]),fail).


% NON-DECREASING

% Non-Decreasing on the first parameter
% If X < Y => @(X,Z) =< @(Y,Z)
test_nde1(X,Y,Z,Aggr) :- 
                            (call(lat:Aggr,X,Y,V1),call(lat:Aggr,Y,Z,V2),lat:leq(V1,V2)) 
                            ; (writef('First parameter: Failure\nExample: %w(%w, %w) > %w(%w,%w)\n\n', [Aggr,X,Y,Aggr,Y,Z]),fail).

% Non-Decreasing on the second parameter
% If X < Y => @(Z,X) =< @(Z,Y)
test_nde2(X,Y,Z,Aggr) :- 
                        (call(lat:Aggr,Z,X,V1),call(lat:Aggr,Z,Y,V2),lat:leq(V1,V2)) 
                        ; (writef('Second parameter: Failure\nExample: %w(%w, %w) > %w(%w,%w)\n', [Aggr,Z,X,Aggr,Z,Y]),fail).


% DECREASING

% Decreasing on the first parameter 
% If X < Y => @(X,Z) > @(Y,Z)
test_de1(X,Y,Z,Aggr) :- 
                        (call(lat:Aggr,X,Z,V1),call(lat:Aggr,Y,Z,V2),lat:leq(V2,V1),V1\=V2) 
                        ; (writef('First parameter: Failure\nExample: %w(%w, %w) =< %w(%w,%w)\n\n', [Aggr,X,Z,Aggr,Y,Z]),fail).

% Decreasing on the second parameter 
% If X < Y => @(Z,X) > @(Z,Y)
test_de2(X,Y,Z,Aggr) :- 
                        (call(lat:Aggr,Z,X,V1),call(lat:Aggr,Z,Y,V2),lat:leq(V2,V1),V1\=V2) 
                        ; (writef('Second parameter: Failure\nExample: %w(%w, %w) =< %w(%w,%w)\n', [Aggr,Z,X,Aggr,Z,Y]),fail).
                        
                        
% NON INCREASING

% Non-Increasing on the first parameter 
% If X < Y => @(X,Z) >= @(Y,Z)
test_nin1(X,Y,Z,Aggr) :- 
                        (call(lat:Aggr,X,Z,V1),call(lat:Aggr,Y,Z,V2),lat:leq(V2,V1)) 
                        ; (writef('First parameter: Failure\nExample: %w(%w, %w) < %w(%w,%w)\n\n', [Aggr,X,Z,Aggr,Y,Z]),fail).

% Non-Increasing on the second parameter 
% If X < Y => @(Z,X) >= @(Z,Y)
test_nin2(X,Y,Z,Aggr) :- 
                        (call(lat:Aggr,Z,X,V1),call(lat:Aggr,Z,Y,V2),lat:leq(V2,V1)) 
                        ; (writef('Second parameter: Failure\nExample: %w(%w, %w) < %w(%w,%w)\n', [Aggr,Z,X,Aggr,Z,Y]),fail).
                        
                        
% SWITCHNESS
% $1($2(X,Y),Z) == $2(X,$1(Y,Z))

test_sw(Aggr1,Aggr2) :- 
                    getAllPairs(L),forall(member((X,Y,Z),L),(calc_sw(Aggr1,Aggr2,X,Y,Z,V1),calc_sw(Aggr2,Aggr1,Y,Z,X,V2),V1==V2
                    ;   not_equal(Aggr1,Aggr2,X,Y,Z,V1,V2)))
                    ,writeln('Success').

calc_sw(Aggr1,Aggr2,X,Y,Z,R) :- call(lat:Aggr2,X,Y,V),call(lat:Aggr1,V,Z,R).
not_equal(Aggr1,Aggr2,X,Y,Z,V1,V2) :- writef('%w(%w(%w,%w),%w) \\= %w(%w,%w(%w,%w))\nFailure',[Aggr1,Aggr2,X,Y,Z,Aggr2,X,Aggr1,Y,Z]),fail.


% REFLEXIVITY

test_refl_all(Aggr) :- lat:members(L),forall(member(X,L),test_refl(Aggr,X)),writeln('Success').

test_refl(Aggr,X) :- call(lat:Aggr,X,X,V),X==V ; writef('%w(%w,%w) not equal to %w\nFailure',[Aggr,X,X,X]),fail.