frontier_top(Aggr) :-
    write('Frontier Top: '),lat:top(T),test_refl(Aggr,T),write('Success').
    
frontier_bot(Aggr) :-
    write('Frontier Bot: '),lat:bot(B),test_refl(Aggr,B),write('Success').
    
increasing(Aggr) :-
    writeln('Increasing:\n'),growth_test(Aggr,test_in1,test_in2).
    
non_increasing(Aggr) :-
	writeln('Non increasing:\n'),growth_test(Aggr,test_nin1,test_nin2).
    
decreasing(Aggr) :-
	writeln('Decreasing:\n'),growth_test(Aggr,test_de1,test_de2).
    
non_decreasing(Aggr) :-
	writeln('Non decreassing:\n'),growth_test(Aggr,test_nde1,test_nde2).
    
switchness(Aggr1,Aggr2) :-
    write('Switchness: '),test_sw(Aggr1,Aggr2),writeln('Success').
    
associativity(Aggr) :-
    write('Associativity: '),test_sw(Aggr,Aggr),writeln('Success').
    
monotony(Aggr) :-
	writeln('Monotony:\n'),test_mono(Aggr).
    
adjointness(Aggr1,Aggr2) :-
	( test_adj(Aggr1,Aggr2) -> writeln('\nAdjointness: Success') ; writeln('\nAdjointness: Failure')).

reflexivity(Aggr) :-
    write('Reflexivity: '),test_refl_all(Aggr),writeln('Success').

commutativity(Aggr) :-
    write('Commutativity: '),test_com(Aggr),writeln('Success').
    
distributivity(Aggr1,Aggr2) :-
    write('Distributivity:\n\n'),test_distr(Aggr1,Aggr2).
    
t_norm(Aggr) :- 
    ( test_tnorm(Aggr) -> writeln('\nT-NORM: SUCCESS') ; writeln('\nT-NORM: FAILURE\n')).
    
t_conorm(Aggr) :-
    ( test_tconorm(Aggr) -> writeln('\nT-CONORM: SUCCESS') ; writeln('\nT-CONORM: FAILURE\n')).
    
implication(Aggr) :-
     writeln('Implication:\n'),test_imp(Aggr).   
    
    
% TEST PREDICATES

% Extract an element from a given list 
extract([X|_],X).
extract([_|T],X):- extract(T,X).

% Get all the pairs (X,Y) where X < Y and X != Y
getXltY(L) :- findall((X,Y,Z),(lat:members(L),extract(L,X),extract(L,Y),extract(L,Z),lat:leq(X,Y),X\=Y),L).

% Get all the pairs of three elements
getAllTriplet(L) :- findall((X,Y,Z),(lat:members(L),extract(L,X),extract(L,Y),extract(L,Z)),L).

% Test the two growth tests in both parameters using the given connective
growth_test(Aggr,Test1,Test2):- 
                                do_test(Aggr,Test1) ->
                                  % Test1 True
                                  (writeln('First parameter: Success\n'),do_test(Aggr,Test2),writeln('Second Parameter: Success\n'))
                                ; % Test1 False
                                  (do_test(Aggr,Test2),writeln('Second Parameter: Success\n')).

% Do the growth test using the connective given
do_test(Aggr,Test) :- getXltY(L),forall(member((X,Y,Z),L),call(Test,X,Y,Z,Aggr)).


% INCREASING

% Increasing on the first parameter
% If X < Y => $(X,Z) < $(Y,Z)
test_in1(X,Y,Z,Aggr ):-
                      (call(lat:Aggr,X,Z,V1),call(lat:Aggr,Y,Z,V2),lat:leq(V1,V2),V1\=V2) 
                      ; (writef('First parameter: Failure\nCounterxample:\n%w(%w, %w) >= %w(%w,%w)\n\n', [Aggr,X,Y,Aggr,Y,Z]),fail).

% Increasing on the second parameter
% If X < Y => $(Z,X) < $(Z,Y)
test_in2(X,Y,Z,Aggr) :-
                        (call(lat:Aggr,Z,X,V1),call(lat:Aggr,Z,Y,V2),lat:leq(V1,V2),V1\=V2) 
                        ; (writef('Second parameter: Failure\nCounterxample:\n%w(%w, %w) >= %w(%w,%w)\n', [Aggr,Z,X,Aggr,Z,Y]),fail).


% NON-DECREASING

% Non-Decreasing on the first parameter
% If X < Y => $(X,Z) =< $(Y,Z)
test_nde1(X,Y,Z,Aggr) :- 
                            (call(lat:Aggr,X,Z,V1),call(lat:Aggr,Y,Z,V2),lat:leq(V1,V2)) 
                            ; (writef('First parameter: Failure\nCounterxample:\n%w(%w, %w) > %w(%w,%w)\n\n', [Aggr,X,Y,Aggr,Y,Z]),fail).

% Non-Decreasing on the second parameter
% If X < Y => $(Z,X) =< $(Z,Y)
test_nde2(X,Y,Z,Aggr) :- 
                        (call(lat:Aggr,Z,X,V1),call(lat:Aggr,Z,Y,V2),lat:leq(V1,V2)) 
                        ; (writef('Second parameter: Failure\nCounterxample:\n%w(%w, %w) > %w(%w,%w)\n', [Aggr,Z,X,Aggr,Z,Y]),fail).


% DECREASING

% Decreasing on the first parameter 
% If X < Y => $(X,Z) > $(Y,Z)
test_de1(X,Y,Z,Aggr) :- 
                        (call(lat:Aggr,X,Z,V1),call(lat:Aggr,Y,Z,V2),lat:leq(V2,V1),V1\=V2) 
                        ; (writef('First parameter: Failure\nCounterxample:\n%w(%w, %w) =< %w(%w,%w)\n\n', [Aggr,X,Z,Aggr,Y,Z]),fail).

% Decreasing on the second parameter 
% If X < Y => $(Z,X) > $(Z,Y)
test_de2(X,Y,Z,Aggr) :- 
                        (call(lat:Aggr,Z,X,V1),call(lat:Aggr,Z,Y,V2),lat:leq(V2,V1),V1\=V2) 
                        ; (writef('Second parameter: Failure\nCounterxample:\n%w(%w, %w) =< %w(%w,%w)\n', [Aggr,Z,X,Aggr,Z,Y]),fail).
                        
                        
% NON INCREASING

% Non-Increasing on the first parameter 
% If X < Y => $(X,Z) >= $(Y,Z)
test_nin1(X,Y,Z,Aggr) :- 
                        (call(lat:Aggr,X,Z,V1),call(lat:Aggr,Y,Z,V2),lat:leq(V2,V1)) 
                        ; (writef('First parameter: Failure\nCounterxample:\n%w(%w, %w) < %w(%w,%w)\n\n', [Aggr,X,Z,Aggr,Y,Z]),fail).

% Non-Increasing on the second parameter 
% If X < Y => $(Z,X) >= $(Z,Y)
test_nin2(X,Y,Z,Aggr) :- 
                        (call(lat:Aggr,Z,X,V1),call(lat:Aggr,Z,Y,V2),lat:leq(V2,V1)) 
                        ; (writef('Second parameter: Failure\nCounterxample:\n%w(%w, %w) < %w(%w,%w)\n', [Aggr,Z,X,Aggr,Z,Y]),fail).
                        
                        
% SWITCHNESS
% $1($2(X,Y),Z) == $2(X,$1(Y,Z))

test_sw(Aggr1,Aggr2) :- 
                    getAllTriplet(L),
                    forall(member((X,Y,Z),L),
                        (calc_sw1(Aggr1,Aggr2,X,Y,Z,V1),calc_sw2(Aggr1,Aggr2,X,Y,Z,V2),V1==V2
                        ;   fail_sw(Aggr1,Aggr2,X,Y,Z))
                    ).

calc_sw1(Aggr1,Aggr2,X,Y,Z,R) :- call(lat:Aggr2,X,Y,V),call(lat:Aggr1,V,Z,R).
calc_sw2(Aggr1,Aggr2,X,Y,Z,R) :- call(lat:Aggr1,Y,Z,V),call(lat:Aggr2,X,V,R).
fail_sw(Aggr1,Aggr2,X,Y,Z) :- writef('Failure\nCounterxample:\n%w(%w(%w,%w),%w) not equal to %w(%w,%w(%w,%w))\n',[Aggr1,Aggr2,X,Y,Z,Aggr2,X,Aggr1,Y,Z]),fail.


% REFLEXIVITY

test_refl_all(Aggr) :- lat:members(L),forall(member(X,L),test_refl(Aggr,X)).

test_refl(Aggr,X) :- ( call(lat:Aggr,X,X,V),X==V -> true ; writef('Failure\nCounterxample:\n%w(%w,%w) not equal to %w\n',[Aggr,X,X,X]),fail).


% COMMUTATIVITY

test_com(Aggr) :- 
                  findall((X,Y),(lat:members(L),extract(L,X),extract(L,Y)),L),
                  forall(member((X,Y),L),(call(lat:Aggr,X,Y,V1),call(lat:Aggr,Y,X,V2),V1==V2
                  ;  fail_com(Aggr,X,Y))
                  ).
                  
fail_com(Aggr,X,Y) :- writef('Failure\nCounterxample:\n%w(%w,%w) not equal to %w(%w,%w)\n',[Aggr,X,Y,Aggr,Y,X]),fail. 


% DISTRIBUTIVITY

test_distr1(Aggr1,Aggr2,X,Y,Z) :- call(lat:Aggr1,X,Y,V),call(lat:Aggr2,V,Z,V1),call(lat:Aggr2,X,Z,U1),call(lat:Aggr2,Y,Z,U2),
                                  call(lat:Aggr1,U1,U2,V2),V1==V2 
                                  ; fail_distr1(Aggr1,Aggr2,X,Y,Z).
                                  
fail_distr1(Aggr1,Aggr2,X,Y,Z) :- writef('First parameter: Failure\nExample: %w(%w(%w,%w),%w) not equal to %w(%w(%w,%w), %w(%w,%w))\n\n',[Aggr2,Aggr1,X,Y,Z,Aggr1,Aggr2,X,Z,Aggr2,Y,Z]),fail.

test_distr2(Aggr1,Aggr2,X,Y,Z) :- call(lat:Aggr1,Y,Z,V),call(lat:Aggr2,X,V,V1),call(lat:Aggr2,X,Y,U1),call(lat:Aggr2,X,Z,U2),
                                  call(lat:Aggr1,U1,U2,V2),V1==V2 
                                  ; fail_distr2(Aggr1,Aggr2,X,Y,Z).
                                  
fail_distr2(Aggr1,Aggr2,X,Y,Z) :- writef('Second parameter: Failure\nExample: %w(%w(%w,%w),%w) not equal to %w(%w(%w,%w), %w(%w,%w))\n\n',[Aggr2,X,Aggr1,Y,Z,Aggr1,Aggr2,X,Y,Aggr2,X,Z]),fail.

test_distr(Aggr1,Aggr2) :-
                            (
                                getAllTriplet(L),
                                forall(member((X,Y,Z),L),test_distr1(Aggr1,Aggr2,X,Y,Z))
                            ->
                                writeln('First parameter: Success\n'), forall(member((X,Y,Z),L),test_distr2(Aggr1,Aggr2,X,Y,Z)),writeln('Second parameter: Success\n')
                            ;
                                getAllTriplet(L),forall(member((X,Y,Z),L),test_distr2(Aggr1,Aggr2,X,Y,Z)),writeln('Second parameter: Success\n')
                            ).
                            
% ADJOINTNESS

test_adj(Aggr1,Aggr2) :- t_norm(Aggr1),implication(Aggr2),adjoint(Aggr1,Aggr2).

adjoint(Aggr1,Aggr2) :- 
                            getAllTriplet(L),
                            forall(member((X,Y,Z),L),
                                (
                                    call(lat:Aggr2,Y,Z,V1),
                                    call(lat:Aggr1,X,Z,V2),
                                    bicond(Aggr1,Aggr2,X,Y,Z,V1,V2)
                                )
                            ).

bicond(Aggr1,Aggr2,X,Y,Z,V1,V2) :- ( lat:leq(X,V1),lat:leq(V2,Y) 
                                    -> true
                                    ; ( not(lat:leq(X,V1)),not(lat:leq(V2,Y)) 
                                        -> true
                                        ; writef("Failure\nCounterxample:\n%w <= %w(%w, %w) <=\\=> %w(%w, %w) <= %w\n",[X,Aggr2,Y,Z,Aggr1,X,Z,Y]),fail
                                      )
                                    ).


% MONOTOMY
% Increasing in both parameters

test_mono(Aggr) :- do_test(Aggr,test_in1),do_test(Aggr,test_in2).


% T-NORM
% Aggregator is and, is commutative, associative, monotone, and identity element is top

test_tnorm(Aggr) :- lat:top(T),identity_element(Aggr,T,tnorm),print_connective(Aggr,tnorm),commutativity(Aggr),associativity(Aggr),monotony(Aggr).

identity_element(Aggr,E,Type) :- writef('Identity element %w: ',[E]),forall(lat:member(X),test_iden(Aggr,X,E,Type)),writeln('Success').
test_iden(Aggr,X,E,Type) :- 
            ( call(lat:Aggr,X,E,V),X==V 
                -> true 
                ; writef('Failure\nCounterxample:\n%w(%w,%w) not equal to %w\n',[Aggr,X,E,X]),print_connective(Aggr,Type,neg),fail 
            ).

            
print_connective(Aggr,tnorm) :- writef('%w is a T-Norm connective\n',[Aggr]).
print_connective(Aggr,tconorm) :- writef('%w is a T-Conorm connective\n',[Aggr]).
print_connective(Aggr,tnorm,neg) :- writef('%w is not a T-Norm connective',[Aggr]).
print_connective(Aggr,tconorm,neg) :- writef('%w is not a T-Conorm connective',[Aggr]).


% T-CONORM
% Aggregator is or, is commutative, associative, monotone, and identity element is top

test_tconorm(Aggr) :- lat:bot(B),identity_element(Aggr,B,tconorm),print_connective(Aggr,tconorm),commutativity(Aggr),associativity(Aggr),monotony(Aggr).                
                
% IMPLICATION
% Increasing in the first parameter and decreasing in the second one

test_imp(Aggr) :- writeln('Increasing:\n'),do_test(Aggr,test_in1),writeln('Decreasing'),do_test(Aggr,test_de2).