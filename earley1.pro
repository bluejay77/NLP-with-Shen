% -*- Mode: Prolog -*-
%
% Dr AJY 2016-06-24
%
% Simple Earley parser written by Stanley Lee (Univ of New Mexico, USA)
% Based on pseudo-code model from Jurafsky and Martin (2008)
% Tested on SWI Prolog 5.6.55 -- June 2008
%
%
% Program listing contains four sections:
%   1) Top level: earley/3, state_gen/2 loop and state generator predicates
%   2) Parser-state predicates
%   3) Chart-maintenance predicates (adding states to the chart)
%   4) Sample predicates to demonstrate parser
%
%
% Grammar rules are stored as facts in the database, as follows:
%   rule(NonTerminal, RHS).           RHS is list of symbols
%   lex_rule(PartOfSpeech, [Word]).
% For example:
%   rule(s, [np, vp]).
%   lex_rule(noun, [dog]).




%
% 1) Top-level parser predicates (PS is ParserState):
%      earley(NonTerminal, Words, Chart)
%      state_gen(PS, Chart)
%      predictor(State, NonTerminal, CurrentPS, NextPS)
%      scanner(State, PartOfSpeech,  CurrentPS, NextPS)
%      completer(State, PS, NextPS)
%

%
% earley(NonTerminal, Words, Chart)
% The list of symbols Words can be parsed as a NonTerminal, yielding Chart.
% Each dotted-rule state is a structure:  s(LHS, Dotted_RHS, Word_Indices)
%
earley(NonTerminal, Words, Chart) :-
    StartS = s($, [@, NonTerminal], [0,0]),      % dummy start state
    initial_parser_state(Words, StartS, PS),     % PS = ParserState
    state_gen(PS, Chart).                        % basic parser loop



%
% state_gen(ParserState, Chart)
% Chart results from iterating top-level loop, starting from ParserState
%
state_gen(PS, Chart) :-
    final_state_set_done(PS, Chart).         % Si = [] = Words  ==> finished
state_gen(PS, Chart) :-
    current_state_set_done(PS, NextPS),      % Si = [] but Words nonempty
    state_gen(NextPS, Chart).
state_gen(PS, Chart) :-                      % Si = [S|Rest]
    current_state_rhs(S, RHS, PS, PS2),      % PS2[Si] = Rest
    (
        append(_, [@, A|_], RHS),
        rule(A, _) ->                           % A is not a part of speech
                predictor(S, A, PS2, NextPS)
      ;
        append(_, [@, A|_], RHS),
        lex_rule(A, _) ->                       % A is a part of speech
                scanner(S, A, PS2, NextPS)
      ;
        completer(S, PS2, NextPS)               % S is completed state
    ),
    state_gen(NextPS, Chart).



%
% predictor(S, B, PS, NextPS)
% Dotted-rule in state S is A -> ... @ B ...
% NewPS contains new states predicting RHS for each B -> RHS in grammar.
%
predictor(S, B, PS, NewPS) :-
    S = s(_, _, [I,J]),
    findall(
             s(B, [@ | RHS], [J,J]),
             rule(B, RHS),
             NewStates
           ),
    add_to_chart(NewStates, PS, NewPS).



%
% scanner(S, Lex, PS, NextPS)
% If the next input word is the part of speech Lex, add a new state to PS;
% otherwise PS is unchanged.
% The scanner assumes that each terminal symbol (e.g., 'mary') is a
% unique part of speech (e.g., noun).
%
scanner(S, Lex, PS, NewPS) :-
    S = s(_, _, [I,J]),
    next_input(Word, J, J1, PS),
    lex_rule(Lex, [Word]),
    !,                               % commit to this lex_rule
    add_to_chart( [s(Lex, [Word,@], [J,J1])], PS, NewPS).
scanner(_, _, PS, PS).



%
% completer(S, PS, NewPS)
% for completed state S that has just recognized a B, add a new state
% for each preceding state looking for a B at the current position
%
completer(S, PS, NewPS) :-
    S = s(B, _, [J,K]),
    findall(
             s(A, BdotRHS, [I,K]),
             (
               in_chart( s(A, DotBRHS, [I,J]), PS),
               append(X, [@, B|Y], DotBRHS),
               append(X, [B, @|Y], BdotRHS)          % advance dot over B
             ),
             NewStates
           ),
    add_to_chart(NewStates, PS, NewPS).




%
% 2) Each parser state is a structure:
%      ps(Words, CurrentIndex_I, State_I, State_(I+1), ChartSoFar)
%
% Parser-state (PS) predicates:
%   initial_parser_state(Words, StartState, InitialPS)
%   final_state_set_done(PS, FinalChart)
%   current_state_set_done(CurrentPS, NextPS)
%   current_state_rhs(State, RHS, CurrentPS, NextPS)
%   next_input(Word, IndexBefore, IndexAfter, PS)
%

%
% initial_parser_state(Words, StartState, InitialPS)
% PS = ps(Words, CurrentIndex, Si, SNext, CurrentChart)
% PS invariant:  Si, SNext always subset of CurrentChart
%
initial_parser_state(Words, StartState, InitPS) :-
    InitPS = ps(Words, 0, [StartState], [], [StartState]).



%
% final_state_set_done(PS, FinalChart)
% parsing is finished when Words = Si = []
%
final_state_set_done( ps([], _, [], _, FinalChart), FinalChart).



%
% current_state_set_done(CurrentPS, NextPS)
% Words nonempty but current Si = []  ==>
% in NextPS:  Si <- SNext, SNext <- [], I <- I+1
%
current_state_set_done( ps([_|Words], I, [], SNext, Chart),
                        ps(   Words,  J, SNext, [], Chart)) :-
    J is I+1.



%
% current_state_rhs(S, RHS, CurrentPS, NextPS)
% S is first member of Si in CurrentPS; dotted-rule(S) is A -> RHS.
% S has been deleted from Si in NextPS.
%
current_state_rhs(S, RHS, ps(Words, I, [S|Si], SNext, Chart),
                          ps(Words, I,    Si,  SNext, Chart)) :-
    S = s(_, RHS, _).



%
% next_input(Word, IndexBefore, IndexAfter, PS)
% The next input Word in PS lies between the two indexes
%
next_input(Word, I, I1, ps([Word|_], I, _, _, _)) :-
    I1 is I+1.



%
% 3) Adding states to the Chart:
%       in_chart(State, PS)
%       add_to_chart(States, PS, NewPS)
%       add_to_state_set(State, PS, NewPS)
%       add_to_end(X, List, NewList)
%

%
% in_chart(State, PS)
% State is in the Chart for PS
%
in_chart(S, ps(_, _, _, _, Chart)) :-
    member(S, Chart).



%
% add_to_chart(States, PS, NewPS)
% add each state in States to PS
%
add_to_chart([], PS, PS).
add_to_chart([S|States], PS, NewPS) :-
    in_chart(S, PS),
    !,
    add_to_chart(States, PS, NewPS).
add_to_chart([S|States], PS, NewPS) :-
    add_to_state_set(S, PS, NextPS),
    add_to_chart(States, NextPS, NewPS).



%
% add_to_state_set(S, PS, NewPS)
% Add state S to Si or SNext as appropriate, and also add to Chart.
% Note: add_to_state_set assumes S is not already in Chart.
%
add_to_state_set(S, PS, NewPS) :-
    PS = ps(Words, I, Si, SNext, Chart),
    S = s(_, _, [_,J]),
    add_to_end(S, Chart, NewChart),
    (
        I == J ->                             % S is not a scan state
	          add_to_end(S, Si, NewSi),
		  NewPS = ps(Words, I, NewSi, SNext, NewChart)
               ;
	          add_to_end(S, SNext, NewSNext),
		  NewPS = ps(Words, I, Si, NewSNext, NewChart)
    ).



%
% add_to_end(Item, List, NewList)
% NewList is List with Item as its last element
%
add_to_end(X, List, NewList) :-
    append(List, [X], NewList).



%
% 4) Sample predicates to show current grammar and symbols to be parsed,
%    and run parser.
%      show/0
%      go/0
%      go(NonTerminal)
%      writesln(List)
%

%
% Show grammar and input symbols, assuming the input is a database fact:
%    input([Word1, Word2, ..., WordN]).
%
show :- listing([rule, lex_rule, input]).


go :- go(sentence).             % parse input as a sentence


go(NT) :-                       % parse input as an NT
    getsentence(Words),
    earley(NT, Words, Chart),
    writesln(Chart).            % output Chart, one state per line


% writesln(Xs) -- write each element of Xs, one per line
writesln([]).
writesln([X|Xs]) :-
    write(X),
    nl,
    writesln(Xs).

