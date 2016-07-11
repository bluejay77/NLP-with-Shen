% -*- Mode: Prolog -*-
%
% Dr AJY 2016-06-22
%
% Written for the SWI Prolog, swipl 
%
% These ones come from the University of Minnesota Twin Cities
% Downloaded from the www
%
% See https://twin-cities.umn.edu/
%
% Definite Clause Grammars Example
%
% Semantic Interpretation


/* from Bratko chapter 17 page 455.
   This comes from Pereira and Warren paper, AI journal, 1980 */

/* this is to define infix operators  and their argument precedence 
   x represents an argument whose precedence is strictly lower than that
   of the operator. y represents an argument whose precedence is lower 
   or equal than that of the operator. */

:- op(100,xfy,and).
:- op(150,xfx,'=>').

/* when using sentence we need to pass 3 arguments, 
   the first will match S in the head of the DGC clause
   the second is the list containing the words in the sentence
   the third is the empty list.
   Example:
     sentence(Meaning, [every,man,that,paints,likes,monet],[]) */

sentence(S) --> 
	noun_phrase(X,Assn,S), verb_phrase(X,Assn).
noun_phrase(X,Assn,S) --> 
	determiner(X,Prop12,Assn,S),noun(X,Prop1),rel_clause(X,Prop1,Prop12).
noun_phrase(X,Assn,Assn) --> 
	proper_noun(X).
verb_phrase(X,Assn) --> 
	trans_verb(X,Y,Assn1),noun_phrase(Y,Assn1,Assn).
verb_phrase(X,Assn) --> 
	intrans_verb(X,Assn).
rel_clause(X,Prop1,Prop1 and Prop2) --> 
	[that],verb_phrase(X,Prop2).
rel_clause(_,Prop1,Prop1) --> [].

determiner(X,Prop,Assn,all(X,(Prop => Assn))) --> [every].
determiner(X,Prop,Assn,exists(X,Prop and Assn)) --> [a].

noun(X,man(X)) --> [man].
noun(X,woman(X)) --> [woman].
proper_noun(john) --> [john].
proper_noun(annie) --> [annie].
proper_noun(monet) --> [monet].
trans_verb(X,Y,like(X,Y)) --> [likes].
trans_verb(X,Y,admire(X,Y)) --> [admires].
intrans_verb(X,paint(X)) --> [paints].

/* examples */
/*
?- sentence(S,[every,man,that,paints,likes,monet],[]).
?- sentence(S,[a,woman,that,admires,john,paints],[]).
?- sentence(S,[every,woman,that,likes,a,man,that,admires,monet,paints],[]).
?- sentence(S,[john,likes,annie],[]).
?- sentence(S,[annie,likes,a,man,that,admires,monet],[]).
*/

