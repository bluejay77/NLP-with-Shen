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
% Definite Clause Grammars Examples
% A simple example of parsing using DCG


sentence --> noun_phrase, verb_phrase.
noun_phrase --> determiner, noun.
verb_phrase --> verb, noun_phrase.
verb_phrase --> verb, sentence.

determiner --> [the].
determiner --> [a].
noun --> [cat].
noun --> [mouse].
verb --> [scares].
verb --> [hates].


% ?-sentence([the,cat,scares,the,mouse],[]).

% Notice that the nonterminal symbol 'sentence' has no arguments when
% used in a DCG rule, but it is called with two arguments when used for
% parsing. The two arguments are: the sentence to be parsed, and an
% empty list. When DCG rules are translated from DCG to regular Prolog
% two arguments are added to each of the nonterminal symbols. This
% explains why they need to be called with two extra arguments.
% 
