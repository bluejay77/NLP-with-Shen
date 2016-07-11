% -*- Mode: Prolog -*-
%
% Sample grammar for the Earley parser
%
% Dr AJY 2016-06-24
%

rule(sentence, [noun_phrase, verb_phrase]).

rule(noun_phrase, [determiner, noun]).
rule(noun_phrase, [proper_name]).

rule(verb_phrase, [verb]).
rule(verb_phrase, [verb, noun_phrase]).
rule(verb_phrase, [adverbial_phrase, verb_phrase]).

rule(adverbial_phrase, [time_determiner]).

lex_rule(determiner, [the]).
lex_rule(determiner, ['The']).

lex_rule(noun, [man]).
lex_rule(noun, [apple]).
lex_rule(noun, ['Acer']).

lex_rule(verb, [eats]).
lex_rule(verb, [sings]).
lex_rule(verb, [debugs]).
lex_rule(verb, [debugged]).

lex_rule(proper_name, ['Antti']).
lex_rule(proper_name, ['Martti']).
lex_rule(proper_name, ['Anne']).

lex_rule(time_determiner, [today]).
lex_rule(time_determiner, [yesterday]).


% ------------------------------------------------------------



% Reads in a sentence and combines the words into a list of atoms.
%
% AJY 2012-06-24.

getsentence(Wordlist) :-
    get0(Char),
    getrest(Char, Wordlist).

getrest(46, []) :- !.  % End of sentence: 46 == the ASCII FOR '.'

getrest(32, Wordlist) :- !,  % 32 == the ASCII for a blank
    getsentence(Wordlist).

getrest(10, Wordlist) :- !,  % 10 == the ASCII for a \n
    getsentence(Wordlist).

getrest(Letter, [Word|Wordlist]) :-
    getletters(Letter, Letters, Nextchar),
    name(Word, Letters), % implode the chars into a word
    getrest(Nextchar, Wordlist).

getletters(46, [], 46) :- !.

getletters(32, [], 32) :- !.

getletters(10, [], 10) :- !.

getletters(Let, [Let|Letters], Nextchar) :-
    get0(Char),
    getletters(Char, Letters, Nextchar).



