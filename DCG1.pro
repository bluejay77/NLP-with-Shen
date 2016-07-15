% -*- Mode: Prolog -*-
% AJY 2012-07-03.
%
% Trying out Definite Clause Grammars in Prolog.


sentence --> noun_phrase, verb_phrase.

noun_phrase --> determiner, noun.
noun_phrase --> proper_name.

verb_phrase --> verb.
verb_phrase --> verb, noun_phrase.
verb_phrase --> adverbial_phrase, verb_phrase.

adverbial_phrase --> time_determiner.

determiner --> [the].
determiner --> ['The'].

noun --> [man].
noun --> [apple].
noun --> ['Acer'].

verb --> [eats].
verb --> [sings].
verb --> [debugs].
verb --> [debugged].

proper_name --> ['Antti'].
proper_name --> ['Marsu'].
proper_name --> ['Kissa']. % Nickname of Anne my sister.

time_determiner --> ['today'].
time_determiner --> ['yesterday'].


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



% and this is there to demonstrate the predicates with the Prolog
% written DGC:

go :-
    getsentence(Words),
    phrase(sentence, Words),
    nl, write('PARSE SUCCESSFUL').


% Handle inflected languages with DCG's.
%
% AJY 2012-07-03.

monikko(Sana, Monikko) :-
    atom_codes(Sana, Lista),
    atom_codes(t, KirjainT),
    append(Lista, KirjainT, MonikkoLista),
    atom_codes(Monikko, MonikkoLista), !.

yksikko(Monikko, Sana) :-
    atom_codes(Monikko, Lista),
    atom_codes(t, KirjainT),
    append(YksikkoLista, KirjainT, Lista),
    atom_codes(Sana, YksikkoLista), !.

