:- include(auxiliaryFunctions).
:- use_module(library(semweb/rdfs)).
:- use_module(library(dcg/basics)).

transition0(T, TropeType, BeforeValue, ActionL, AfterValue) :-
    rdf(T, tmr:hasExpectedSituation, After),
    label(After, AfterL),
    tropetype_value(AfterL, TropeType, AfterValue),
    (   rdf(T, tmr:hasTransformableSituation, Before)
    ->  label(Before, BeforeL),
        tropetype_value(BeforeL, TropeType, BeforeValue)
    ;   BeforeValue = '?'
    ),
    rdf(Action, tmr:causes, T),
    label(Action, ActionL).

transition(CausationBelief, TropeType, BeforeValue, ActionL, AfterValue) :-
    rdfs_individual_of(CausationBelief, tmr:'CausationBelief'),
    rdf(Action, tmr:causes, Transition, CausationBelief),
    label(Action, ActionL),
    rdf(Transition, tmr:hasExpectedSituation, After),
    label(After, AfterL),
    tropetype_value(AfterL, TropeType, AfterValue),
    (   rdf(Transition, tmr:hasTransformableSituation, Before)
    ->  label(Before, BeforeL),
        tropetype_value(BeforeL, TropeType, BeforeValue)
    ;   BeforeValue = '?'
    ).

transition(IB, 'unknown', '?', ActionL, 'undesired\n state') :-
    rdfs_individual_of(IB, tmr:'IncompatibilityBelief'),
	findall(Label,(rdf(IB, tmr:isAbout, Action),
    				( rdf(Action, tmr:administrationOf, Drug)
					  -> label(Drug, Label)
					; label(Action, Label))), 
            		ActionLabelsList),
    % assuming the example is only about drug administration
    % we make the label shorter using 'Administer' only once
    concat_atom(ActionLabelsList, '\n + ', ActionL0),
    concat_atom(['Administer ', ActionL0], ActionL).

tropetype_value(Label, TropeType, Value) :-
    atom_codes(Label, Codes),
    phrase(tropetype_value(TropeType, Value), Codes).

tropetype_value(TropeType, Value) -->
    string(TTCodes), " is ", string(VCodes), eos, !,
    { atom_codes(TropeType, TTCodes),
      atom_codes(Value, VCodes)
    }.
tropetype_value('Pain', 'yes') -->
    "Patient has Pain", !.
tropetype_value('Pain', 'no') -->
    "Patient has no Pain", !.
tropetype_value('TropeType', 'Value') -->
    string(_).


