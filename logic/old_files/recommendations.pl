:- include(auxiliaryFunctions).

recommendation0(R, Label, Strength, Transition) :-
    rdf(R, rdf:type, tmr:'ClinicalRecommendation'),
    label(R, Label),
    rdf(R, tmr:strength, literal(Strength)),
    rdf(R, tmr:basedOn, BasedOnGraph),
    rdf(_Action, tmr:causes, Transition, BasedOnGraph).

recommendation(R, Label, Strength, CausationBelief) :-
    rdf(R, rdf:type, tmr:'ClinicalRecommendation'),
    label(R, Label),
    rdf(R, tmr:strength, literal(Strength)),
    rdf(R, tmr:basedOn, CausationBelief).

/** <examples>
?- recommendation(R, Label, Strength, Transition).
*/
