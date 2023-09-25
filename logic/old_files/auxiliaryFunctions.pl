/* *********************************** */
%  AUXILIARY FUNCTIONS 
/* *********************************** */

%:- rdf_set_predicate(owl:sameAs, inverse_of(owl:sameAs)).
%:- include(guidelines).

%check if a modifier is greater than another one. TRansitive closure 
%
%library(semweb/rdfs)
greaterOrderThan(Modifier1, Modifier2, Steps) :-
    (
        rdfs_individual_of(Modifier1, tmr:'Modifier') ,
        rdfs_individual_of(Modifier2, tmr:'Modifier') ,
        rdf_reachable( Modifier1, tmr:'hasHigherOrderThan', Modifier2 , infinite , Steps) -> true 
    ; 
        fail 
    ) .

sameOrderAs(Modifier1, Modifier2) :-
    (
        rdfs_individual_of(Modifier1, tmr:'Modifier') ,
        rdfs_individual_of(Modifier2, tmr:'Modifier') ,
        Modifier1 = Modifier2 -> true 
    ; 
        fail 
    ) .


%% temporary
typeOf(Inst, Type) :-
	rdf_global_id(Type, TypeURI),
    rdf(Inst, rdf:type, TypeURI).

%%% replace by 
% :- use_module(library(semweb/rdfs)).
% rdfs_individual_of(Inst, Type)
instanceOf(Inst, Type) :-
    (   rdf_global_id(Type, TypeURI),
        rdf(Inst, rdf:type, TypeURI) 
	    ;   
    	rdf_global_id(Type, TypeURI),
        rdf(Inst, rdf:type, SubType),
        rdf_reachable(SubType, rdfs:subClassOf, TypeURI)
    ).

/* *********************************** */
% different(Resource1, Resource2) {CWA - Negation as failure}
% check if two resources not the same
different(Resource1, Resource2) :-
    \+ same(Resource1, Resource2).


%%	label(+Resource, -Label:atom)

label(R, Label) :-
    rdf(R, rdfs:label, literal(lang(_, Label))).

/* *********************************** */
% same(Resource1, Resource2)    {if nothing is said, they are not the same}
% check if two resources are equal or explicitly said to be the same
same(Resource1, Resource2) :-
    rdf_global_id(Resource1, R1),
    rdf_global_id(Resource2, R2),
    (   R1 = R2 -> true
    ;   
	    rdf_reachable(R1, owl:sameAs, R2) -> true
    ;   
    	rdf_reachable(R2, owl:sameAs, R1)
    ).

reachable(X, Y) :-
    reachable(X, Y, []).
reachable(X, X, _).
reachable(X, Y, Visited) :-
    (   rdf(X, owl:sameAs, Z)
    ;   rdf(Z, owl:sameAs, X)
    ),
    \+ memberchk(Z, Visited),
    reachable(Z, Y, [Z|Visited]).


/* *********************************** */
%  FUNCTIONS FOR OUTPUT
/* *********************************** */
getLabel(Resource, Label) :-   
    (   instanceOf(Resource, tmr:'Norm'),
    rdf(Resource, rdfs:label, literal(lang(en, Label1))),
    regulates(_, Resource, Action, Strength, _),
    getLabel(Action, ActionLabel),
    concat_atom([Label1,' - by ', Strength, ' ', ActionLabel], Label) -> true
    );   
    (   instanceOf(Resource, tmr:'CausationBelief'),
    causes(Action, Effect, Frequency, Resource),
    getLabel(Action, ActionLabel),
    getLabel(Effect, EffectLabel),
    concat_atom([ActionLabel, ' ', Frequency, ' causes ', EffectLabel], Label) -> true
	);   
    (   instanceOf(Resource, tmr:'IncompatibilityBelief'),
		findall(ActionLabel,
        (rdf(Resource, tmr:'isAbout', Action),
         getLabel(Action, ActionLabel)),
 	    ActionLabels),
        concat_atom(ActionLabels, ', ', ActionsList),
		concat_atom(['Incompatibility among ', ActionsList], Label) -> true
	);   
    rdf(Resource, rdfs:label, literal(lang(en, Label))) -> true
    ;   
    rdf_global_id(data:Label, Resource).

% retrieves a dictionary interacting recommendations and the type
getInteractingRecommendations(Regulation, IntType, List, Source) :-
	distinct([Interaction,IntType,Regulation,Source],
    (instanceOf(Interaction, tmr4i:'Interaction'),
	rdf(Interaction, rdf:type, IntType),
	rdf_global_id(tmr4i:IntType, IntType),
    rdf(Interaction, tmr4i:'relates', Norm),
    rdf(Norm, tmr:'partOf', Regulation),
	rdf(Interaction, tmr4i:'relates', Elem),
    ( instanceOf(Interaction, tmr4i:'ExternalInteraction') 
    	-> (instanceOf(Elem, tmr:'Belief'),
    		rdf(Elem, prov:'wasDerivedFrom', Source))
    ;  Source = 'Internal'
    ))),
	findall(Label, (distinct([Label], 
        (rdf(Interaction, tmr4i:'relates', Resource),
         getLabel(Resource, Label)
 	    ))),List). %sort(Items, Unique)

getCaseStudyInteractions(IntType, List) :-
    rdf_global_id(data:'CIG-OA-HT-DB', Regulation),
%    getInteractingRecommendations(Regulation, IntType, List, Source).
    rdf_reachable(IntTypeURI, rdfs:subClassOf, tmr4i:'Interaction'),
    rdf(Interaction, rdf:type, IntTypeURI),
    rdf_global_id(tmr4i:IntType, IntTypeURI),
    setof(Label, interactionRecommendation(Interaction, Label, Regulation), List).

getInternallyInteractingRecommendations(Regulation, IntType, List) :-
    rdf_reachable(IntTypeURI, rdfs:subClassOf, tmr4i:'InternalInteraction'),
    rdf(Interaction, rdf:type, IntTypeURI),
    rdf_global_id(tmr4i:IntType, IntTypeURI),
    setof(Label, interactionRecommendation(Interaction, Label, Regulation), List).
    %InteractionDict = interaction{type:IntType, recList:List}.

getExternallyInteractingRecommendations(Regulation, IntType, List) :-
    rdf_reachable(IntTypeURI, rdfs:subClassOf, tmr4i:'ExternalInteraction'),
    rdf(Interaction, rdf:type, IntTypeURI),
    rdf_global_id(tmr4i:IntType, IntTypeURI),
    setof(Label, interactionRecommendation(Interaction, Label, Regulation), List).
    %InteractionDict = interaction{type:IntType, recList:List}.


interactionRecommendation(Interaction, Label, Reg) :-
    instanceOf(Interaction, tmr4i:'Interaction'),
    (rdf_reachable(Interaction, owl:sameAs, I2)
     ;
     rdf_reachable(I2, owl:sameAs, Interaction)
    ),
    rdf(I2, tmr4i:'relates', Entity),
    getLabel(Entity, Label),
    rdf(I2, tmr4i:'relates', Entity2),
    rdf(Entity2, tmr:'partOf', Reg),
    instanceOf(Reg, tmr:'Regulation').


