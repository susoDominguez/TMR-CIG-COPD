:- use_module(library(semweb/turtle)).        % Turtle and TriG
:- use_module(library(semweb/rdf_ntriples)).
:- use_module(library(semweb/rdf_zlib_plugin)).
:- use_module(library(semweb/rdf_http_plugin)).
:- use_module(library(http/http_ssl_plugin)).
:- use_module(library(semweb/rdf_ntriples)).
:- use_module(library(semweb/rdf11)).
%:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(dcg/basics)).
:- use_module(library(dif)).

:- rdf_prefix(data, 'http://anonymous.org/tmr/data/').
:- rdf_prefix(tmr, 'http://anonymous.org/tmr/').
:- rdf_prefix(tmr4i, 'http://anonymous.org/tmr4i/').
:- rdf_prefix(oa, 'http,//www.w3.org/ns/oa#'). 
:- rdf_prefix(prov, 'http://www.w3.org/ns/prov#').
:- rdf_prefix(nanopub, 'http://www.nanopub.org/nschema#').


% set properties of RDF predicates
:- rdf_set_predicate(tmr4i:'comparedWith', symmetric(true)) .
:- rdf_set_predicate(tmr4i:'comparedWith', transitive(false)) .
:- rdf_set_predicate(owl:'sameAs', symmetric(true)) .
:- rdf_set_predicate(owl:'sameAs', transitive(true)) .
:- rdf_set_predicate(tmr:'subsumes', transitive(true)) .
:- rdf_set_predicate(tmr:'subsumes', symmetric(false)) .
:- rdf_set_predicate(tmr:'administrationOf', transitive(false)) .
:- rdf_set_predicate(tmr:'administrationOf', symmetric(false)) .
:- rdf_set_predicate(tmr:'hasComposition', transitive(true)) .
:- rdf_set_predicate(tmr:'hasComposition', symmetric(false)) .
:- rdf_set_predicate(tmr:'incompatibleWith', symmetric(true)) .
:- rdf_set_predicate(tmr:'hasEqOrder', symmetric(true)) .
:- rdf_set_predicate(tmr:'hasEqOrder', transitive(true)) .
:- rdf_set_predicate(tmr:'hasHigherOrderThan', transitive(true)) .
:- rdf_set_predicate(tmr:'hasHigherOrderThan', symmetric(false)) .
:- rdf_set_predicate(tmr:'subsumedBy', transitive(true)) .
:- rdf_set_predicate(tmr:'subsumedBy', symmetric(false)) .
:- rdf_set_predicate(tmr:'subsumedBy', inverse_of('http://anonymous.org/tmr/subsumes')) .
%%%%%%%%%%%%%%%%%%%

% given an URI, create the predicate logic formula that represents 
% its precondition. 
create_formula(URI, Formula, Mapping) :-
    build_tree(URI, Tree), !,
    assign_prolog_vars(Tree, Formula, Mapping).

% if the URI is of type PredicateType, we are dealing with a leaf
% and we don't need to recurse any more
build_tree(URI, p(Label)) :-
    rdf(URI, rdf:type, vocab:'SituationType'),
    !,
    rdf(URI, rdfs:label, literal(Label)).
% if the URI has 'and' relationships, write a * in the tree and recurse
build_tree(URI, *(Parts)) :-
    rdf(URI, rdf:type, vocab:'PreconditionType'),
    rdf(URI, vocab:and, _),
    !,
    findall(Member, rdf(URI, vocab:and, Member), Members),
    maplist(build_tree, Members, Parts).
% if the URI has 'or' relationships, write a + in the tree and recurse
build_tree(URI, +(Parts)) :-
    rdf(URI, rdf:type, vocab:'PreconditionType'),
    rdf(URI, vocab:or, _),
    !,
    findall(Member, rdf(URI, vocab:or, Member), Members),
    maplist(build_tree, Members, Parts).
% if the URI has 'neg' relationships, write a ~ in the tree and recurse
build_tree(URI, ~(Part)) :-
    rdf(URI, rdf:type, vocab:'PreconditionType'),
    rdf(URI, vocab:neg, Member),
    !,
    build_tree(Member, Part).

%%%%%%%%%%
%TODO: Reg could be given by service to contextualize the interactions search
%this version of the predicate discards the CBs so no iteration of regulates occurs for each CB in Norm
regulates(Reg, Norm, ActionT, Strength):-
    rdfs_individual_of(Reg, tmr:'ClinicalGuideline'), %for each available CG. This must be filtered to the contextualized CG
    rdf(Norm, tmr:'isPartOf', Reg, Reg),
    rdfs_individual_of(Norm, tmr:'ClinicalRecommendation'),
    rdf(Norm, tmr:'partOf', Reg, Norm),%potentially, this Reg could be distinct to the one above when combining guidelines by specifying a new isPartOf object
    rdf(Norm, tmr:'strength', Strength, Norm),
    rdf(Norm, tmr:'aboutExecutionOf', ActionT, Norm).
regulates(Reg, Norm, ActionT, Strength, CBelief):-
    regulates(Reg, Norm, ActionT, Strength),
    rdf(Norm, tmr:'basedOn', CBelief, Norm).
regulates(Reg, Norm, ActionT, Strength, CBelief, Contrib):-
    regulates(Reg, Norm, ActionT, Strength, CBelief),
    rdf(CBelief, tmr:contribution, Contrib, Norm).

/* *********************************** */

has_relation_symm(Act1, Act2, Norm1, Norm2):-
            (related_effect_symm(Act1, Act2) 
             -> true 
            ;
             subsumed_due_to_effect_symm(Act1, Act2, Norm1, Norm2)
             -> true 
            ).

related_effect_symm(CareAction1, CareAction2) :-
    nonvar(CareAction1),
    nonvar(CareAction2),
    (
        rdf_equal(CareAction1, CareAction2) 
        -> true %T if same care action
    ;   %they are the same semantically or can be reached using semantic same transitive closure
        semanticallySameAs(CareAction1 , CareAction2) 
        ->  true
    ;
        related_to_symm(CareAction1,CareAction2) 
    ).


subsumed_due_to_effect_symm(Action1, Action2, Norm1, Norm2) :-
    rdf(Action1, tmr:'administrationOf', DrugType1),
    rdfs_individual_of(DrugType1, tmr:'DrugCategory'),
    rdf(Action2, tmr:'administrationOf', DrugType2),
    rdfs_individual_of(DrugType2, tmr:'DrugCategory'),
    (   
        hasActiveGroupingCriterion(DrugType1,Action1,Norm1,Tr),
        hasActiveGroupingCriterion(DrugType2,Action2,Norm2,Tr)
        -> true
    ;
        hasActiveGroupingCriterion(DrugType2,Action2,Norm2,Tr),
        hasActiveGroupingCriterion(DrugType1,Action1,Norm1,Tr)
    ).

subsumed_due_to_effect(Action1, Action2, Norm1, Norm2) :-
        rdf(Action1, tmr:'administrationOf', DrugType1),
        rdfs_individual_of(DrugType1, tmr:'DrugCategory'),
        rdf(Action2, tmr:'administrationOf', DrugType2),
        rdfs_individual_of(DrugType2, tmr:'DrugCategory'),
        hasActiveGroupingCriterion(DrugType1,Action1,Norm1,Tr),
        hasActiveGroupingCriterion(DrugType2,Action2,Norm2,Tr).

%TODO: select CBs that are not side effects
hasActiveGroupingCriterion(DrugType,Action,Norm,Tr) :-
    rdf(DrugType, tmr:'hasGroupingCriteria', Tr),
    rdf(Norm, tmr:'basedOn', CBelief, Norm),
    causes(Action, Tr, _, CBelief).
    

%symmetric predicate to find out whether to given Action types have a relation (same, subsumes, hasComponent)
related_to_symm(CareAction1,CareAction2) :- 
    nonvar(CareAction1),
    nonvar(CareAction2),
    (   
       %if CareAction1 is related to CareAction2
        isRelatedTo(CareAction1, CareAction2) 
        -> true  %checks also for CareAction3 s.t. its owl:same as CareAction2
    ;   %if CareAction2 is related to CareAction1
        isRelatedTo(CareAction2, CareAction1) 
        -> true %checks also for CareAction3 s.t. its owl:same as CareAction1
    ;   %if CareAction1 is semantically the same as another careAction type 3 and the latter is related to careAction 2
        semanticallySameAs(CareAction1,CareAction3),
        isRelatedTo(CareAction3, CareAction2)
        -> true
    ;   %the symmetric version of above
        semanticallySameAs(CareAction2, CareAction3),
        isRelatedTo(CareAction3, CareAction1)
    ).

related_to(CareAction1,CareAction2) :- 
        nonvar(CareAction1),
        nonvar(CareAction2),
        (   
           %if CareAction1 is related to CareAction2
            isRelatedTo(CareAction1, CareAction2) 
            -> true  %checks also for CareAction3 s.t. its owl:same as CareAction2
        ;   
            %if CareAction1 is semantically the same as another careAction type 3 and the latter is related to careAction 2
            semanticallySameAs(CareAction1,CareAction3),
            isRelatedTo(CareAction3, CareAction2)
        ).

% check if a careAction is related, but not equal, to another via subsumption, hasComponent and the composition of both
isRelatedTo(CareAction1, CareAction2) :-
    nonvar(CareAction1),
    nonvar(CareAction2),
    rdf_not_equal(CareAction1, CareAction2), %related but not equal
    rdf(CareAction1, tmr:'administrationOf', DrugType1),
    rdf(CareAction2, tmr:'administrationOf', DrugType2),
    (   
        subsumes(CareAction1 , CareAction2) -> true  %by definition CareAction2 cannot be a Compound Drug Type so not followed by component property
    ;
        hasComponentRelation(DrugType1,DrugType2) -> true  %the property already handles owl:same
    ;   %below are the cases where some free variables are at play
        hasComponentRelation(DrugType1, DrugType3),
        rdf(CareAction3, tmr:'administrationOf', DrugType3),
            (
                subsumes(CareAction3, CareAction2) -> true %checks for owl:same on 2nd param
            ;
                semanticallySameAs(CareAction3, CareAction4),
                rdf_not_equal(CareAction4, CareAction1),
                subsumes(CareAction4, CareAction2)
            )
        
    ) .

% semanticallyDifferent(Resource1, Resource2) {CWA - Negation as failure}
% check if two resources are not the semantically the semanticallySameAs

/* *********************************** */
% semanticallySameAs(Resource1, Resource2)    {if nothing is said, they are not the semanticallySameAs}
% check if two resources are explicitly said to be the semanticallySameAs (semantic equivalence)
%owl:semanticallySameAs must be set as a symmetric predicate
semanticallySameAs(Resource1, Resource2) :-
    rdf_reachable(Resource1, owl:sameAs, Resource2),
    rdf_not_equal(Resource1, Resource2).

semanticallyDifferent(Resource1, Resource2) :-
    \+ semanticallySameAs(Resource1, Resource2),
    rdf_not_equal(Resource1, Resource2). %This case is required for the transitive closure of owl:same as it succeeds with R1=R1.

rdf_not_equal(Resource1,Resource2) :-
    \+ rdf_equal(Resource1,Resource2).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/* *********************************** */
%  EVENT_TYPE-BASED RULES
/* *********************************** */
%

/* *********************************** */
%


% improve with selection of relieable sources
%using the transitive closure of  tmr:subsumes, find all objects that are subsumed.
%order of disjuncts matter
subsumes(SType, Type) :-
    nonvar(SType),
    dif(SType, Type),
    (
        nonvar(Type), 
        rdf_reachable(SType, tmr:'subsumes', Type)
        -> true 
    ;
        nonvar(Type), 
        semanticallySameAs(Type, Type2),
        dif(SType, Type2),
        rdf_reachable(SType, tmr:'subsumes', Type2)
        -> true 
    ;
        var(Type), %find all unification solutions
        dif(SType,Type2),
        rdf_reachable(SType, tmr:'subsumes', Type2),
        (
            rdf_equal(Type2, Type) %expands goal
            ;
            semanticallySameAs(Type2, Type)
        ) 
    ).

%RElation from combined Drug types to drug types
%apply sameAS to second parameter and sameAs for first parameter must be call from outside, as parameters of CType
hasComponentRelation(CType, Type):-
    nonvar(CType),
    rdfs_individual_of(CType, tmr:'CombinedDrugType'),
    (
        nonvar(Type),
        once(hasDirectComponent(CType,Type)) 
        -> true
    ;
        nonvar(Type), 
        semanticallySameAs(Type,Type2),
        once(hasDirectComponent(CType,Type2))
        -> true
    ;
        var(Type), %This case requires to find all possible unifications
        hasDirectComponent(CType,Type2),
        (
            rdf_equal(Type2, Type) %expands goal
            ;
            semanticallySameAs(Type2, Type)
        )     
    ).

%tests whether Type is a direct component of CType, or finds a direct component of CType
hasDirectComponent(CType,Type) :-
    nonvar(CType),
    dif(CType, Type);
    rdf_reachable(CType, tmr:'hasComponent', Type),
    semanticallyDifferent(CType,Type).

similarOrEqTrTypes(Tr1,Tr2,ModVal):-
        var(ModVal),
        nonvar(Tr1),
        nonvar(Tr2),
        (
            rdf_equal(Tr1, Tr2),
            ModVal = 1
            -> true
        ;
            semanticallySameAs(Tr1, Tr2),
            ModVal = 1
            -> true 
        ;
            hasTransition(TropeT,PreSitT1,Derivative,PostSitT1,Tr1),
            hasTransition(TropeT,PreSitT2,Derivative,PostSitT2,Tr2),
                (
                rdf_equal(PreSit1, PreSit2),
                ModVal = 1
                -> true
                ;
                semanticallySameAs(PreSit1, PreSit2),
                ModVal = 1
                -> true
                ;
                ModVal = 0.5
                -> true
                ;
                fail
                )
            -> true
        ;
            fail
        ).

inverseTrTypesModal(Tr1, Tr2, ModVal):-
        var(ModVal),
        nonvar(Tr1),
        nonvar(Tr2),
        dif(Tr1, Tr2),
        semanticallyDifferent(Tr1, Tr2),
        hasTransition(TropeT,PreSitT1,Derivative1,PostSitT1,Tr1),
        hasTransition(TropeT,PreSitT2,Derivative2,PostSitT2,Tr2),
        \+rdf(Derivative1, tmr:hasEqOrder, Derivative2),
        (
            (
                rdf_equal(PreSitT1, PostSitT2)
                -> true
                ;
                semanticallySameAs(PreSitT1, PostSitT2)
                -> true
            ),
            (
                rdf_equal(PreSitT2, PostSitT1)
                -> true
                ;
                semanticallySameAs(PreSitT2, PostSitT1)
                -> true
            ),
            ModVal = 1
            -> true
        ;
            (
                rdf_equal(PreSitT1, PreSitT2)
                -> true
            ;
                semanticallySameAs(PreSitT1, PreSitT2)
                -> true
            ),
            ModVal = 1
            -> true
        ;
            %the most generic inverse Tr. Subsumes previous one
            ModVal = 0.5
            -> true
        ; 
            fail 
        ).

inverseTrTypes(Tr1, Tr2):-
    nonvar(Tr1),
    nonvar(Tr2),
    rdf_not_equal(Tr1, Tr2),
    semanticallyDifferent(Tr1, Tr2),
    hasTransition(TropeT,PreSitT1,Derivative1,PostSitT1,Tr1),
    hasTransition(TropeT,PreSitT2,Derivative2,PostSitT2,Tr2),
    rdf_not_equal(Derivative1, Derivative2),
    (
        (
        rdf_equal(PreSitT1, PostSitT2)
        -> true
        ;
        semanticallySameAs(PreSitT1, PostSitT2)
        -> true
        ),
        (
        rdf_equal(PreSitT2, PostSitT1)
        -> true
        ;
        semanticallySameAs(PreSitT2, PostSitT1)
        -> true
        )
        -> true
    ;
            (
            rdf_equal(PreSitT1, PreSitT2)
            -> true
            ;
            semanticallySameAs(PreSitT1, PreSitT2)
            -> true
            ) 
            -> true
        %;
        %the most generic inverse Tr. Subsumes previous one
        %true
    ).

hasTransition(TropeT,PreSitT,Derivative,PostSitT,Tr):-
    rdfs_individual_of(Tr, tmr:'TransitionType'),
    rdf(Tr, tmr:affects, TropeT),
    rdf(Tr, tmr:derivative, Derivative),
    rdf(Tr, tmr:affects, TropeT),
    rdf(Tr, tmr:hasTransformableSituation, PreSitT),
    rdf(Tr, tmr:hasExpectedSituation, PostSitT).

is_greater_modifier_than(Modifier1, Modifier2):-
    nonvar(Modifier2),
    rdfs_individual_of(Modifier2, tmr:'Modifier'),
    dif(Modifier1, Modifier2),
    rdf_reachable(Modifier1, tmr:hasHigherOrderThan, Modifier2).

is_greater_or_eq_modifier_than(Modifier1, Modifier2):-
    ( 
      is_greater_modifier_than(Modifier1, Modifier2)
      -> true 
      ;
      nonvar(Modifier2),
      rdfs_individual_of(Modifier2, tmr:'Modifier'),
      rdf_reachable(Modifier1, tmr:hasEqOrder, Modifier2) 
    ).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%    

%%	label(+Resource, -Label:atom)

label(R, Label) :-
    rdf(R, rdfs:label, literal(lang(_, Label))).



/* *********************************** */
%  BELIEF-BASED RULES
/* *********************************** */

causes(ActionT, Tr, Frequency, CBelief) :-
    rdfs_individual_of(CBelief, tmr:'CausationBelief'),
    rdf(CBelief, tmr:'frequency', Frequency, CBelief),
    rdf(ActionT, tmr:'causes', Tr, CBelief).
causes(ActionT, Tr, Frequency, CBelief, Source) :-
    causes(ActionT, Tr, Frequency, CBelief),
    rdf(CBelief, prov:'wasDerivedFrom', Source).

basedOn(Norm, CB, Cntrb):-
        nonvar(Norm),
        rdf(Norm, tmr:basedOn, CB, Norm),
        rdf(CB, tmr:contribution, Cntrb, Norm).

similarTo(CBelief1, CBelief2) :-
    rdfs_individual_of(CBelief1, tmr:'CausationBelief'),
    rdfs_individual_of(CBelief2, tmr:'CausationBelief'),
    causes(Action1, EventT3, Frequency1, CBelief1),
    rdf_reachable(Frequency1, tmr:hasHigherOrderThan, tmr:'Rarely'),
    dif(Action1,Action2),
    causes(Action2, EventT4, Frequency2, CBelief2),
    rdf_reachable(Frequency2, tmr:hasHigherOrderThan, tmr:'Rarely'),
    semanticallyDifferent(Action1, Action2),
    %not related by subsumption.
    \+subsumes(Action1,Action2),
    \+subsumes(Action2,Action1),
    \+inverseTrTypes(EventT3, EventT4).

incompatibleWith(EventT1, EventT2, IBelief) :-
    rdfs_individual_of(EventT1, tmr:'EventType'),
    rdfs_individual_of(EventT2, tmr:'EventType'),
    rdf(EventT1, tmr:'incompatibleWith', EventT2, IBelief).



interaction_(I, Label, Elements) :-
    rdfs_individual_of(I, tmr4i:'Interaction'),
    rdf(I, rdf:type, Class),
    label(Class, Label),
    findall(E, interaction_element(I, E), Elements0),
    sort(Elements0, Elements).

interaction_element(I, E) :-
    ( rdf_reachable(I, owl:sameAs, I2)
    ; 
    rdf_reachable(I2, owl:sameAs, I)),  
    rdf(I2, tmr4i:relates, E).

interaction(I, Label, Elements) :-
        distinct(Label-Elements, interaction_(I, Label, Elements)).


/* *********************************** */
%  INTERACTION-BASED RULES
/* *********************************** */ 

% Check if an interaction exist among two norms
interacts(IntTypeUri, Norm1, Norm2, Interaction) :-
    rdfs_individual_of(Interaction, IntTypeUri),
    rdf(Interaction, tmr4i:relates, Norm1),
    rdf(Interaction, tmr4i:relates, Norm2).

% Check if an interaction exist among two norms, caused by 2 CBs with a modal strength
interacts(IntTypeUri, Norm1, Norm2, CB1, CB2, Interaction) :-
    rdfs_individual_of(Interaction, IntTypeUri),
    rdf(Interaction, tmr4i:relates, Norm1),
    rdf(Interaction, tmr4i:relates, Norm2),
    rdf(Interaction, tmr4i:identifies, CB1),
    rdf(Interaction, tmr4i:identifies, CB2).


/* *********************************** */
% ** Assert an interaction of a certain type between the two recommendations
% just in case the interaction does not already exist
existsInteraction(IntTypeURI, Norm1, Norm2) :-
    (   interacts(IntTypeURI, Norm1, Norm2, _) 
        -> true
    ;
        % composing new URI for interaction
        rdfs_label(IntTypeURI, Label),
        split_string(Label, " ", "\s\t\n", [PrefixLbl|_]),
        atom_concat(PrefixLbl, #, Atom3),
        gensym(Atom3, NewID),
        rdf_global_id(data:NewID, NewURI),
        % asserting the interaction
        rdf_assert(NewURI, rdf:type, IntTypeURI, my_entailments),
        rdf_assert(NewURI, tmr4i:relates, Norm1, my_entailments),
        rdf_assert(NewURI, tmr4i:relates, Norm2, my_entailments)
    ).


/* *********************************** */
% ** Assert an interaction of a certain type between the two recommendations
% just in case the interaction does not already exist
existsInteraction(IntTypeURI, Norm1, Norm2, CB1, CB2, ModVal) :-
    (   interacts(IntTypeURI, Norm1, Norm2, CB1, CB2, _) 
        -> true
    ;
        % composing new URI for interaction
        rdfs_label(IntTypeURI, Label),
        split_string(Label, " ", "\s\t\n", [PrefixLbl|_]),
        atom_concat(PrefixLbl, #, Atom3),
        gensym(Atom3, NewID),
        rdf_global_id(data:NewID, NewURI),
        % asserting the interaction
        rdf_assert(NewURI, rdf:type, IntTypeURI, my_entailments),
        rdf_assert(NewURI, tmr4i:relates, Norm1, my_entailments),
        rdf_assert(NewURI, tmr4i:relates, Norm2, my_entailments),
        rdf_assert(NewURI, tmr4i:identifies, CB1, my_entailments),
        rdf_assert(NewURI, tmr4i:identifies, CB2, my_entailments),
        rdf_assert(NewURI, tmr4i:hasModalStregth, ModVal, my_entailments)
    ).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Interaction formulae
is_safe_contribution_with_modal_str(Tr1,Cntrb2,Freq2,Tr2, ModVal):-
    is_greater_or_eq_modifier_than(Freq2, tmr:'Sometimes'),
    (
        rdf(Cntrb2, tmr:hasEqOrder, tmr:'Negative'),
        similarOrEqTrTypes(Tr1,Tr2,ModVal)
        -> true
    ;
        \+rdf(Cntrb2, tmr:hasEqOrder, tmr:'Negative'),
        inverseTrTypesModal(Tr1,Tr2,ModVal)
        -> true
    ;
    fail
    ).
%check Safety Interaction
%Detects recommendations that are considered to be safe with respect to other ones due to  the awareness of negative causations regarding a transition
detect_safety :-
    rdf_equal(tmr4i:'Safety', IntTypeFullUri),
    forall((
        dif(Norm1,Norm2),
        regulates(Reg, Norm1, Act1, DeonticValUri1),
        regulates(Reg, Norm2, Act2, DeonticValUri2),
        %split search space
        \+rdf(Norm2,tmr4i:comparedWith,Norm1,IntTypeFullUri),
        rdf_assert(Norm1,tmr4i:comparedWith,Norm2,IntTypeFullUri),
        %endOf split search space
        is_greater_or_eq_modifier_than(DeonticValUri1, tmr:'Should'),
        is_greater_or_eq_modifier_than(DeonticValUri2, tmr:'Should'),
        \+has_relation_symm(Act1, Act2, Norm1, Norm2),
        basedOn(Norm1, CB1, Cntrb1),
        causes(Act1, Tr1, Freq1, CB1),
        basedOn(Norm2, CB2, Cntrb2),
        causes(Act2, Tr2, Freq2, CB2),
        (
            rdf(Cntrb1, tmr:hasEqOrder, tmr:'Negative'),
            \+is_greater_or_eq_modifier_than(Freq1, tmr:'Sometimes'),
            is_greater_or_eq_modifier_than(Freq2, tmr:'Sometimes'),
            is_safe_contribution_with_modal_str(Tr1,Cntrb2,Freq2,Tr2, ModVal)
            -> true
            ;
            rdf(Cntrb2, tmr:hasEqOrder, tmr:'Negative'),
            \+is_greater_or_eq_modifier_than(Freq2, tmr:'Sometimes'),
            is_greater_or_eq_modifier_than(Freq1, tmr:'Sometimes'),
            is_safe_contribution_with_modal_str(Tr2,Cntrb1,Freq1,Tr1, ModVal)
            -> true
        )
    ),
        existsInteraction(IntTypeFullUri, Norm1, Norm2, CB1, CB2, ModalVal)
    ),
    rdf_retractall(_, tmr4i:comparedWith, _, IntTypeFullUri).


%check side-effect
%Recommendations here one causation belief can negatively interfere on the effects of the other
detect_side_effect :-
    rdf_equal(tmr4i:'SideEffect', IntTypeFullUri),
    forall(
        (
        dif(Norm1,Norm2),
        regulates(Reg, Norm1, Act1, DeonticValUri1),
        regulates(Reg, Norm2, Act2, DeonticValUri2),
        %split search space
        %\+rdf(Norm2,tmr4i:comparedWith,Norm1,IntTypeFullUri),
        %rdf_assert(Norm1,tmr4i:comparedWith,Norm2,IntTypeFullUri),
        %endOf split search space
        is_greater_or_eq_modifier_than(DeonticValUri1, tmr:'Should'),
        is_greater_or_eq_modifier_than(DeonticValUri2, tmr:'Should'),
        \+has_relation_symm(Act1, Act2, Norm1, Norm2),
        basedOn(Norm1, CB1, Cntrb1),
        basedOn(Norm2, CB2, Cntrb2),
        (
            is_greater_or_eq_modifier_than(Cntrb1, tmr:'Positive'),
            \+is_greater_or_eq_modifier_than(Cntrb2, tmr:'Positive')
            -> true
        ;
            is_greater_or_eq_modifier_than(Cntrb2, tmr:'Positive'),
            \+is_greater_or_eq_modifier_than(Cntrb1, tmr:'Positive')
            -> true      
        ),
        inverseTrTypesModal(Tr1, Tr2, ModVal)
        ),
        existsInteraction(IntTypeFullUri, Norm1, Norm2, CB1, CB2, ModalVal)
    ), rdf_retractall(_, tmr4i:comparedWith, _, IntTypeFullUri).

%check Compliance 
%Recommendations that agree that a transition or derivative negatively contributes, regardless of the deontic strength
detectCompliance :-
    rdf_equal(tmr4i:'Compliance', IntTypeFullUri),
    forall(
        ( dif(Norm1,Norm2),
        regulates(Reg, Norm1, Act1, DeonticValUri1),
        regulates(Reg, Norm2, Act2, _),
        %split search space
        \+rdf(Norm2,tmr4i:comparedWith,Norm1,IntTypeFullUri),
        rdf_assert(Norm1,tmr4i:comparedWith,Norm2,IntTypeFullUri),
        %endOf split search space
        is_greater_or_eq_modifier_than(DeonticValUri1, tmr:'Should'),
        \+has_relation_symm(Act1, Act2, Norm1, Norm2),
        basedOn(Norm1, CB1, tmr:'Negative'),
        basedOn(Norm2, CB2, tmr:'Negative'),
        causes(Act1, Tr1, Freq1, CB1),
        causes(Act2, Tr2, Freq2, CB2),
        similarOrEqTrTypes(Tr1,Tr2,ModVal)
        ), 
            existsInteraction(IntTypeFullUri, Norm1, Norm2, CB1, CB2, ModalVal)
        ),
        rdf_retractall(_, tmr4i:comparedWith, _, IntTypeFullUri).
        
% check Contradiction
detect_contradiction :-
    rdf_equal(tmr:'Should', DeonticValUri1), %on top level it automatically converts prexi:local to fullURI
    rdf_equal(tmr:'Should_not', DeonticValUri2),
    rdf_equal(tmr4i:'Contradiction', IntTypeFullUri),
    rdf_equal(tmr:'Always', ProbValUri1),
    forall(
        ( 
          regulates(Reg, Norm1, Act1, DeonticValUri1, CB1),
          dif(Norm1,Norm2),
     	  regulates(Reg, Norm2, Act2, Strength, CB2),
     	  causes(Act1, Tr1, ProbValUri1, CB1),
     	  causes(Act2, Tr2, ProbValUri1, CB2),
     	  ( 
            Strength = DeonticValUri1,
            semanticallyDifferent(Act1,Act2),
            inverseTrTypes(Tr1,Tr2)
            -> true
            ;
            %shouldnot
            Strength = DeonticValUri2,
            (
                related_effect_symm(Act1, Act2)
                -> true
 	        ;
                semanticallyDifferent(Act1,Act2),
                \+inverseTrTypes(Tr1,Tr2)
                -> true
            )
            -> true
          )
        ),
            existsInteraction(IntTypeFullUri, Norm1, Norm2)
        ).

% check RepeatedAction
detect_repetition :-
        rdf_equal(tmr:'Should', DeonticValUriThreshold), %on top level it converts internally prexi:local to fullURI
        rdf_equal(tmr4i:'RepeatedAction', IntTypeFullUri),
        forall(
                ( 
                dif(Norm1,Norm2),
                regulates(Reg, Norm1, Act1, DeonticValUri1),
                regulates(Reg, Norm2, Act2,DeonticValUri2),
                %split search space
                \+rdf(Norm2,tmr4i:comparedWith,Norm1,IntTypeFullUri),
                rdf_assert(Norm1,tmr4i:comparedWith,Norm2,IntTypeFullUri),
                %endOf split search space
                %assert deontic values are positive
                is_greater_or_eq_modifier_than(DeonticValUri1, DeonticValUriThreshold),
                is_greater_or_eq_modifier_than(DeonticValUri2, DeonticValUriThreshold),
                once(has_relation_symm(Act1, Act2, Norm1, Norm2))
                ), 
                existsInteraction(IntTypeFullUri, Norm1, Norm2)
            ),
            % accumulate RepeatedAction
            forall(
                (
                interacts(IntTypeFullUri, Norm1, Norm2, I1),
                dif(Norm2,Norm3),
                interacts(IntTypeFullUri, Norm3, Norm1, I2)
                ),
                rdf_assert(I1, owl:sameAs, I2, my_entailments)
                ),
                %remove temp assertion for this int type
            rdf_retractall(_, tmr4i:comparedWith, _, IntTypeFullUri).
    
detect_alternative_actions :-
        %on top level it automatically converts prexi:local to fullURI
        rdf_equal(tmr4i:'AlternativeActions', IntTypeURI),
        rdf_equal(tmr:'Should', DeonticValUri1),
        forall((
             regulates(Reg, Norm1, _, DeonticValUri1, CB1),
             dif(Norm1,Norm2),
             regulates(Reg, Norm2, _, DeonticValUri1, CB2),
             similarTo(CB1, CB2) %includes diff(a1, a2) and diff(n1, n2).
         ),
         existsInteraction(IntTypeURI, Norm1, Norm2)),
        % accumulate AlternativeActions
        forall( (
            interacts(IntTypeURI, Norm1, Norm2, I1),
            dif(Norm1, Norm3),
            interacts(IntTypeURI, Norm2, Norm3, I2)
         ),
         rdf_assert(I1, owl:sameAs, I2, my_entailments) ).
    
detect_repairable_transition :-
        %on top level it automatically converts prexi:local to fullURI
        rdf_equal(tmr4i:'RepairableTransition', IntTypeFullUri),
        forall(
                (
                dif(Norm1, Norm2),
                regulates(Reg, Norm1, Act1, DeonticValUri2),
                regulates(Reg, Norm2, Act2, DeonticValUri1),
                %split search space
                %\+rdf(Norm2,tmr4i:comparedWith,Norm1,IntTypeFullUri),
                %rdf_assert(Norm1,tmr4i:comparedWith,Norm2,IntTypeFullUri),
                %endOf split search space
                semanticallyDifferent(Act1,Act2),
                is_greater_or_eq_modifier_than(DeonticValUri1, tmr:'Should'),
                \+is_greater_or_eq_modifier_than(DeonticValUri2, tmr:'Should'),
                \+has_relation_symm(Act1, Act2, Norm1, Norm2),
                basedOn(Norm1, CB1, Cntrb1),
                basedOn(Norm2, CB2, Cntrb2),
                causes(Act1, Tr1, ProbValUri1, CB1),
                causes(Act2, Tr2, ProbValUri1, CB2),
                inverseTrTypes(Tr1, Tr2)
                ),
                    existsInteraction(IntTypeURI, Norm1, Norm2)
            ) .
    
    % infer the internal interactions for all the norms in a regulation


inferInternalInteractions :-
    detect_contradiction,
    detect_repetition,
    detect_alternative_actions,
    detect_repairable_transition.