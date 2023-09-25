:- rdf_prefix(data, 'http://anonymous.org/data/').
:- rdf_prefix(tmr, 'http://anonymous.org/tmr/').
:- rdf_prefix(tmr4i, 'http://anonymous.org/tmr4i/').
:- rdf_prefix(oa, 'http,//www.w3.org/ns/oa#').
:- rdf_prefix(prov, 'http://www.w3.org/ns/prov#').
:- rdf_prefix(nanopub, 'http://www.nanopub.org/nschema#').

:- include(auxiliaryFunctions).
:- include(nanopublication).

:- rdf_set_predicate(subsumes, symmetric(true)) .
/* *********************************** */
%  EVENT_TYPE-BASED RULES
/* *********************************** */
%
inverseTo(TransitionT1, TransitionT2) :-
	rdfs_individual_of(TransitionT1, tmr:'TransitionType'),
	rdfs_individual_of(TransitionT2, tmr:'TransitionType'),
    rdf(TransitionT1, tmr:'hasTransformableSituation', S1),
    rdf(TransitionT1, tmr:'hasExpectedSituation', S2),
    rdf(TransitionT2, tmr:'hasTransformableSituation', S2),
    rdf(TransitionT2, tmr:'hasExpectedSituation', S1),
    different(TransitionT1, TransitionT2).

/* *********************************** */
% relatedTypes(Type1, Type2)
% check if two types are the same or subsuming one another
relatedTypes(Type1, Type2) :-
    (   same(Type1, Type2) ->  true
    ;
        subsumes(Type1, Type2) ->  true
    ;
        subsumes(Type2, Type1) ->  true
    ;
        (subsumes(Type1, Type3),
         same(Type3, Type2) )
    ).

%makes hasComposition to hold whenever there is a one component of drugCombinationType that is part of the property
hasComponent(CType, Type) :-
    %rdf_global_id(CType1, CType),
    %rdf_global_id(Type1, Type),
    rdfs_individual_of(CType, tmr:'CombinedDrugType'),
    (
    %not equal types
    CType \= Type,
       rdf_reachable(CType, tmr:'hasComponent', Type) -> true 
    ;
        subsumes(Type, Type2), rdf_reachable(CType, tmr:'hasComponent', Type2)
    ) .


% makes subsumption to hold whenever there is a positive cbelief
% improve with selection of relieable sources
subsumes(SType, Type) :-
    rdf_set_predicate(+Predicate, +Property) ,
    %rdf_global_id(SType1, SType),
    %rdf_global_id(Type1, Type),
    % SuperType subsumes Type if
    (   SType \= Type,
        % the Type is reachable via subsumes relationship
    	rdf_reachable(SType, tmr:'subsumes', Type) -> true
    ;   % or it satisfies the grouping criterias of SType
    	% of which at least one is effect
    	subsumesDueToEffect(SType, Type, _)
    ;
        subsumesViaGroupingCriterias(SType, Type, _)
    ).

propagGroupingCriteriaDrugToEventType :-
   forall(%conditions
    (
      rdf(DrugType, tmr:'hasGroupingCriteria', Type),
      rdf(EventType, tmr:'administrationOf', DrugType)
      ),%action 
	  rdf_assert(EventType, tmr:'hasGroupingCriteria', Type, 'http://anonymous.org/data')
      ).

subsumesDueToEffect(EventType1, EventType2, CBelief) :-
    rdf_global_id(EventType1, EvType1URI),
    rdf_global_id(EventType2, EvType2URI),
	rdfs_individual_of(EvType1URI, tmr:'EventType'),
	rdfs_individual_of(EvType2URI, tmr:'EventType'),
    rdf(EvType1URI, tmr:'hasGroupingCriteria', TrType),
    causes(EvType2URI, TrType, 'always', CBelief),
    EventType1 \= EventType2,
    \+ rdf(EvType1URI, owl:sameAs, EvType2URI),
    \+ rdf(EvType2URI, owl:sameAs, EvType1URI).
    %different(EventType1,EventType2).

subsumesViaGroupingCriterias(EventType1, EventType2, CBelief) :-
	rdfs_individual_of(EventType1, tmr:'EventType'),
	rdfs_individual_of(EventType2, tmr:'EventType'),
    rdf(EventType1, tmr:'hasGroupingCriteria', _),
    different(EventType1, EventType2),
    forall(
        % The grouping criterias can be both a promoted Effect or
        % a structural super-type (e.g. administration of non-steroidal)
        rdf(EventType1, tmr:'hasGroupingCriteria', Type),
        (causes(EventType2, Type, 'always', CBelief)
        ;(rdf(CatType, tmr:'hasGroupingCriteria', Type),
          rdf_reachable(CatType, tmr:'subsumes', EventType2))
        )).
    %EventType1 \= EventType2,
    %\+ rdf(EventType1, owl:sameAs, EventType2),
    %\+ rdf(EventType2, owl:sameAs, EventType1).

/* *********************************** */
%  BELIEF-BASED RULES
/* *********************************** */

causes(EventT1, EventT2, Frequency, CBelief) :-
    rdfs_individual_of(CBelief, tmr:'CausationBelief'),
    rdf(CBelief, tmr:'frequency', literal(Frequency), CBelief),
    rdf(EventT1, tmr:'causes', EventT2, CBelief).
causes(EventT1, EventT2, Frequency, CBelief, Source) :-
    causes(EventT1, EventT2, Frequency, CBelief),
    rdf(CBelief, prov:'wasDerivedFrom', Source).

similarTo(CBelief1, CBelief2) :-
    rdfs_individual_of(CBelief1, tmr:'CausationBelief'),
    rdfs_individual_of(CBelief2, tmr:'CausationBelief'),
    causes(EventT1, EventT3, Frequency, CBelief1),
    causes(EventT2, EventT3, Frequency, CBelief2),
    different(EventT1, EventT2).

incompatibleWith(EventT1, EventT2, IBelief) :-
    rdfs_individual_of(EventT1, tmr:'EventType'),
    rdfs_individual_of(EventT2, tmr:'EventType'),
    rdf(EventT1, tmr:'incompatibleWith', EventT2, IBelief).


/* *********************************** */
%  NORM-BASED RULES
/* *********************************** */

regulates(Reg, Norm, ActionT, Strength, CBelief) :-
    rdfs_individual_of(Norm, tmr:'Norm'),
    rdf(Norm, tmr:'partOf', Reg, Norm),
    rdf(Norm, tmr:'strength', literal(Strength), Norm),
    rdf(Norm, tmr:'basedOn', CBelief, Norm),
    rdf(Norm, tmr:'aboutExecutionOf', ActionT, Norm).


/* *********************************** */
%  ASSERTIONS
/* *********************************** */


/* *********************************** */
% ** Assert a causation belief
assertCausation2(CauseTURI, EffectTURI, Frequency, SourceURI) :-
	assertCausation2(CauseTURI, EffectTURI, Frequency, SourceURI, _).

assertCausation2(CauseTURI, EffectTURI, Frequency, SourceURI, NanopubURI) :-
    (   %check if causation is already asserted for the source
    	rdf(CauseTURI, tmr:'causes', EffectTURI, BeliefURI:_),
		rdf(BeliefURI, tmr:'frequency', literal(Frequency)),
		rdf(BeliefURI, prov:'wasDerivedFrom', SourceURI),
	    rdfs_individual_of(BeliefURI, tmr:'CausationBelief')  ->   true
    ;
        % composing new URI for nanopublication
        rdf_global_id(data:ID1, CauseTURI),
        rdf_global_id(data:ID2, EffectTURI),
        rdf_global_id(data:SourceID, SourceURI),
        concat_atom([SourceID, 'CB', ID1, ID2, Frequency], LabelID),
        rdf_global_id(data:'PrologRBS', AgentURI),
        % creating the Nanopublication
        assert_Nanopublication(LabelID, AgentURI, 'dataset_extraction', NanopubURI),
        % asserting the causation belief
    	rdf(NanopubURI, nanopub:'hasAssertion', AssertionURI),
        rdf_assert(AssertionURI, rdf:type, tmr:'CausationBelief', AssertionURI),
        rdf_assert(CauseTURI, tmr:'causes', EffectTURI, AssertionURI),
        rdf_assert(AssertionURI, tmr:'frequency', literal(Frequency), AssertionURI),
        % asserting the provenance
        assertProvSource(NanopubURI, SourceURI)
    ).

/* *********************************** */
% ** Assert an incompatibility belief
assertIncompatibility(EventTURI1, EventTURI2, SourceURI) :-
	assertIncompatibility(EventTURI1, EventTURI2, SourceURI, _).

assertIncompatibility(EventTURI1, EventTURI2, SourceURI, NanopubURI) :-
    (   %check if incompatibility is already asserted for the source
    	(rdf(EventTURI1, tmr:'incompatibleWith', EventTURI2, Belief:_);
         rdf(EventTURI2, tmr:'incompatibleWith', EventTURI1, Belief:_)),
		rdf(Belief, prov:'wasDerivedFrom', SourceURI),
        rdf_global_id(tmr:'IncompatibilityBelief', IBType),
   		instanceOf(Belief, IBType)  ->   true
    ;
        % composing new URI for belief
        rdf_global_id(data:ID1, EventTURI1),
        rdf_global_id(data:ID2, EventTURI2),
        rdf_global_id(data:SourceID, SourceURI),
        concat_atom([SourceID, 'IB', ID1, ID2], LabelID),
        rdf_global_id(data:'PrologRBS', AgentURI),
        % creating the Nanopublication
        assert_Nanopublication(LabelID, AgentURI, 'dataset_extraction', NanopubURI),
        % asserting the incompatibility belief
    	rdf(NanopubURI, nanopub:'hasAssertion', AssertionURI),
        rdf_assert(AssertionURI, rdf:type, tmr:'IncompatibilityBelief', AssertionURI),
        rdf_assert(EventTURI1, tmr:'incompatibleWith', EventTURI2, AssertionURI),
        rdf_assert(AssertionURI, tmr:'isAbout', EventTURI1, AssertionURI),
        rdf_assert(AssertionURI, tmr:'isAbout', EventTURI2, AssertionURI),
        % asserting the provenance
        assertProvSource(NanopubURI, SourceURI)
    ).

assertIncompatibilityMultiEvents(EventTypeListURI, SourceURI) :-
	(   rdf(Belief, prov:'wasDerivedFrom', SourceURI),
		rdfs_individual_of(Belief, tmr:'IncompatibilityBelief'),
    	foreach(member((EventTypeURI,_), EventTypeListURI),
			rdf(Belief, tmr:'isAbout', EventTypeURI, Belief:_)) -> true
    ;
    	% >>> make a name for the belief: compose all events' names??
        rdf_global_id(data:SourceID, SourceURI),
        random_between(0,1000000,R),
        concat_atom([SourceID, 'IB', R], LabelID),
        rdf_global_id(data:'PrologRBS', AgentURI),
        % creating the Nanopublication
        assert_Nanopublication(LabelID, AgentURI, 'dataset_extraction', NanopubURI),
        % asserting the incompatibility belief
    	rdf(NanopubURI, nanopub:'hasAssertion', AssertionURI),
        rdf_assert(AssertionURI, rdf:type, tmr:'IncompatibilityBelief', AssertionURI),
    	foreach(member((EventTypeURI,_), EventTypeListURI),
                rdf_assert(AssertionURI, tmr:'isAbout', EventTypeURI, AssertionURI)),        % asserting the provenance
    	foreach(member((_,ExtResource), EventTypeListURI),
                assertProvResourceUsed(NanopubURI, ExtResource)),
        assertProvSource(NanopubURI, SourceURI)
    ).

%% meant for assertion the causation from interaction in LIDDI
updateIncompatibility2(EventTURI1, EventTURI2, SituationURI, SourceURI) :-
    (   %check if incompatibility is already asserted for the source
    	(rdf(EventTURI1, tmr:'incompatibleWith', EventTURI2, Belief:_);
         rdf(EventTURI2, tmr:'incompatibleWith', EventTURI1, Belief:_)),
		rdf(Belief, prov:'wasDerivedFrom', SourceURI),
   		rdfs_individual_of(Belief, tmr:'IncompatibilityBelief')  ->   true
    ;
        % composing new URI for belief
        rdf_global_id(data:ID1, EventTURI1),
        rdf_global_id(data:ID2, EventTURI2),
        rdf_global_id(data:SourceID, SourceURI),
        concat_atom([SourceID, 'IB', ID1, ID2], LabelID),
        rdf_global_id(data:'PrologRBS', AgentURI),
        % creating the Nanopublication
        assert_Nanopublication(LabelID, AgentURI, 'dataset_extraction', NanopubURI),
        % asserting the incompatibility belief
    	rdf(NanopubURI, nanopub:'hasAssertion', AssertionURI),
        rdf_assert(AssertionURI, rdf:type, tmr:'IncompatibilityBelief', AssertionURI),
        rdf_assert(EventTURI1, tmr:'incompatibleWith', EventTURI2, AssertionURI),
        % asserting the provenance
        assertProvSource(NanopubURI, SourceURI),
        % asserting the causation for the incompatibility belief
        % if the composed action does not exist, create it
        ( ( rdf(EventTURI1, tmr:'partOf', ComposedActionURI),
            rdf(EventTURI2, tmr:'partOf', ComposedActionURI)
          );
          ( concat_atom([ID1, ID2], ComposedActionID),
	        rdf_global_id(data:ComposedActionID, ComposedActionURI),
            rdf_assert(EventTURI1, tmr:'partOf', ComposedActionURI),
          	rdf_assert(EventTURI2, tmr:'partOf', ComposedActionURI)
          )
        ),
        % assert a transition that has post situation SituationURI
        rdf_global_id(data:SituationID, SituationURI),
       	concat_atom(['TrIB', ID1, ID2, SituationID, SourceID], EffectID),
        rdf_global_id(data:EffectID, EffectTURI),
    	% assert a causation
        rdf_assert(AssertionURI, rdf:type, tmr:'CausationBelief', AssertionURI),
        rdf_assert(ComposedActionURI, tmr:'causes', EffectTURI, AssertionURI),
        rdf_assert(EffectTURI, tmr:'hasExpectedSituation', SituationURI, AssertionURI)
    ).


assertTransition(TransformableSituation, ExpectedSituation, Transition) :-
    (   rdf(Transition, tmr:'hasTransformableSituation', TransformableSituation),
		rdf(Transition, tmr:'hasExpectedSituation', ExpectedSituation) ->   true
    ;
        % composing new URI for transition
		(rdf_global_id(data:ID1, TransformableSituation),
        rdf_global_id(data:ID2, ExpectedSituation),
        concat_atom(['Tr', ID1, ID2], NewID),
        rdf_global_id(data:NewID, Transition),
        % asserting the transition
        rdf_assert(Transition, rdf:type, tmr:'TransitionType'),
        rdf_assert(Transition, tmr:'hasTransformableSituation', TransformableSituation),
        rdf_assert(Transition, tmr:'hasExpectedSituation', ExpectedSituation))
    ).

assertPartialTransition(ExpectedSituation, Transition) :-
    (   % composing new URI for transition
		rdf_global_id(data:ID2, ExpectedSituation),
        concat_atom(['TrNone', ID2], NewID),
        rdf_global_id(data:NewID, Transition),
        % asserting the transition
        rdf_assert(Transition, rdf:type, tmr:'TransitionType'),
        rdf_assert(Transition, tmr:'hasExpectedSituation', ExpectedSituation)
    ).

%propagCausation :-
%:- forall(
%      rdf(EventType, tmr:'hasGroupingCriteria', TrType),
%	  assertCausation(EventType, TrType, 'always', 'None')).
