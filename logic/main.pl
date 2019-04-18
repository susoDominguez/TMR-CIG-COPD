:- use_module(library(semweb/turtle)).
:- use_module(library(semweb/rdf_http_plugin)).
:- use_module(library('semweb/rdf_db')).
:- use_module(library('semweb/rdfs')).

:- rdf_load('https://raw.githubusercontent.com/veruskacz/CG-RDF/master/schema/model.ttl', [format('turtle'), register_namespaces(false), base_uri('http://anonymous.org/vocab/'), graph('http://anonymous.org/vocab')]).
:- rdf_load('https://raw.githubusercontent.com/veruskacz/CG-RDF/master/schema/model4I%203.0.ttl', [format('turtle'), register_namespaces(false), base_uri('http://anonymous.org/vocab4i/'), graph('http://anonymous.org/vocab4i')]).

:- rdf_load('https://raw.githubusercontent.com/veruskacz/CG-RDF/master/instance/CareAction%26DrugTypes.ttl', [format('turtle'), register_namespaces(false), base_uri('http://anonymous.org/data/'), graph('http://anonymous.org/CareAction&DrugTypes')]).
:- rdf_load('https://raw.githubusercontent.com/veruskacz/CG-RDF/master/instance/Transition%26SituationTypes.ttl', [format('turtle'), register_namespaces(false), base_uri('http://anonymous.org/data/'), graph('http://anonymous.org/Transition&SituationTypes')]).
:- rdf_load('https://raw.githubusercontent.com/veruskacz/CG-RDF/master/instance/CausationBeliefs-Nanopub.trig', [format('trig'), register_namespaces(false), base_uri('http://anonymous.org/data/'), graph('http://anonymous.org/CausationBeliefs-Nanopub')]).
:- rdf_load('https://raw.githubusercontent.com/veruskacz/CG-RDF/master/instance/Reg%26Norms-Nanopub.trig', [format('trig'), register_namespaces(false), base_uri('http://anonymous.org/data/'), graph('http://anonymous.org/Reg&Norms-Nanopub')]).
:- rdf_load('https://raw.githubusercontent.com/veruskacz/CG-RDF/master/instance/MergedReg%26Norms-Nanopub.trig', [format('trig'), register_namespaces(false), base_uri('http://anonymous.org/data/'), graph('http://anonymous.org/MergedReg&Norms-Nanopub')]).

:- include(interactionRules).

:- inferInternalInteractions.

