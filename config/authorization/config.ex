alias Acl.Accessibility.Always, as: AlwaysAccessible
alias Acl.Accessibility.ByQuery, as: AccessByQuery
alias Acl.GraphSpec.Constraint.ResourceFormat, as: ResourceFormatConstraint
alias Acl.GraphSpec.Constraint.Resource, as: ResourceConstraint
alias Acl.GraphSpec, as: GraphSpec
alias Acl.GroupSpec, as: GroupSpec
alias Acl.GroupSpec.GraphCleanup, as: GraphCleanup
alias Acl.GraphSpec.Constraint.Resource.NoPredicates, as: NoPredicates
alias Acl.GraphSpec.Constraint.Resource.AllPredicates, as: AllPredicates

defmodule Acl.UserGroups.Config do
  def user_groups do
    # These elements are walked from top to bottom.  Each of them may
    # alter the quads to which the current query applies.  Quads are
    # represented in three sections: current_source_quads,
    # removed_source_quads, new_quads.  The quads may be calculated in
    # many ways.  The useage of a GroupSpec and GraphCleanup are
    # common.
    [
      # // PUBLIC
      %GroupSpec{
        name: "public",
        useage: [:read,:read_for_write],
        access: %AlwaysAccessible{}, # TODO: Should be only for logged in users
        graphs: [ %GraphSpec{
                    graph: "http://mu.semte.ch/graphs/public",
                    constraint: %ResourceConstraint{
                      resource_types: [
                        "http://data.lblod.info/vocabularies/leidinggevenden/Bestuursfunctie",
                        "http://data.lblod.info/vocabularies/leidinggevenden/Functionaris",
                        "http://data.lblod.info/vocabularies/leidinggevenden/FunctionarisStatusCode",
                        "http://data.lblod.info/vocabularies/mobiliteit/MaatregelConcept",
                        "http://data.lblod.info/vocabularies/mobiliteit/Verkeersbordcombinatie",
                        "http://data.lblod.info/vocabularies/mobiliteit/VerkeersbordconceptStatusCode",
                        "http://data.vlaanderen.be/ns/besluit#Artikel",
                        "http://data.vlaanderen.be/ns/besluit#Besluit",
                        "http://data.vlaanderen.be/ns/besluit#Bestuurseenheid",
                        "http://data.vlaanderen.be/ns/besluit#Bestuursorgaan",
                        "http://data.vlaanderen.be/ns/mandaat#Fractie",
                        "http://data.vlaanderen.be/ns/mandaat#Kandidatenlijst",
                        "http://data.vlaanderen.be/ns/mandaat#Mandaat",
                        "http://data.vlaanderen.be/ns/mandaat#Mandataris",
                        "http://data.vlaanderen.be/ns/mandaat#RechtstreekseVerkiezing",
                        "http://data.vlaanderen.be/ns/mandaat#Verkiezingsresultaat",
                        "http://mu.semte.ch/vocabularies/ext/BeleidsdomeinCode",
                        "http://mu.semte.ch/vocabularies/ext/BestuurseenheidClassificatieCode",
                        "http://mu.semte.ch/vocabularies/ext/BestuursfunctieCode",
                        "http://mu.semte.ch/vocabularies/ext/BestuursorgaanClassificatieCode",
                        "http://mu.semte.ch/vocabularies/ext/EditorDocumentFolder",
                        "http://mu.semte.ch/vocabularies/ext/EditorDocumentStatus",
                        "http://mu.semte.ch/vocabularies/ext/GeslachtCode",
                        "http://mu.semte.ch/vocabularies/ext/KandidatenlijstLijsttype",
                        "http://mu.semte.ch/vocabularies/ext/MandatarisStatusCode",
                        "http://mu.semte.ch/vocabularies/ext/SyncTask",
                        "http://mu.semte.ch/vocabularies/ext/Task",
                        "http://mu.semte.ch/vocabularies/ext/Tasklist",
                        "http://mu.semte.ch/vocabularies/ext/Template",
                        "http://mu.semte.ch/vocabularies/ext/VerkiezingsresultaatGevolgCode",
                        "http://mu.semte.ch/vocabularies/ext/signing/BlockchainStatus",
                        "http://purl.org/dc/terms/PeriodOfTime",
                        "http://schema.org/ContactPoint",
                        "http://schema.org/PostalAddress",
                        "http://www.semanticdesktop.org/ontologies/2007/03/22/nfo#FileDataObject",
                        "http://www.w3.org/2000/01/rdf-schema#Class",
                        "http://www.w3.org/2000/01/rdf-schema#Property",
                        "http://www.w3.org/2004/02/skos/core#Concept",
                        "http://www.w3.org/2004/02/skos/core#ConceptScheme",
                        "http://www.w3.org/ns/locn#Address",
                        "http://www.w3.org/ns/org#Membership",
                        "http://www.w3.org/ns/org#Organization",
                        "http://www.w3.org/ns/org#Post",
                        "http://www.w3.org/ns/org#Role",
                        "http://www.w3.org/ns/org#Site",
                        "http://www.w3.org/ns/person#Person",
                        "http://www.w3.org/ns/prov#Location",
                        "http://xmlns.com/foaf/0.1/Image",
                        "http://xmlns.com/foaf/0.1/OnlineAccount",
                        "http://xmlns.com/foaf/0.1/Person",
                        "https://data.vlaanderen.be/ns/mobiliteit#Verkeersbordcategorie",
                        "https://data.vlaanderen.be/ns/mobiliteit#Verkeersbordconcept",
                        "https://data.vlaanderen.be/ns/mobiliteit#VerkeersbordconceptStatus"
                      ]
                    } },
                  %GraphSpec{
                    graph: "http://mu.semte.ch/graphs/sessions",
                    constraint: %ResourceFormatConstraint{
                      resource_prefix: "http://mu.semte.ch/sessions/"
                    } } ] },

      # // ORGANIZATION HAS POSSIBLY DUPLICATE USER DATA
      %GroupSpec{
        name: "org",
        useage: [:read],
        access: %AccessByQuery{
          vars: ["session_group"],
          query: "PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
                  PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
                  SELECT DISTINCT ?session_group WHERE {
                    <SESSION_ID> ext:sessionGroup/mu:uuid ?session_group;
                                 ext:sessionRole ?role.
                     FILTER(?role in (\"GelinktNotuleren-lezer\",\"GelinktNotuleren-schrijver\", \"GelinktNotuleren-publiceerder\",  \"GelinktNotuleren-ondertekenaar\"))
                    }" },
        graphs: [ %GraphSpec{
                    graph: "http://mu.semte.ch/graphs/organizations/",
                    constraint: %ResourceConstraint{
                      resource_types: [
                        "http://www.semanticdesktop.org/ontologies/2007/03/22/nfo#FileDataObject",
                        "http://mu.semte.ch/vocabularies/ext/Attachment",
                        "http://data.vlaanderen.be/ns/besluitvorming#Agenda",
                        "http://mu.semte.ch/vocabularies/ext/EditorDocument",
                        "http://data.vlaanderen.be/ns/besluit#Zitting",
                        "http://mu.semte.ch/vocabularies/ext/Intermission",
                        "http://mu.semte.ch/vocabularies/ext/AgendaPosition",
                        "http://mu.semte.ch/vocabularies/ext/Agenda",
                        "http://data.vlaanderen.be/ns/besluit#Agendapunt",
                        "http://data.vlaanderen.be/ns/besluit#BehandelingVanAgendapunt",
                        "http://data.vlaanderen.be/ns/besluit#Stemming",
                        "http://mu.semte.ch/vocabularies/ext/DocumentContainer",
                        "http://mu.semte.ch/vocabularies/ext/VersionedAgenda",
                        "http://mu.semte.ch/vocabularies/ext/VersionedNotulen",
                        "http://mu.semte.ch/vocabularies/ext/VersionedBesluitenLijst",
                        "http://mu.semte.ch/vocabularies/ext/VersionedBehandeling",
                        "http://mu.semte.ch/vocabularies/ext/VersionedRegulatoryStatement",
                        "http://redpencil.data.gift/vocabularies/tasks/Task",
                        "http://mu.semte.ch/vocabularies/ext/TaskSolution",
                        "http://mu.semte.ch/vocabularies/ext/TasklistSolution",
                        "http://www.w3.org/ns/person#Person",
                        "http://data.vlaanderen.be/ns/persoon#Geboorte",
                        "http://www.w3.org/ns/adms#Identifier",
                        "http://mu.semte.ch/vocabularies/ext/signing/SignedResource",
                        "http://mu.semte.ch/vocabularies/ext/signing/PublishedResource",
                        "http://mu.semte.ch/vocabularies/ext/PublishingLog",
                        "http://mu.semte.ch/vocabularies/ext/Installatievergadering",
                        "http://mu.semte.ch/vocabularies/ext/InstallatievergaderingSynchronizationStatus",

                        # Hackathon
                        "http://dbpedia.org/resource/Case",
                        "https://data.vlaanderen.be/ns/omgevingsvergunning#Activiteit",
                        "https://data.vlaanderen.be/ns/omgevingsvergunning#Aanvraag",
                        "http://www.opengis.net/ont/geosparql#Feature",
                        "http://www.w3.org/2006/time#Interval"
                      ] } } ] },

      %GroupSpec{
        name: "org-wf",
        useage: [:write, :read_for_write],
        access: %AccessByQuery{
          vars: ["session_group"],
          query: "PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
                  PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
                  SELECT DISTINCT ?session_group WHERE {
                    <SESSION_ID> ext:sessionGroup/mu:uuid ?session_group;
                                 ext:sessionRole \"GelinktNotuleren-schrijver\".
                    }" },
        graphs: [ %GraphSpec{
                    graph: "http://mu.semte.ch/graphs/organizations/",
                    constraint: %ResourceConstraint{
                      resource_types: [
                        "http://www.semanticdesktop.org/ontologies/2007/03/22/nfo#FileDataObject",
                        "http://mu.semte.ch/vocabularies/ext/Attachment",
                        "http://mu.semte.ch/vocabularies/ext/EditorDocument",
                        "http://data.vlaanderen.be/ns/besluit#Zitting",
                        "http://mu.semte.ch/vocabularies/ext/Intermission",
                        "http://mu.semte.ch/vocabularies/ext/AgendaPosition",
                        "http://mu.semte.ch/vocabularies/ext/Agenda",
                        "http://data.vlaanderen.be/ns/besluit#Agendapunt",
                        "http://data.vlaanderen.be/ns/besluit#BehandelingVanAgendapunt",
                        "http://data.vlaanderen.be/ns/besluit#Stemming",
                        "http://mu.semte.ch/vocabularies/ext/DocumentContainer",
                        "http://mu.semte.ch/vocabularies/ext/TaskSolution",
                        "http://mu.semte.ch/vocabularies/ext/TasklistSolution",
                        "http://redpencil.data.gift/vocabularies/tasks/Task",
                        "http://www.w3.org/ns/person#Person",
                        "http://data.vlaanderen.be/ns/persoon#Geboorte",
                        "http://www.w3.org/ns/adms#Identifier",
                        "http://mu.semte.ch/vocabularies/ext/PublishingLog",
                        "http://mu.semte.ch/vocabularies/ext/Installatievergadering",
                        "http://mu.semte.ch/vocabularies/ext/InstallatievergaderingSynchronizationStatus",

                        # Hackathon
                        "http://dbpedia.org/resource/Case",
                        "https://data.vlaanderen.be/ns/omgevingsvergunning#Activiteit",
                        "https://data.vlaanderen.be/ns/omgevingsvergunning#Aanvraag",
                        "http://www.opengis.net/ont/geosparql#Feature",
                        "http://www.w3.org/2006/time#Interval"
                      ] } } ] },

      %GroupSpec{
        name: "org-publish",
        useage: [:write, :read_for_write],
        access: %AccessByQuery{
          vars: ["session_group"],
          query: "PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
                  PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
                  SELECT DISTINCT ?session_group WHERE {
                    <SESSION_ID> ext:sessionGroup/mu:uuid ?session_group;
                                 ext:sessionRole \"GelinktNotuleren-publiceerder\".
                    }" },
        graphs: [ %GraphSpec{
                    graph: "http://mu.semte.ch/graphs/organizations/",
                    constraint: %ResourceConstraint{
                      resource_types: [
                        "http://data.vlaanderen.be/ns/besluitvorming#Agenda",
                        "http://mu.semte.ch/vocabularies/ext/VersionedAgenda",
                        "http://mu.semte.ch/vocabularies/ext/VersionedNotulen",
                        "http://mu.semte.ch/vocabularies/ext/VersionedBesluitenLijst",
                        "http://mu.semte.ch/vocabularies/ext/VersionedBehandeling",
                        "http://mu.semte.ch/vocabularies/ext/VersionedRegulatoryStatement",
                        "http://mu.semte.ch/vocabularies/ext/signing/PublishedResource",
                        "http://redpencil.data.gift/vocabularies/tasks/Task",
                        "http://mu.semte.ch/vocabularies/ext/DocumentContainer", # needed to update status on publishing decision/notulen
                      ],
                      inverse_predicates: %AllPredicates{}
                      } } ] },

      %GroupSpec{
        name: "org-sign",
        useage: [:write, :read_for_write],
        access: %AccessByQuery{
          vars: ["session_group"],
          query: "PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
                  PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
                  SELECT DISTINCT ?session_group WHERE {
                    <SESSION_ID> ext:sessionGroup/mu:uuid ?session_group;
                                 ext:sessionRole \"GelinktNotuleren-ondertekenaar\".
                    }" },
        graphs: [ %GraphSpec{
                    graph: "http://mu.semte.ch/graphs/organizations/",
                    constraint: %ResourceConstraint{
                      resource_types: [
                        "http://data.vlaanderen.be/ns/besluitvorming#Agenda",
                        "http://mu.semte.ch/vocabularies/ext/VersionedAgenda",
                        "http://mu.semte.ch/vocabularies/ext/VersionedNotulen",
                        "http://mu.semte.ch/vocabularies/ext/VersionedBesluitenLijst",
                        "http://mu.semte.ch/vocabularies/ext/VersionedBehandeling",
                        "http://mu.semte.ch/vocabularies/ext/signing/SignedResource",
                        "http://redpencil.data.gift/vocabularies/tasks/Task",
                        "http://mu.semte.ch/vocabularies/ext/PublishingLog",
                      ],
                      inverse_predicates: %AllPredicates{}
                    } } ] },
      %GroupSpec{
        name: "lmb-public",
        useage: [:read,:read_for_write],
        access: %AlwaysAccessible{}, # TODO: Should be only for logged in users
        graphs: [ %GraphSpec{
                    graph: "http://mu.semte.ch/graphs/lmb-data-public",
                    constraint: %ResourceConstraint{
                      resource_types: [
                        "http://data.vlaanderen.be/ns/mandaat#Mandataris",
                        "http://data.vlaanderen.be/ns/mandaat#Fractie",
                        "http://www.w3.org/ns/org#Membership",
                        "http://www.w3.org/ns/person#Person",
                      ]
                    }
        }]
      },
      %GroupSpec{
        name: "lmb-private",
        useage: [:read,:read_for_write],
        access: %AccessByQuery{
          vars: ["session_group"],
          query: "PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
                  PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
                  SELECT DISTINCT ?session_group WHERE {
                    <SESSION_ID> ext:sessionGroup/mu:uuid ?session_group;
                                 ext:sessionRole ?role.
                     FILTER(?role in (\"GelinktNotuleren-lezer\",\"GelinktNotuleren-schrijver\", \"GelinktNotuleren-publiceerder\",  \"GelinktNotuleren-ondertekenaar\"))
                    }" },
        graphs: [ %GraphSpec{
                    graph: "http://mu.semte.ch/graphs/lmb-data-private/",
                    constraint: %ResourceConstraint{
                      resource_types: [
                        "http://www.w3.org/ns/person#Person",
                        "http://data.vlaanderen.be/ns/persoon#Geboorte",
                        "http://www.w3.org/ns/adms#Identifier",
                      ]
                    }
        }]
      },
      %GroupSpec{
        name: "org-reports",
        useage: [:read],
        access: %AccessByQuery{
          vars: [],
          query: "PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
                  PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
                  PREFIX session: <http://mu.semte.ch/vocabularies/session/>
                  SELECT DISTINCT * WHERE {
                    <SESSION_ID> session:account/ext:sessionRole \"GelinktNotuleren-report-admin\".
                    }"},
        graphs: [ %GraphSpec{
                    graph: "http://mu.semte.ch/graphs/reports",
                    constraint: %ResourceConstraint{
                      resource_types: [
                        # For logs, reports (dashboard and report service)
                        "http://persistence.uni-leipzig.org/nlp2rdf/ontologies/rlog#Entry",
                        "http://persistence.uni-leipzig.org/nlp2rdf/ontologies/rlog#Level",
                        "http://persistence.uni-leipzig.org/nlp2rdf/ontologies/rlog#StatusCode",
                        "http://mu.semte.ch/vocabularies/ext/LogSource",
                        "http://lblod.data.gift/vocabularies/reporting/Report",
                        "http://vocab.deri.ie/cogs#Job",
                        "http://open-services.net/ns/core#Error",
                        "http://www.semanticdesktop.org/ontologies/2007/03/22/nfo#DataContainer",
                        "http://www.semanticdesktop.org/ontologies/2007/03/22/nfo#FileDataObject",
                        "http://www.semanticdesktop.org/ontologies/2007/03/22/nfo#DataContainer"
                      ]}}]},
      # // CLEANUP
      #
      %GraphCleanup{
        originating_graph: "http://mu.semte.ch/application",
        useage: [:write],
        name: "clean"
      }
    ]
  end
end
