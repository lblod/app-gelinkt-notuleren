alias Acl.Accessibility.Always, as: AlwaysAccessible
alias Acl.Accessibility.ByQuery, as: AccessByQuery
alias Acl.GraphSpec.Constraint.Resource.AllPredicates, as: AllPredicates
alias Acl.GraphSpec.Constraint.Resource.NoPredicates, as: NoPredicates
alias Acl.GraphSpec.Constraint.ResourceFormat, as: ResourceFormatConstraint
alias Acl.GraphSpec.Constraint.Resource, as: ResourceConstraint
alias Acl.GraphSpec, as: GraphSpec
alias Acl.GroupSpec, as: GroupSpec
alias Acl.GroupSpec.GraphCleanup, as: GraphCleanup

defmodule Acl.UserGroups.Config do
  defp access_by_role( group_string ) do
    %AccessByQuery{
      vars: ["session_group","session_role"],
      query: sparql_query_for_access_role( group_string ) }
  end

  defp sparql_query_for_access_role( group_string ) do
    "PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
    PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
    SELECT ?session_group ?session_role WHERE {
      <SESSION_ID> ext:sessionGroup/mu:uuid ?session_group;
                   ext:sessionRole ?session_role.
      FILTER( ?session_role = \"#{group_string}\" )
    }"
  end

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
                        "http://mu.semte.ch/vocabularies/ext/EditorDocumentStatus",
                        "http://mu.semte.ch/vocabularies/ext/Template",
                        "http://www.w3.org/2000/01/rdf-schema#Class",
                        "http://www.w3.org/2000/01/rdf-schema#Property",
                        "http://mu.semte.ch/vocabularies/ext/Task",
                        "http://mu.semte.ch/vocabularies/ext/Tasklist",
                        "http://mu.semte.ch/vocabularies/ext/BestuursorgaanClassificatieCode",
                        "http://mu.semte.ch/vocabularies/ext/BestuurseenheidClassificatieCode",
                        "http://mu.semte.ch/vocabularies/ext/BestuursfunctieCode",
                        "http://mu.semte.ch/vocabularies/ext/MandatarisStatusCode",
                        "http://mu.semte.ch/vocabularies/ext/BeleidsdomeinCode",
                        "http://mu.semte.ch/vocabularies/ext/GeslachtCode",
                        "http://mu.semte.ch/vocabularies/ext/KandidatenlijstType",
                        "http://mu.semte.ch/vocabularies/ext/VerkiezingsresultaatGevolgCode",
                        "http://mu.semte.ch/vocabularies/ext/Fractietype",
                        "http://data.vlaanderen.be/ns/besluit#Bestuursorgaan",
                        "http://data.vlaanderen.be/ns/besluit#Bestuurseenheid",
                        "http://www.w3.org/ns/prov#Location",
                        "http://data.vlaanderen.be/ns/mandaat#Kandidatenlijst",
                        "http://data.vlaanderen.be/ns/mandaat#Verkiezingsresultaat",
                        "http://data.vlaanderen.be/ns/besluit#Zitting",
                        "http://data.vlaanderen.be/ns/besluit#Stemming",
                        "http://data.vlaanderen.be/ns/besluit#Besluit",
                        "http://data.vlaanderen.be/ns/besluit#Artikel",
                        "http://data.vlaanderen.be/ns/besluit#Agenda",
                        "http://data.vlaanderen.be/ns/besluit#Agendapunt",
                        "http://data.vlaanderen.be/ns/besluit#BehandelingVanAgendapunt",
                        "http://data.vlaanderen.be/ns/mandaat#Mandaat",
                        "http://data.vlaanderen.be/ns/mandaat#Mandataris",
                        "http://data.vlaanderen.be/ns/mandaat#Fractie",
                        "http://purl.org/dc/terms/PeriodOfTime",
                        "http://www.w3.org/ns/org#Membership",
                        "http://mu.semte.ch/vocabularies/ext/signing/BlockchainStatus",
                      ]
                    } },
                  %GraphSpec{
                    graph: "http://mu.semte.ch/graphs/sessions",
                    constraint: %ResourceFormatConstraint{
                      resource_prefix: "http://mu.semte.ch/sessions/"
                    } } ] },
      # %GroupSpec{
      #   name: "otherpublic",
      #   useage: [:read],
      #   access: %AlwaysAccessible{}, # TODO: Should be only for logged in users
      #   graphs: [ %GraphSpec{
      #               graph: "http://mu.semte.ch/graphs/sessions",
      #               constraint: %ResourceFormatConstraint{
      #                 resource_prefix: "http://mu.semte.ch/sessions/"
      #               } } ] },

      # %GroupSpec{
      #   name: "public-wf",
      #   useage: [:write, :read_for_write],
      #   access: %AlwaysAccessible{}, # TODO: Should be only for logged in users
      #   graphs: [%GraphSpec{
      #               graph: "http://mu.semte.ch/graphs/public",
      #               constraint: %ResourceConstraint{
      #                 resource_types: [

      #                 ]
      #               } } ] },

      # // ORGANIZATION HAS POSSIBLY DUPLICATE USER DATA
      %GroupSpec{
        name: "org",
        useage: [:read],
        access: %AccessByQuery{
          vars: ["session_group"],
          query: "PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
                  PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
                  SELECT ?session_group ?session_role WHERE {
                    <SESSION_ID> ext:sessionGroup/mu:uuid ?session_group.
                    }" },
        graphs: [ %GraphSpec{
                    graph: "http://mu.semte.ch/graphs/organizations/",
                    constraint: %ResourceConstraint{
                      resource_types: [
                        "http://mu.semte.ch/vocabularies/ext/EditorDocument",
                        "http://mu.semte.ch/vocabularies/ext/DocumentContainer",
                        "http://mu.semte.ch/vocabularies/ext/VersionedAgenda",
                        "http://mu.semte.ch/vocabularies/ext/VersionedNotulen",
                        "http://www.w3.org/ns/person#Person",
                        "http://data.vlaanderen.be/ns/persoon#Geboorte",
                        "http://www.w3.org/ns/adms#Identifier",
                        "http://xmlns.com/foaf/0.1/Person",
                        "http://xmlns.com/foaf/0.1/OnlineAccount",
                        "http://mu.semte.ch/vocabularies/ext/TaskSolution",
                        "http://mu.semte.ch/vocabularies/ext/TasklistSolution",
                        "http://mu.semte.ch/vocabularies/ext/signing/SignedResource",
                        "http://mu.semte.ch/vocabularies/ext/signing/PublishedResource",
                      ] } } ] },

        %GroupSpec{
        name: "org-temp",
        useage: [:read],
        access: %AccessByQuery{
          vars: ["session_group"],
          query: "PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
                  PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
                  SELECT ?session_group ?session_role WHERE {
                    <SESSION_ID> ext:sessionGroup/mu:uuid ?session_group.
                    }" },
        graphs: [ %GraphSpec{
                    graph: "http://mu.semte.ch/graphs/temporary-sync-",
                    constraint: %ResourceConstraint{
                      resource_types: [
                        "http://data.vlaanderen.be/ns/besluit#Zitting",
                        "http://data.vlaanderen.be/ns/besluit#Stemming",
                        "http://data.vlaanderen.be/ns/besluit#Besluit",
                        "http://data.vlaanderen.be/ns/besluit#Artikel",
                        "http://data.vlaanderen.be/ns/besluit#Agenda",
                        "http://data.vlaanderen.be/ns/besluit#Agendapunt",
                        "http://data.vlaanderen.be/ns/besluit#BehandelingVanAgendapunt",
                        "http://data.vlaanderen.be/ns/mandaat#Mandaat",
                        "http://data.vlaanderen.be/ns/mandaat#Mandataris",
                        "http://data.vlaanderen.be/ns/mandaat#Fractie",
                      ] } } ] },

      %GroupSpec{
        name: "org-wf",
        useage: [:write, :read_for_write],
        access: %AccessByQuery{
          vars: ["session_group"],
          query: "PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
                  PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
                  SELECT ?session_group ?session_role WHERE {
                    <SESSION_ID> ext:sessionGroup/mu:uuid ?session_group.
                    }" },
        graphs: [ %GraphSpec{
                    graph: "http://mu.semte.ch/graphs/organizations/",
                    constraint: %ResourceConstraint{
                      resource_types: [
                        "http://mu.semte.ch/vocabularies/ext/EditorDocument",
                        "http://mu.semte.ch/vocabularies/ext/DocumentContainer",
                        "http://mu.semte.ch/vocabularies/ext/VersionedAgenda",
                        "http://mu.semte.ch/vocabularies/ext/VersionedNotulen",
                        "http://mu.semte.ch/vocabularies/ext/TaskSolution",
                        "http://mu.semte.ch/vocabularies/ext/TasklistSolution",
                        "http://www.w3.org/ns/person#Person",
                        "http://data.vlaanderen.be/ns/persoon#Geboorte",
                        "http://www.w3.org/ns/adms#Identifier",
                        "http://mu.semte.ch/vocabularies/ext/signing/SignedResource",
                        "http://mu.semte.ch/vocabularies/ext/signing/PublishedResource",
                      ] } } ] },


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
