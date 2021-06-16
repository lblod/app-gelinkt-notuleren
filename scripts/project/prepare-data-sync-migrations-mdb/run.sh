#!/bin/bash
export="$1"
tstamp=$(date +%Y%m%d%H%M%S)
dir="/project/config/migrations/$tstamp-data-sync-mdb"
mkdir -p "$dir"
cat "/project/$export" >> "$dir/$tstamp-export.ttl"
cat <<EOF > "$dir/$tstamp-export.graph"
http://mu.semte.ch/graphs/temp-ingest-graph
EOF
((tstamp++))
cat <<EOF > "$dir/$tstamp-ingest-exported-triples-1.sparql"
DELETE {
    GRAPH <http://mu.semte.ch/graphs/public> {
        ?s ?p ?o .
    }
} WHERE {
    GRAPH <http://mu.semte.ch/graphs/public> {
        ?s a ?type ; ?p ?o .
        FILTER (?type IN (
            <http://mu.semte.ch/vocabularies/ext/MandatarisStatusCode>,
            <http://data.vlaanderen.be/ns/mandaat#Mandataris>,
            <http://data.vlaanderen.be/ns/mandaat#Mandaat>
            ))
    }
}
;
INSERT {
    GRAPH <http://mu.semte.ch/graphs/public> {
        ?s ?p ?o .
    }
} WHERE {
    GRAPH <http://mu.semte.ch/graphs/temp-ingest-graph> {
        ?s a ?type ; ?p ?o .
        FILTER (?type IN (
            <http://mu.semte.ch/vocabularies/ext/MandatarisStatusCode>,
            <http://data.vlaanderen.be/ns/mandaat#Mandataris>,
            <http://data.vlaanderen.be/ns/mandaat#Mandaat>
            ))
    }
}
;
DELETE {
    GRAPH <http://mu.semte.ch/graphs/public> {
        ?s ?p ?old .
    }
} INSERT{
    GRAPH <http://mu.semte.ch/graphs/public> {
        ?s ?p ?new .
    }
} WHERE {
    GRAPH <http://mu.semte.ch/graphs/temp-ingest-graph> {
        ?s a ?type ; ?p ?new .
        FILTER (?type IN (
            <http://mu.semte.ch/vocabularies/ext/BeleidsdomeinCode>,
            <http://www.w3.org/ns/org#Membership>,
            <http://data.vlaanderen.be/ns/mandaat#Fractie>,
            <http://mu.semte.ch/vocabularies/ext/BestuursfunctieCode>
            ))
    }
    GRAPH <http://mu.semte.ch/graphs/public> {
      ?s ?p ?old .
    }
}
;
EOF
((tstamp++))
cat <<EOF > "$dir/$tstamp-ingest-exported-triples-2.sparql"
DELETE {
    GRAPH ?organizationalGraph {
        ?s ?p ?old .
    }
}  WHERE {
    GRAPH <http://mu.semte.ch/graphs/temp-ingest-graph> {
        ?s a <http://www.w3.org/ns/person#Person>.
    }
    ?s ^<http://data.vlaanderen.be/ns/mandaat#isBestuurlijkeAliasVan>/<http://www.w3.org/ns/org#holds>/^<http://www.w3.org/ns/org#hasPost>/<http://data.vlaanderen.be/ns/mandaat#isTijdspecialisatieVan>/<http://data.vlaanderen.be/ns/besluit#bestuurt> ?organization .
    GRAPH <http://mu.semte.ch/graphs/public> {
        ?organization <http://mu.semte.ch/vocabularies/core/uuid> ?organizationUuid .
        ?s ?p ?old .
    }
    BIND(IRI(CONCAT("http://mu.semte.ch/graphs/organizations/", ?organizationUuid)) as ?organizationalGraph)
}
;
 INSERT {
    GRAPH ?organizationalGraph {
        ?s ?p ?new .
    }
} WHERE {
    GRAPH <http://mu.semte.ch/graphs/temp-ingest-graph> {
        ?s a <http://www.w3.org/ns/person#Person> ;
           ?p ?new .
    }
    ?s ^<http://data.vlaanderen.be/ns/mandaat#isBestuurlijkeAliasVan>/<http://www.w3.org/ns/org#holds>/^<http://www.w3.org/ns/org#hasPost>/<http://data.vlaanderen.be/ns/mandaat#isTijdspecialisatieVan>/<http://data.vlaanderen.be/ns/besluit#bestuurt> ?organization .
    GRAPH <http://mu.semte.ch/graphs/public> {
        ?organization <http://mu.semte.ch/vocabularies/core/uuid> ?organizationUuid .
    }
    BIND(IRI(CONCAT("http://mu.semte.ch/graphs/organizations/", ?organizationUuid)) as ?organizationalGraph)
};
EOF
((tstamp++))
cat <<EOF > "$dir/$tstamp-ingest-exported-triples-3.sparql"
DELETE {
    GRAPH ?organizationalGraph {
        ?s ?p ?old .
    }
}  WHERE {
    GRAPH <http://mu.semte.ch/graphs/temp-ingest-graph> {
        ?s a <http://data.vlaanderen.be/ns/persoon#Geboorte>.
    }
    ?s ^<http://data.vlaanderen.be/ns/persoon#heeftGeboorte>/^<http://data.vlaanderen.be/ns/mandaat#isBestuurlijkeAliasVan>/<http://www.w3.org/ns/org#holds>/^<http://www.w3.org/ns/org#hasPost>/<http://data.vlaanderen.be/ns/mandaat#isTijdspecialisatieVan>/<http://data.vlaanderen.be/ns/besluit#bestuurt> ?organization .
    GRAPH <http://mu.semte.ch/graphs/public> {
        ?organization <http://mu.semte.ch/vocabularies/core/uuid> ?organizationUuid .
    }
    BIND(IRI(CONCAT("http://mu.semte.ch/graphs/organizations/", ?organizationUuid)) as ?organizationalGraph)
     GRAPH ?organizationalGraph {
        ?s ?p ?old .
    }
}
;
 INSERT {
    GRAPH ?organizationalGraph {
        ?s ?p ?new .
    }
} WHERE {
    GRAPH <http://mu.semte.ch/graphs/temp-ingest-graph> {
        ?s a <http://data.vlaanderen.be/ns/persoon#Geboorte> ;
           ?p ?new .
    }
    ?s ^<http://data.vlaanderen.be/ns/persoon#heeftGeboorte>/^<http://data.vlaanderen.be/ns/mandaat#isBestuurlijkeAliasVan>/<http://www.w3.org/ns/org#holds>/^<http://www.w3.org/ns/org#hasPost>/<http://data.vlaanderen.be/ns/mandaat#isTijdspecialisatieVan>/<http://data.vlaanderen.be/ns/besluit#bestuurt> ?organization .
    GRAPH <http://mu.semte.ch/graphs/public> {
        ?organization <http://mu.semte.ch/vocabularies/core/uuid> ?organizationUuid .
    }
    BIND(IRI(CONCAT("http://mu.semte.ch/graphs/organizations/", ?organizationUuid)) as ?organizationalGraph)
}
;
CLEAR GRAPH <http://mu.semte.ch/graphs/temp-ingest-graph>
EOF
