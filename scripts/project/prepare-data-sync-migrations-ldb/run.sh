#!/bin/bash
export="$1"
tstamp=$(date +%Y%m%d%H%M%S)
dir="/project/config/migrations/$tstamp-data-sync-ldb"
mkdir -p "$dir"
cat "/project/$export" >> "$dir/$tstamp-export.ttl"
cat <<EOF > "$dir/$tstamp-export.graph"
http://mu.semte.ch/graphs/temp-ingest-graph
EOF
((tstamp++))
cat <<EOF > "$dir/$tstamp-ingest-exported-triples.sparql"
DELETE {
    GRAPH <http://mu.semte.ch/graphs/public> {
        ?s ?p ?o .
    }
} WHERE {
    GRAPH <http://mu.semte.ch/graphs/public> {
        ?s a ?type ; ?p ?o .
        FILTER (?type IN (
            <http://data.lblod.info/vocabularies/leidinggevenden/FunctionarisStatusCode>,
            <http://data.lblod.info/vocabularies/leidinggevenden/Functionaris>,
            <http://data.lblod.info/vocabularies/leidinggevenden/Bestuursfunctie>,
            <http://www.w3.org/ns/locn#Address>,
            <http://schema.org/ContactPoint>,
            <http://www.w3.org/ns/prov#Location>
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
            <http://data.lblod.info/vocabularies/leidinggevenden/FunctionarisStatusCode>,
            <http://data.lblod.info/vocabularies/leidinggevenden/Functionaris>,
            <http://data.lblod.info/vocabularies/leidinggevenden/Bestuursfunctie>,
            <http://www.w3.org/ns/locn#Address>,
            <http://schema.org/ContactPoint>,
            <http://www.w3.org/ns/prov#Location>
            ))
    }
}
;
# here we want to only replace data or insert new data, never remove old data.

# First delete triples that exist in the public graph and will be inserted again from temp-ingest-graph 
# The difference here is that data not being overwritten bij temp-ingest-graph will stay in the public graph
DELETE {
    GRAPH <http://mu.semte.ch/graphs/public> {
        ?s ?p ?old .
    }
} WHERE {
    GRAPH <http://mu.semte.ch/graphs/temp-ingest-graph> {
        ?s a ?type ; ?p ?new .
        FILTER (?type IN (
            <http://mu.semte.ch/vocabularies/ext/BestuursfunctieCode>
            ))
    }
    GRAPH <http://mu.semte.ch/graphs/public> {
      ?s ?p ?old .
    }
}
;
# Secondly, insert all related data to the public graph.
# this will insert triples removed from the public graph in the above query 
# and triples that are "new" (were not present in public graph before)
INSERT{
    GRAPH <http://mu.semte.ch/graphs/public> {
        ?s ?p ?new .
    }
} WHERE {
    GRAPH <http://mu.semte.ch/graphs/temp-ingest-graph> {
        ?s a ?type ; ?p ?new .
        FILTER (?type IN (
            <http://mu.semte.ch/vocabularies/ext/BestuursfunctieCode>
            ))
    }
}
;
DELETE {
    GRAPH ?organizationalGraph {
        ?s ?p ?old .
    }
}  WHERE {
    GRAPH <http://mu.semte.ch/graphs/temp-ingest-graph> {
        ?s a <http://www.w3.org/ns/person#Person>.
    }
    ?s ^<http://data.vlaanderen.be/ns/mandaat#isBestuurlijkeAliasVan>/<http://www.w3.org/ns/org#holds>/^<http://data.lblod.info/vocabularies/leidinggevenden/heeftBestuursfunctie>/<http://data.vlaanderen.be/ns/mandaat#isTijdspecialisatieVan>/<http://data.vlaanderen.be/ns/besluit#bestuurt> ?organization .
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
        ?s a <http://www.w3.org/ns/person#Person> ;
           ?p ?new .
    }
    ?s ^<http://data.vlaanderen.be/ns/mandaat#isBestuurlijkeAliasVan>/<http://www.w3.org/ns/org#holds>/^<http://data.lblod.info/vocabularies/leidinggevenden/heeftBestuursfunctie>/<http://data.vlaanderen.be/ns/mandaat#isTijdspecialisatieVan>/<http://data.vlaanderen.be/ns/besluit#bestuurt> ?organization .
    GRAPH <http://mu.semte.ch/graphs/public> {
        ?organization <http://mu.semte.ch/vocabularies/core/uuid> ?organizationUuid .
    }
    BIND(IRI(CONCAT("http://mu.semte.ch/graphs/organizations/", ?organizationUuid)) as ?organizationalGraph)
}
CLEAR GRAPH <http://mu.semte.ch/graphs/temp-ingest-graph>
EOF
