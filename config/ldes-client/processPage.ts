// this is a winston logger
import { logger } from "../logger";
import { updateSudo } from '@lblod/mu-auth-sudo';
import { sparqlEscapeUri } from 'mu';
import { BATCH_GRAPH, BYPASS_MU_AUTH, DIRECT_DATABASE_CONNECTION, TARGET_GRAPH, TIME_PREDICATE, VERSION_PREDICATE } from "../environment";

async function replaceExistingData() {
  let options = {};
  if(BYPASS_MU_AUTH){
    options = {
      sparqlEndpoint: DIRECT_DATABASE_CONNECTION,
    }
  }
  const query = /* sparql */ `
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
    PREFIX mandaat: <http://data.vlaanderen.be/ns/mandaat#>
    PREFIX org: <http://www.w3.org/ns/org#>
    PREFIX person: <http://www.w3.org/ns/person#>
    PREFIX dct: <http://purl.org/dc/terms/>
    PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
    PREFIX foaf: <http://xmlns.com/foaf/0.1/>
    PREFIX persoon: <http://data.vlaanderen.be/ns/persoon#>
    PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
    PREFIX adms: <http://www.w3.org/ns/adms#> 
    PREFIX as: <http://www.w3.org/ns/activitystreams#>
    DELETE {
      GRAPH ?target_graph {
          ?s ?pOld ?oOld.
      }
    }
    INSERT {
      GRAPH ?target_graph {
          ?s ?pNew ?oNew.
      }
    }
    WHERE {
      GRAPH ${sparqlEscapeUri(BATCH_GRAPH)} {
        ?stream <https://w3id.org/tree#member> ?versionedMember .
        ?versionedMember ${sparqlEscapeUri(VERSION_PREDICATE)} ?s .

        {
          ?versionedMember (a | as:formerType) ?type.
          VALUES ?type { mandaat:Mandataris mandaat:Fractie org:Membership }
          ?versionedMember ?pNew ?oNew.
          BIND(<http://mu.semte.ch/graphs/lmb-data-public> as ?target_graph)
        }
        UNION
        {
          ?versionedMember (a | as:formerType) ?type.
          VALUES ?type { person:Person }
          ?versionedMember ?pNew ?oNew.
          VALUES ?pNew {
              dct:modified
              mu:uuid
              foaf:familyName
              persoon:gebruikteVoornaam
          }
          BIND(<http://mu.semte.ch/graphs/lmb-data-public> as ?target_graph)
        }
        UNION
        {
          ?versionedMember (a | as:formerType) ?type.
          VALUES ?type { person:Person persoon:Geboorte }
          ?versionedMember ?pNew ?oNew.
          FILTER (?pNew NOT IN ( adms:identifier ))
          ?versionedMember ext:relatedTo ?administrativeUnit
          BIND(URI(REPLACE(STR(?administrativeUnit), "http://data.lblod.info/id/bestuurseenheden/", "http://mu.semte.ch/graphs/lmb-data-private/")) as ?target_graph)
        }
        FILTER (?pNew NOT IN ( ${sparqlEscapeUri(VERSION_PREDICATE)}, ${sparqlEscapeUri(TIME_PREDICATE)}, ext:relatedTo ))
      }
      OPTIONAL {
        GRAPH ?target_graph {
          ?s ?pOld ?oOld.
        }
      }
    };
    DELETE {
      GRAPH <http://mu.semte.ch/graphs/lmb-data-public> {
        ?administrativeUnit ext:lastLMBUpdate ?oldTimestamp.
      }
    }
    INSERT {
      GRAPH <http://mu.semte.ch/graphs/lmb-data-public> {
        ?administrativeUnit ext:lastLMBUpdate ?newTimestamp.
      }
    }
    WHERE {
      {
        SELECT ?administrativeUnit (MAX(?timestamp) as ?newTimestamp)
        {
          GRAPH ${sparqlEscapeUri(BATCH_GRAPH)} {
            ?stream <https://w3id.org/tree#member> ?versionedMember .
            ?versionedMember ext:relatedTo ?administrativeUnit.
            ?versionedMember ${sparqlEscapeUri(TIME_PREDICATE)} ?timestamp.
          }
        }
        GROUP BY ?administrativeUnit
      }
      OPTIONAL {
        GRAPH <http://mu.semte.ch/graphs/lmb-data-public> {
          ?administrativeUnit ext:lastLMBUpdate ?oldTimestamp.
        }
      }
    }
  `
  await updateSudo(query, {}, options);
}

export async function processPage(){
  logger.debug('Running custom logic to process the current page');
  await replaceExistingData();
  return;
}

