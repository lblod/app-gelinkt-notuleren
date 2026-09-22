// this is a winston logger
import { logger } from "../../logger";
import { updateSudo } from "@lblod/mu-auth-sudo";
import { sparqlEscapeUri, sparqlEscapeString, uuid } from "mu";
import { environment } from "../../environment";

async function replaceExistingData() {
  let options = {};
  if (environment.BYPASS_MU_AUTH) {
    options = {
      sparqlEndpoint: environment.DIRECT_DATABASE_CONNECTION,
    };
  }
  await updateSudo(
    /* sparql */`
    PREFIX mu: <http://mu.semte.ch/vocabularies/core/>

    INSERT {
      GRAPH ${sparqlEscapeUri(environment.getTargetGraph())} {
        ?s mu:uuid ?uuid.
      }
    } WHERE {
      GRAPH ${sparqlEscapeUri(environment.BATCH_GRAPH)} {
        ?stream <https://w3id.org/tree#member> ?versionedMember .
        ?versionedMember ${sparqlEscapeUri(
          environment.getVersionPredicate()
        )} ?s .
      }
      FILTER NOT EXISTS {
        GRAPH ${sparqlEscapeUri(environment.getTargetGraph())} {
          ?s mu:uuid ?existingUuid.
        }
      }
      BIND(LCASE(STRUUID()) AS ?uuid)
    };

    DELETE {
      GRAPH ${sparqlEscapeUri(environment.getTargetGraph())} {
        ?s ?pOld ?oOld.
      }
    }
    INSERT {
      GRAPH ${sparqlEscapeUri(environment.getTargetGraph())} {
        ?s ?pNew ?oNew.
      }
    } WHERE {
      GRAPH ${sparqlEscapeUri(environment.BATCH_GRAPH)} {
        ?stream <https://w3id.org/tree#member> ?versionedMember .
        ?versionedMember ${sparqlEscapeUri(
          environment.getVersionPredicate()
        )} ?s .
        ?versionedMember ?pNew ?oNew.
        FILTER (?pNew NOT IN ( ${sparqlEscapeUri(
          environment.getVersionPredicate()
        )} ))
      }
      OPTIONAL {
        GRAPH ${sparqlEscapeUri(environment.getTargetGraph())} {
          ?s ?pOld ?oOld.
          FILTER (?pOld NOT IN ( mu:uuid ))
        }
      }
    }`,
    {},
    options
  );
}

export async function processPage() {
  logger.debug("Running custom logic to process the current page");
  await replaceExistingData();
  return;
}
