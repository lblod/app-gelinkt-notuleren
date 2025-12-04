// this is a winston logger
import { logger } from "../../logger";
import { updateSudo } from "@lblod/mu-auth-sudo";
import { sparqlEscapeUri } from "mu";
import { environment } from "../../environment";

async function replaceExistingData() {
  let options = {};
  if (environment.BYPASS_MU_AUTH) {
    options = {
      sparqlEndpoint: environment.DIRECT_DATABASE_CONNECTION,
    };
  }
  await updateSudo(
    `
    PREFIX mu: <http://mu.semte.ch/vocabularies/core/>

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
