import { updateSudo } from "@lblod/mu-auth-sudo";
import { sparqlEscapeUri } from "mu";
import { LDES_GRAPH, SUDO_OPTIONS } from "./constants";

export async function moveResource(uri: string, targetGraph: string) {
  const orgGraph = sparqlEscapeUri(targetGraph);
  const resource = sparqlEscapeUri(uri);
  // WARNING: this query is split in two (note the semicolon) to bypass a virtuoso bug regarding DELETES
  // with optionals. This bug is present up until redpencil/virtuoso:1.2.1
  // even in later versions, this way of doing a replace-if-exists will keep working
  const moveQuery = `
    DELETE {
      GRAPH ${orgGraph} {
        ${resource} ?pOld ?oOld.
      }
    } WHERE {
      GRAPH ${orgGraph} {
	${resource} ?pOld ?oOld.
      }
    };

    INSERT {
      GRAPH ${orgGraph} {
        ${resource} ?a ?b
      }
    } WHERE {
      GRAPH ${LDES_GRAPH} {
        ${resource} ?a ?b
      }
      
    }
  `;
  await updateSudo(moveQuery, {}, SUDO_OPTIONS);
}
