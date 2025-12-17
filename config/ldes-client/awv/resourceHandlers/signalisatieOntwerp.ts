import { updateSudo, querySudo } from "@lblod/mu-auth-sudo";
import { sparqlEscapeUri } from "mu";
// this is a winston logger
import { logger } from "../../../logger";
import { moveBevatVerkeersteken } from "./bevatVerkeersteken";
import { LDES_GRAPH, PUBLIC_GRAPH, SUDO_OPTIONS } from "../utils/constants";

export async function moveSignalisatieOntwerp(uri: string) {
  const graphQuery = `
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
    PREFIX relatie: <https://wegenenverkeer.data.vlaanderen.be/ns/implementatieelement#RelatieObject.>
    PREFIX onderdeel: <https://wegenenverkeer.data.vlaanderen.be/ns/onderdeel#>
    PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
    PREFIX owl: <http://www.w3.org/2002/07/owl#>
    select distinct ?adminUnitUuid where {
          GRAPH ${LDES_GRAPH} {
            ?rel a  onderdeel:HeeftBetrokkene ;
              relatie:bron ${sparqlEscapeUri(uri)};
              relatie:doel ?ovoUri.
            
          }
          GRAPH ${PUBLIC_GRAPH} {
            ?adminUnit owl:sameAs ?ovoUri;
              mu:uuid ?adminUnitUuid.
          }
      }`;
  const queryResult = await querySudo<{ adminUnitUuid: string }>(
    graphQuery,
    {},
    SUDO_OPTIONS,
  );
  const adminUnitUuid = queryResult.results.bindings[0]?.adminUnitUuid.value;
  if (!adminUnitUuid) {
    logger.error(`No admin unit found for ${uri}`);
    return;
  }
  const graph = `http://mu.semte.ch/graphs/awv/ldes/${adminUnitUuid}`;
  const moveQuery = `
    DELETE {
      GRAPH ${sparqlEscapeUri(graph)} {
        ${sparqlEscapeUri(uri)} ?pOld ?oOld.
      }
    } WHERE {
     OPTIONAL {
        GRAPH ${sparqlEscapeUri(graph)} {
          ${sparqlEscapeUri(uri)} ?pOld ?oOld.
        }
      }
    };

    INSERT {
      GRAPH ${sparqlEscapeUri(graph)} {
        ${sparqlEscapeUri(uri)} ?a ?b
      }
    } WHERE {
      GRAPH ${LDES_GRAPH} {
        ${sparqlEscapeUri(uri)} ?a ?b
      }
      
    }
  `;
  await updateSudo(moveQuery, {}, SUDO_OPTIONS);
  const queryUrisToMove = `
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
    PREFIX relatie: <https://wegenenverkeer.data.vlaanderen.be/ns/implementatieelement#RelatieObject.>
    PREFIX onderdeel: <https://wegenenverkeer.data.vlaanderen.be/ns/onderdeel#>
    PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
    select distinct ?uriToMove where {
     GRAPH ${LDES_GRAPH} {
        ?uriToMove a onderdeel:BevatVerkeersteken ;
                relatie:bron ${sparqlEscapeUri(uri)}.
      }
      FILTER NOT EXISTS {
        GRAPH ${sparqlEscapeUri(graph)} {
          ?uriToMove ?pred ?obj.
        }
      }
    }
  `;

  const moveQueryResult = await querySudo<{ uriToMove: string }>(
    queryUrisToMove,
    {},
    SUDO_OPTIONS,
  );
  const urisToMove = moveQueryResult.results.bindings.map(
    (binding) => binding.uriToMove.value,
  );
  for (const uriToMove of urisToMove) {
    await moveBevatVerkeersteken(uriToMove);
  }
}
