import { updateSudo, querySudo } from "@lblod/mu-auth-sudo";
import { sparqlEscapeUri } from "mu";
// this is a winston logger
import { logger } from "../../../logger";
import { moveHeeftWaardeVoor } from "./heeftWaardeVoor";
import {
  LDES_GRAPH,
  verkeerstekenQuery,
  PUBLIC_GRAPH,
  sudoOptions,
} from "../processPage";

export async function moveVariableInstanceWithLiteralValue(uri: string) {
  const graphQuery = `
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
    PREFIX relatie: <https://wegenenverkeer.data.vlaanderen.be/ns/implementatieelement#RelatieObject.>
    PREFIX onderdeel: <https://wegenenverkeer.data.vlaanderen.be/ns/onderdeel#>
    PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
    PREFIX owl: <http://www.w3.org/2002/07/owl#>
    select distinct ?adminUnitUuid where {
          GRAPH ${LDES_GRAPH} {
            ?relHeeftVerkeersteken a onderdeel:HeeftVerkeersteken ;
              relatie:bron ${sparqlEscapeUri(uri)};
              relatie:doel ?verkeersteken.
            ${verkeerstekenQuery}
          }
          GRAPH ${PUBLIC_GRAPH} {
            ?adminUnit owl:sameAs ?ovoUri;
              mu:uuid ?adminUnitUuid.
          }
      }`;
  const queryResult = await querySudo(graphQuery, {}, sudoOptions);
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
  await updateSudo(moveQuery, {}, sudoOptions);
  const queryUrisToMove = `
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
    PREFIX relatie: <https://wegenenverkeer.data.vlaanderen.be/ns/implementatieelement#RelatieObject.>
    PREFIX onderdeel: <https://wegenenverkeer.data.vlaanderen.be/ns/onderdeel#>
    PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
    select distinct ?uriToMove where {
     GRAPH ${LDES_GRAPH} {
        ?uriToMove a onderdeel:HeeftWaardeVoor ;
              relatie:bron ${sparqlEscapeUri(uri)}.
      }
      FILTER NOT EXISTS {
        GRAPH ${sparqlEscapeUri(graph)} {
          ?uriToMove ?pred ?obj.
        }
      }
    }
  `;
  const moveQueryResult = await querySudo(queryUrisToMove, {}, sudoOptions);
  const urisToMove = moveQueryResult.results.bindings.map(
    (binding) => binding.uriToMove.value,
  );
  for (const uriToMove of urisToMove) {
    await moveHeeftWaardeVoor(uriToMove);
  }
}
