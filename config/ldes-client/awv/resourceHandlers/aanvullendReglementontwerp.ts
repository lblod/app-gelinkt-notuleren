import { updateSudo, querySudo } from "@lblod/mu-auth-sudo";
import { sparqlEscapeUri } from "mu";
// this is a winston logger
import { logger } from "../../../logger";
import { moveBevatMaatregelOntwerp } from "./bevatMaatregelOntwerp";
import { LDES_GRAPH, sudoOptions, PUBLIC_GRAPH } from "../utils/constants";

export async function moveAanvullendReglementOntwerp(uri: string) {
  const graphQuery = `
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
    PREFIX relatie: <https://wegenenverkeer.data.vlaanderen.be/ns/implementatieelement#RelatieObject.>
    PREFIX onderdeel: <https://wegenenverkeer.data.vlaanderen.be/ns/onderdeel#>
    PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
    PREFIX owl: <http://www.w3.org/2002/07/owl#>
    select distinct ?adminUnitUuid where {
          GRAPH ${LDES_GRAPH} {
            ?relHeeftOntwerp a onderdeel:HeeftOntwerp ;
              relatie:bron ?ontwerpVerkeersteken;
              relatie:doel ${sparqlEscapeUri(uri)}.
            ?relBevatVerkeersteken a onderdeel:BevatVerkeersteken ;
              relatie:bron ?signalisatieOntwerp;
              relatie:doel ?ontwerpVerkeersteken.
            ?relHeeftBetrokkene a onderdeel:HeeftBetrokkene ;
              relatie:bron ?signalisatieOntwerp;
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
    sudoOptions,
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
  await updateSudo(moveQuery, {}, sudoOptions);
  const queryUrisToMove = `
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
    PREFIX relatie: <https://wegenenverkeer.data.vlaanderen.be/ns/implementatieelement#RelatieObject.>
    PREFIX onderdeel: <https://wegenenverkeer.data.vlaanderen.be/ns/onderdeel#>
    PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
    select distinct ?uriToMove where {
     GRAPH ${LDES_GRAPH} {
        ?uriToMove a onderdeel:BevatMaatregelOntwerp ;
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
    sudoOptions,
  );
  const urisToMove = moveQueryResult.results.bindings.map(
    (binding) => binding.uriToMove.value,
  );
  for (const uriToMove of urisToMove) {
    await moveBevatMaatregelOntwerp(uriToMove);
  }
}
