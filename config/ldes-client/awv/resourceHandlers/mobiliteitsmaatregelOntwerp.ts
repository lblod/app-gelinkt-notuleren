import { querySudo, updateSudo } from "@lblod/mu-auth-sudo";
import { sparqlEscapeUri } from "mu";
import { logger } from "../../../logger";
import { LDES_GRAPH, PUBLIC_GRAPH, SUDO_OPTIONS } from "../utils/constants";
import { moveWordtAangeduidDoor } from "./wordtAangeduidDoor";

export async function moveMobiliteitsmaatregelOntwerp(uri: string) {
  const graphQuery = `
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
    PREFIX relatie: <https://wegenenverkeer.data.vlaanderen.be/ns/implementatieelement#RelatieObject.>
    PREFIX onderdeel: <https://wegenenverkeer.data.vlaanderen.be/ns/onderdeel#>
    PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
    PREFIX owl: <http://www.w3.org/2002/07/owl#>
    select distinct ?adminUnitUuid where {
          GRAPH ${LDES_GRAPH} {
            ?relBevatMaatregelOntwerp a onderdeel:BevatMaatregelOntwerp ;
              relatie:bron ?aanvullendReglementOntwerp;
              relatie:doel ${sparqlEscapeUri(uri)}.
            ?relHeeftOntwerp a onderdeel:HeeftOntwerp ;
              relatie:bron ?ontwerpVerkeersteken;
              relatie:doel ?aanvullendReglementOntwerp.
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
         ?uriToMove a onderdeel:WordtAangeduidDoor;
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
    await moveWordtAangeduidDoor(uriToMove);
  }
}
