import { updateSudo, querySudo } from "@lblod/mu-auth-sudo";
import { sparqlEscapeUri } from "mu";
// this is a winston logger
import { logger } from "../../../logger";
import { moveOntwerpVerkeersteken } from "./ontwerpVerkeersteken";
import { moveVerkeersbordVerkeersteken } from "./verkeersbordVerkeersteken";
import { verkeerstekenQuery } from "../processPage";
import { LDES_GRAPH, PUBLIC_GRAPH, SUDO_OPTIONS } from "../utils/constants";

export async function moveBevatVerkeersteken(uri: string) {
  const graphQuery = `
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
    PREFIX relatie: <https://wegenenverkeer.data.vlaanderen.be/ns/implementatieelement#RelatieObject.>
    PREFIX onderdeel: <https://wegenenverkeer.data.vlaanderen.be/ns/onderdeel#>
    PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
    PREFIX owl: <http://www.w3.org/2002/07/owl#>

    select distinct ?adminUnitUuid where {
          GRAPH ${LDES_GRAPH} {
            {
              ${sparqlEscapeUri(uri)} a onderdeel:BevatVerkeersteken ;
                relatie:bron ?signalisatieOntwerp.
              ?relHeeftBetrokkene a onderdeel:HeeftBetrokkene ;
                relatie:bron ?signalisatieOntwerp;
                relatie:doel ?ovoUri.
            } UNION {
             ${sparqlEscapeUri(uri)} a onderdeel:BevatVerkeersteken ;
              relatie:bron ?verkeersteken.
              ${verkeerstekenQuery}
            }
            
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
  console.log(JSON.stringify(queryResult));
  console.log(SUDO_OPTIONS);
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

  // This relationships can have 2 meanings so we have to move both children
  const queryUrisToMove = `
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
    PREFIX relatie: <https://wegenenverkeer.data.vlaanderen.be/ns/implementatieelement#RelatieObject.>
    PREFIX onderdeel: <https://wegenenverkeer.data.vlaanderen.be/ns/onderdeel#>
    PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
    select distinct ?uriToMove where {
     GRAPH ${LDES_GRAPH} {
        {
          ${sparqlEscapeUri(uri)} a onderdeel:BevatVerkeersteken ;
                relatie:doel ?uriToMove.
        }
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
    await moveOntwerpVerkeersteken(uriToMove);
    await moveVerkeersbordVerkeersteken(uriToMove);
  }
}
