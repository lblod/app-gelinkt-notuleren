import { querySudo } from "@lblod/mu-auth-sudo";
import { sparqlEscapeUri } from "mu";
import { logger } from "../../../logger";
import { LDES_GRAPH, PUBLIC_GRAPH, SUDO_OPTIONS } from "../utils/constants";
import { moveResource } from "../utils/moveResource";

export async function moveWordtAangeduidDoor(uri: string) {
  const graphQuery = `
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
    PREFIX relatie: <https://wegenenverkeer.data.vlaanderen.be/ns/implementatieelement#RelatieObject.>
    PREFIX onderdeel: <https://wegenenverkeer.data.vlaanderen.be/ns/onderdeel#>
    PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
    PREFIX owl: <http://www.w3.org/2002/07/owl#>
    select distinct ?adminUnitUuid where {
          GRAPH ${LDES_GRAPH} {
            ${sparqlEscapeUri(uri)} a onderdeel:WordtAangeduidDoor;
               relatie:bron ?mobiliteitsmaatregelOntwerp.
            ?relBevatMaatregelOntwerp a onderdeel:BevatMaatregelOntwerp ;
              relatie:bron ?aanvullendReglementOntwerp;
              relatie:doel ?mobiliteitsmaatregelOntwerp.
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
    logger.debug(`No admin unit found for ${uri}`);
    return;
  }
  const graph = `http://mu.semte.ch/graphs/awv/ldes/${adminUnitUuid}`;

  await moveResource(uri, graph);
}
