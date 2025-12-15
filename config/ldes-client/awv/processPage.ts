// this is a winston logger
import { logger } from "../../logger";
import { updateSudo, querySudo } from "@lblod/mu-auth-sudo";
import { sparqlEscapeUri, sparqlEscapeString, uuid as generateUuid } from "mu";
import { environment } from "../../environment";

const LDES_GRAPH = "<http://mu.semte.ch/graphs/awv/ldes>";
const PUBLIC_GRAPH = "<http://mu.semte.ch/graphs/public>";

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
      GRAPH ${LDES_GRAPH} {
        ?s ?pOld ?oOld.
      }
    }
    INSERT {
      GRAPH ${LDES_GRAPH} {
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
        GRAPH ${LDES_GRAPH} {
          ?s ?pOld ?oOld.
          FILTER (?pOld NOT IN ( mu:uuid))
        }
      }
    }`,
    {},
    options
  );
  const subjectsQuery = await querySudo(
    `
      SELECT DISTINCT ?s WHERE {
          GRAPH ${sparqlEscapeUri(environment.BATCH_GRAPH)} {
            ?stream <https://w3id.org/tree#member> ?versionedMember .
            ?versionedMember ${sparqlEscapeUri(
              environment.getVersionPredicate()
            )} ?s .
          }
      }
    `,
    options
  );
  const subjects = subjectsQuery.results.bindings.map(
    (binding) => binding.s.value
  );

  await generateUuids();

  const urisWithType = await Promise.all([...subjects].map(mapUriToType));
  await moveByType(urisWithType);
}

async function generateUuids() {
  const subjectsWithoutUuidsQuery = await querySudo(`
    PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
    
    SELECT DISTINCT ?s WHERE {
      GRAPH ${LDES_GRAPH} {
        ?s a ?type
        FILTER NOT EXISTS {
          ?s mu:uuid ?uuid
        }
      }
    }
  `);
  const subjectsWithUuid = subjectsWithoutUuidsQuery.results.bindings.map(
    (binding) => ({ subject: binding.s.value, uuid: generateUuid() })
  );
  if (!subjectsWithUuid.length) return;

  const insertUuids = await updateSudo(`
    PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
    INSERT DATA {
      GRAPH ${LDES_GRAPH} {
        ${subjectsWithUuid
          .map(
            (subjects) =>
              `${sparqlEscapeUri(
                subjects.subject
              )} mu:uuid ${sparqlEscapeString(subjects.uuid)}`
          )
          .join(". \n")}
      }
    }
  `);
}

export async function processPage() {
  logger.debug("Running custom logic to process the current page");
  await replaceExistingData();
  return;
}

async function mapUriToType(uri: string) {
  const signUriQuery = `
        select distinct ?type where {
            GRAPH ${LDES_GRAPH} {
                ${sparqlEscapeUri(uri)} a ?type.
            }
        } 
    `;
  const queryResult = await querySudo(signUriQuery);
  return {
    uri,
    type: queryResult.results.bindings[0]?.type.value as string | undefined,
  };
}

async function moveByType(urisWithType: { uri: string; type?: string }[]) {
  for (const { uri, type } of urisWithType) {
    switch (type) {
      case "https://data.vlaanderen.be/ns/mobiliteit#SignalisatieOntwerp":
        await moveSignalisatieOntwerp(uri);
        break;
      case "https://wegenenverkeer.data.vlaanderen.be/ns/installatie#Verkeersbordopstelling":
        await moveVerkeersbordopstelling(uri);
        break;

      case "https://wegenenverkeer.data.vlaanderen.be/ns/onderdeel#WordtAangeduidDoor":
        await moveWordtAangeduidDoor(uri);
        break;

      case "https://data.vlaanderen.be/ns/mobiliteit#VerkeersbordVerkeersteken":
        await moveVerkeersbordVerkeersteken(uri);
        break;

      case "https://wegenenverkeer.data.vlaanderen.be/ns/onderdeel#BevatMaatregelOntwerp":
        await moveBevatMaatregelOntwerp(uri);
        break;

      case "https://wegenenverkeer.data.vlaanderen.be/ns/onderdeel#IsGebaseerdOp":
        await moveIsGebaseerdOp(uri);
        break;

      case "https://lblod.data.gift/vocabularies/variables/VariableInstanceWithLiteralValue":
        await moveVariableInstanceWithLiteralValue(uri);
        break;

      case "https://wegenenverkeer.data.vlaanderen.be/ns/onderdeel#HeeftVerkeersteken":
        await moveHeeftVerkeersteken(uri);
        break;

      case "https://wegenenverkeer.data.vlaanderen.be/ns/onderdeel#HeeftBetrokkene":
        await moveHeeftBetrokkene(uri);
        break;

      case "https://wegenenverkeer.data.vlaanderen.be/ns/onderdeel#HeeftOntwerp":
        await moveHeeftOntwerp(uri);
        break;

      case "https://wegenenverkeer.data.vlaanderen.be/ns/onderdeel#BevatVerkeersteken":
        await moveBevatVerkeersteken(uri);
        break;

      case "https://wegenenverkeer.data.vlaanderen.be/ns/onderdeel#IsOntwerpVan":
        break;

      case "https://data.vlaanderen.be/ns/mobiliteit#MobiliteitsmaatregelOntwerp":
        await moveMobiliteitsmaatregelOntwerp(uri);
        break;

      case "https://data.vlaanderen.be/ns/mobiliteit#OntwerpVerkeersteken":
        await moveOntwerpVerkeersteken(uri);
        break;

      case "https://data.vlaanderen.be/ns/mobiliteit#AanvullendReglementOntwerp":
        await moveAanvullendReglementOntwerp(uri);
        break;

      case "http://lblod.data.gift/vocabularies/variables/VariableInstanceWithResourceValue":
        await moveVariableInstanceWithResourceValue(uri);
        break;
    }
  }
}

async function moveSignalisatieOntwerp(uri: string) {
  const graphQuery = `
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
    PREFIX relatie: <https://wegenenverkeer.data.vlaanderen.be/ns/implementatieelement#RelatieObject.>
    PREFIX onderdeel: <https://wegenenverkeer.data.vlaanderen.be/ns/onderdeel#>
    PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
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
  const queryResult = await querySudo(graphQuery);
  const adminUnitUuid = queryResult.results.bindings[0]?.adminUnitUuid.value;
  if (!adminUnitUuid) return;
  const graph = `http://mu.semte.ch/graphs/awv/ldes/${adminUnitUuid}`;
  const moveQuery = `
    DELETE {
      GRAPH ${sparqlEscapeUri(graph)} {
        ?s ?pOld ?oOld.
      }
    }
    INSERT {
      GRAPH ${sparqlEscapeUri(graph)} {
        ${sparqlEscapeUri(uri)} ?a ?b
      }
    } WHERE {
      GRAPH ${LDES_GRAPH} {
        ${sparqlEscapeUri(uri)} ?a ?b
      }
      OPTIONAL {
        GRAPH ${sparqlEscapeUri(graph)} {
          ?s ?pOld ?oOld.
        }
      }
    }
  `;
  await updateSudo(moveQuery);
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

  const moveQueryResult = await querySudo(queryUrisToMove);
  const urisToMove = moveQueryResult.results.bindings.map(
    (binding) => binding.uriToMove.value
  );
  for (const uriToMove of urisToMove) {
    await moveBevatVerkeersteken(uriToMove);
  }
}

async function moveBevatVerkeersteken(uri: string) {
  const graphQuery = `
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
    PREFIX relatie: <https://wegenenverkeer.data.vlaanderen.be/ns/implementatieelement#RelatieObject.>
    PREFIX onderdeel: <https://wegenenverkeer.data.vlaanderen.be/ns/onderdeel#>
     PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
    select distinct ?adminUnitUuid where {
          GRAPH ${LDES_GRAPH} {
            ${sparqlEscapeUri(uri)} a onderdeel:BevatVerkeersteken ;
              relatie:bron ?signalisatieOntwerp.
            ?relHeeftBetrokkene a onderdeel:HeeftBetrokkene ;
              relatie:bron ?signalisatieOntwerp;
              relatie:doel ?ovoUri.
            
          }
          GRAPH ${PUBLIC_GRAPH} {
            ?adminUnit owl:sameAs ?ovoUri;
              mu:uuid ?adminUnitUuid.
          }
      }`;
  const queryResult = await querySudo(graphQuery);
  const adminUnitUuid = queryResult.results.bindings[0]?.adminUnitUuid.value;
  if (!adminUnitUuid) return;
  const graph = `http://mu.semte.ch/graphs/awv/ldes/${adminUnitUuid}`;
  const moveQuery = `
    DELETE {
      GRAPH ${sparqlEscapeUri(graph)} {
        ?s ?pOld ?oOld.
      }
    }
    INSERT {
      GRAPH ${sparqlEscapeUri(graph)} {
        ${sparqlEscapeUri(uri)} ?a ?b
      }
    } WHERE {
      GRAPH ${LDES_GRAPH} {
        ${sparqlEscapeUri(uri)} ?a ?b
      }
      OPTIONAL {
        GRAPH ${sparqlEscapeUri(graph)} {
          ?s ?pOld ?oOld.
        }
      }
    }
  `;
  await updateSudo(moveQuery);
  const queryUrisToMove = `
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
    PREFIX relatie: <https://wegenenverkeer.data.vlaanderen.be/ns/implementatieelement#RelatieObject.>
    PREFIX onderdeel: <https://wegenenverkeer.data.vlaanderen.be/ns/onderdeel#>
    PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
    select distinct ?uriToMove where {
     GRAPH ${LDES_GRAPH} {
        ${sparqlEscapeUri(uri)} a onderdeel:BevatVerkeersteken ;
                relatie:doel ?uriToMove.
      }
      FILTER NOT EXISTS {
        GRAPH ${sparqlEscapeUri(graph)} {
          ?uriToMove ?pred ?obj.
        }
      }
    }
  `;
  const moveQueryResult = await querySudo(queryUrisToMove);
  const urisToMove = moveQueryResult.results.bindings.map(
    (binding) => binding.uriToMove.value
  );
  for (const uriToMove of urisToMove) {
    await moveOntwerpVerkeersteken(uriToMove);
  }
}

async function moveOntwerpVerkeersteken(uri: string) {
  const graphQuery = `
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
    PREFIX relatie: <https://wegenenverkeer.data.vlaanderen.be/ns/implementatieelement#RelatieObject.>
    PREFIX onderdeel: <https://wegenenverkeer.data.vlaanderen.be/ns/onderdeel#>
     PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
    select distinct ?adminUnitUuid where {
          GRAPH ${LDES_GRAPH} {
            ?relBevatVerkeersteken a onderdeel:BevatVerkeersteken ;
              relatie:bron ?signalisatieOntwerp;
              relatie:doel ${sparqlEscapeUri(uri)}.
            ?relHeeftBetrokkene a onderdeel:HeeftBetrokkene ;
              relatie:bron ?signalisatieOntwerp;
              relatie:doel ?ovoUri.
            
          }
          GRAPH ${PUBLIC_GRAPH} {
            ?adminUnit owl:sameAs ?ovoUri;
              mu:uuid ?adminUnitUuid.
          }
      }`;
  const queryResult = await querySudo(graphQuery);
  const adminUnitUuid = queryResult.results.bindings[0]?.adminUnitUuid.value;
  if (!adminUnitUuid) return;
  const graph = `http://mu.semte.ch/graphs/awv/ldes/${adminUnitUuid}`;
  const moveQuery = `
    DELETE {
      GRAPH ${sparqlEscapeUri(graph)} {
        ?s ?pOld ?oOld.
      }
    }
    INSERT {
      GRAPH ${sparqlEscapeUri(graph)} {
        ${sparqlEscapeUri(uri)} ?a ?b
      }
    } WHERE {
      GRAPH ${LDES_GRAPH} {
        ${sparqlEscapeUri(uri)} ?a ?b
      }
      OPTIONAL {
        GRAPH ${sparqlEscapeUri(graph)} {
          ?s ?pOld ?oOld.
        }
      }
    }
  `;
  await updateSudo(moveQuery);
  const queryUrisToMove = `
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
    PREFIX relatie: <https://wegenenverkeer.data.vlaanderen.be/ns/implementatieelement#RelatieObject.>
    PREFIX onderdeel: <https://wegenenverkeer.data.vlaanderen.be/ns/onderdeel#>
    PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
    select distinct ?uriToMove where {
     GRAPH ${LDES_GRAPH} {
       ?uriToMove a onderdeel:HeeftOntwerp ;
              relatie:bron ${sparqlEscapeUri(uri)}.
      }
      FILTER NOT EXISTS {
        GRAPH ${sparqlEscapeUri(graph)} {
          ?uriToMove ?pred ?obj.
        }
      }
    }
  `;
  const moveQueryResult = await querySudo(queryUrisToMove);
  const urisToMove = moveQueryResult.results.bindings.map(
    (binding) => binding.uriToMove.value
  );
  for (const uriToMove of urisToMove) {
    await moveHeeftOntwerp(uriToMove);
  }
}

async function moveHeeftOntwerp(uri: string) {
  const graphQuery = `
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
    PREFIX relatie: <https://wegenenverkeer.data.vlaanderen.be/ns/implementatieelement#RelatieObject.>
    PREFIX onderdeel: <https://wegenenverkeer.data.vlaanderen.be/ns/onderdeel#>
     PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
    select distinct ?adminUnitUuid where {
          GRAPH ${LDES_GRAPH} {
          ${sparqlEscapeUri(uri)} a onderdeel:HeeftOntwerp ;
              relatie:bron ?ontwerpVerkeersteken.
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
  const queryResult = await querySudo(graphQuery);
  const adminUnitUuid = queryResult.results.bindings[0]?.adminUnitUuid.value;
  if (!adminUnitUuid) return;
  const graph = `http://mu.semte.ch/graphs/awv/ldes/${adminUnitUuid}`;
  const moveQuery = `
    DELETE {
      GRAPH ${sparqlEscapeUri(graph)} {
        ?s ?pOld ?oOld.
      }
    }
    INSERT {
      GRAPH ${sparqlEscapeUri(graph)} {
        ${sparqlEscapeUri(uri)} ?a ?b
      }
    } WHERE {
      GRAPH ${LDES_GRAPH} {
        ${sparqlEscapeUri(uri)} ?a ?b
      }
      OPTIONAL {
        GRAPH ${sparqlEscapeUri(graph)} {
          ?s ?pOld ?oOld.
        }
      }
    }
  `;
  await updateSudo(moveQuery);
  const queryUrisToMove = `
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
    PREFIX relatie: <https://wegenenverkeer.data.vlaanderen.be/ns/implementatieelement#RelatieObject.>
    PREFIX onderdeel: <https://wegenenverkeer.data.vlaanderen.be/ns/onderdeel#>
    PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
    select distinct ?uriToMove where {
     GRAPH ${LDES_GRAPH} {
       ${sparqlEscapeUri(uri)} a onderdeel:HeeftOntwerp ;
              relatie:doel ?uriToMove.
      }
      FILTER NOT EXISTS {
        GRAPH ${sparqlEscapeUri(graph)} {
          ?uriToMove ?pred ?obj.
        }
      }
    }
  `;
  const moveQueryResult = await querySudo(queryUrisToMove);
  const urisToMove = moveQueryResult.results.bindings.map(
    (binding) => binding.uriToMove.value
  );
  for (const uriToMove of urisToMove) {
    await moveAanvullendReglementOntwerp(uriToMove);
  }
}

async function moveAanvullendReglementOntwerp(uri: string) {
  const graphQuery = `
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
    PREFIX relatie: <https://wegenenverkeer.data.vlaanderen.be/ns/implementatieelement#RelatieObject.>
    PREFIX onderdeel: <https://wegenenverkeer.data.vlaanderen.be/ns/onderdeel#>
     PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
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
  const queryResult = await querySudo(graphQuery);
  const adminUnitUuid = queryResult.results.bindings[0]?.adminUnitUuid.value;
  if (!adminUnitUuid) return;
  const graph = `http://mu.semte.ch/graphs/awv/ldes/${adminUnitUuid}`;
  const moveQuery = `
    DELETE {
      GRAPH ${sparqlEscapeUri(graph)} {
        ?s ?pOld ?oOld.
      }
    }
    INSERT {
      GRAPH ${sparqlEscapeUri(graph)} {
        ${sparqlEscapeUri(uri)} ?a ?b
      }
    } WHERE {
      GRAPH ${LDES_GRAPH} {
        ${sparqlEscapeUri(uri)} ?a ?b
      }
      OPTIONAL {
        GRAPH ${sparqlEscapeUri(graph)} {
          ?s ?pOld ?oOld.
        }
      }
    }
  `;
  await updateSudo(moveQuery);
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
  const moveQueryResult = await querySudo(queryUrisToMove);
  const urisToMove = moveQueryResult.results.bindings.map(
    (binding) => binding.uriToMove.value
  );
  for (const uriToMove of urisToMove) {
    await moveBevatMaatregelOntwerp(uriToMove);
  }
}

async function moveBevatMaatregelOntwerp(uri: string) {
  const graphQuery = `
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
    PREFIX relatie: <https://wegenenverkeer.data.vlaanderen.be/ns/implementatieelement#RelatieObject.>
    PREFIX onderdeel: <https://wegenenverkeer.data.vlaanderen.be/ns/onderdeel#>
     PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
    select distinct ?adminUnitUuid where {
          GRAPH ${LDES_GRAPH} {
            ${sparqlEscapeUri(uri)} a onderdeel:BevatMaatregelOntwerp ;
              relatie:bron ?aanvullendReglementOntwerp.
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
  const queryResult = await querySudo(graphQuery);
  const adminUnitUuid = queryResult.results.bindings[0]?.adminUnitUuid.value;
  if (!adminUnitUuid) return;
  const graph = `http://mu.semte.ch/graphs/awv/ldes/${adminUnitUuid}`;
  const moveQuery = `
    DELETE {
      GRAPH ${sparqlEscapeUri(graph)} {
        ?s ?pOld ?oOld.
      }
    }
    INSERT {
      GRAPH ${sparqlEscapeUri(graph)} {
        ${sparqlEscapeUri(uri)} ?a ?b
      }
    } WHERE {
      GRAPH ${LDES_GRAPH} {
        ${sparqlEscapeUri(uri)} ?a ?b
      }
      OPTIONAL {
        GRAPH ${sparqlEscapeUri(graph)} {
          ?s ?pOld ?oOld.
        }
      }
    }
  `;
  await updateSudo(moveQuery);
  const queryUrisToMove = `
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
    PREFIX relatie: <https://wegenenverkeer.data.vlaanderen.be/ns/implementatieelement#RelatieObject.>
    PREFIX onderdeel: <https://wegenenverkeer.data.vlaanderen.be/ns/onderdeel#>
    PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
    select distinct ?uriToMove where {
     GRAPH ${LDES_GRAPH} {
        ${sparqlEscapeUri(uri)} a onderdeel:BevatMaatregelOntwerp;
          relatie:doel ?uriToMove.
      }
      FILTER NOT EXISTS {
        GRAPH ${sparqlEscapeUri(graph)} {
          ?uriToMove ?pred ?obj.
        }
      }
    }
  `;
  const moveQueryResult = await querySudo(queryUrisToMove);
  const urisToMove = moveQueryResult.results.bindings.map(
    (binding) => binding.uriToMove.value
  );
  for (const uriToMove of urisToMove) {
    await moveMobiliteitsmaatregelOntwerp(uriToMove);
  }
}

async function moveMobiliteitsmaatregelOntwerp(uri: string) {
  const graphQuery = `
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
    PREFIX relatie: <https://wegenenverkeer.data.vlaanderen.be/ns/implementatieelement#RelatieObject.>
    PREFIX onderdeel: <https://wegenenverkeer.data.vlaanderen.be/ns/onderdeel#>
     PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
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
  const queryResult = await querySudo(graphQuery);
  const adminUnitUuid = queryResult.results.bindings[0]?.adminUnitUuid.value;
  if (!adminUnitUuid) return;
  const graph = `http://mu.semte.ch/graphs/awv/ldes/${adminUnitUuid}`;
  const moveQuery = `
    DELETE {
      GRAPH ${sparqlEscapeUri(graph)} {
        ?s ?pOld ?oOld.
      }
    }
    INSERT {
      GRAPH ${sparqlEscapeUri(graph)} {
        ${sparqlEscapeUri(uri)} ?a ?b
      }
    } WHERE {
      GRAPH ${LDES_GRAPH} {
        ${sparqlEscapeUri(uri)} ?a ?b
      }
      OPTIONAL {
        GRAPH ${sparqlEscapeUri(graph)} {
          ?s ?pOld ?oOld.
        }
      }
    }
  `;
  await updateSudo(moveQuery);
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
  const moveQueryResult = await querySudo(queryUrisToMove);
  const urisToMove = moveQueryResult.results.bindings.map(
    (binding) => binding.uriToMove.value
  );
  for (const uriToMove of urisToMove) {
    await moveWordtAangeduidDoor(uriToMove);
  }
}

async function moveWordtAangeduidDoor(uri: string) {
  const graphQuery = `
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
    PREFIX relatie: <https://wegenenverkeer.data.vlaanderen.be/ns/implementatieelement#RelatieObject.>
    PREFIX onderdeel: <https://wegenenverkeer.data.vlaanderen.be/ns/onderdeel#>
     PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
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
  const queryResult = await querySudo(graphQuery);
  const adminUnitUuid = queryResult.results.bindings[0]?.adminUnitUuid.value;
  if (!adminUnitUuid) return;
  const graph = `http://mu.semte.ch/graphs/awv/ldes/${adminUnitUuid}`;
  const moveQuery = `
    DELETE {
      GRAPH ${sparqlEscapeUri(graph)} {
        ?s ?pOld ?oOld.
      }
    }
    INSERT {
      GRAPH ${sparqlEscapeUri(graph)} {
        ${sparqlEscapeUri(uri)} ?a ?b
      }
    } WHERE {
      GRAPH ${LDES_GRAPH} {
        ${sparqlEscapeUri(uri)} ?a ?b
      }
      OPTIONAL {
        GRAPH ${sparqlEscapeUri(graph)} {
          ?s ?pOld ?oOld.
        }
      }
    }
  `;
  await updateSudo(moveQuery);
  const queryUrisToMove = `
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
    PREFIX relatie: <https://wegenenverkeer.data.vlaanderen.be/ns/implementatieelement#RelatieObject.>
    PREFIX onderdeel: <https://wegenenverkeer.data.vlaanderen.be/ns/onderdeel#>
    PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
    select distinct ?uriToMove where {
     GRAPH ${LDES_GRAPH} {
         ${sparqlEscapeUri(uri)} a onderdeel:WordtAangeduidDoor;
               relatie:doel ?uriToMove.
      }
      FILTER NOT EXISTS {
        GRAPH ${sparqlEscapeUri(graph)} {
          ?uriToMove ?pred ?obj.
        }
      }
    }
  `;
  const moveQueryResult = await querySudo(queryUrisToMove);
  const urisToMove = moveQueryResult.results.bindings.map(
    (binding) => binding.uriToMove.value
  );
  for (const uriToMove of urisToMove) {
    await moveVerkeersbordVerkeersteken(uriToMove);
  }
}

async function moveVerkeersbordVerkeersteken(uri: string) {
  const graphQuery = `
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
    PREFIX relatie: <https://wegenenverkeer.data.vlaanderen.be/ns/implementatieelement#RelatieObject.>
    PREFIX onderdeel: <https://wegenenverkeer.data.vlaanderen.be/ns/onderdeel#>
     PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
    select distinct ?adminUnitUuid where {
          GRAPH ${LDES_GRAPH} {
            {
              ?relWordtAangeduidDoor a onderdeel:WordtAangeduidDoor;
                relatie:doel ${sparqlEscapeUri(uri)};
                  relatie:bron ?mobiliteitsmaatregelOntwerp.
              ?relBevatMaatregelOntwerp a onderdeel:BevatMaatregelOntwerp ;
                relatie:bron ?aanvullendReglementOntwerp;
                relatie:doel ?mobiliteitsmaatregelOntwerp.
              ?relHeeftOntwerp a onderdeel:HeeftOntwerp ;
                relatie:bron ?ontwerpVerkeersteken;
                relatie:doel ?aanvullendReglementOntwerp.
            }
                UNION
            {
              ?relHeeftOntwerp a onderdeel:HeeftOntwerp ;
                relatie:bron ?ontwerpVerkeersteken;
                relatie:doel ${sparqlEscapeUri(uri)}.
            }
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
  const queryResult = await querySudo(graphQuery);
  const adminUnitUuid = queryResult.results.bindings[0]?.adminUnitUuid.value;
  if (!adminUnitUuid) return;
  const graph = `http://mu.semte.ch/graphs/awv/ldes/${adminUnitUuid}`;
  const moveQuery = `
    DELETE {
      GRAPH ${sparqlEscapeUri(graph)} {
        ?s ?pOld ?oOld.
      }
    }
    INSERT {
      GRAPH ${sparqlEscapeUri(graph)} {
        ${sparqlEscapeUri(uri)} ?a ?b
      }
    } WHERE {
      GRAPH ${LDES_GRAPH} {
        ${sparqlEscapeUri(uri)} ?a ?b
      }
      OPTIONAL {
        GRAPH ${sparqlEscapeUri(graph)} {
          ?s ?pOld ?oOld.
        }
      }
    }
  `;
  await updateSudo(moveQuery);
  const queryUrisToMove = `
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
    PREFIX relatie: <https://wegenenverkeer.data.vlaanderen.be/ns/implementatieelement#RelatieObject.>
    PREFIX onderdeel: <https://wegenenverkeer.data.vlaanderen.be/ns/onderdeel#>
    PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
    select distinct ?uriToMove where {
     GRAPH ${LDES_GRAPH} {
        ?uriToMove a onderdeel:HeeftVerkeersteken ;
              relatie:doel ${sparqlEscapeUri(uri)}.
      }
      FILTER NOT EXISTS {
        GRAPH ${sparqlEscapeUri(graph)} {
          ?uriToMove ?pred ?obj.
        }
      }
    }
  `;
  const moveQueryResult = await querySudo(queryUrisToMove);
  const urisToMove = moveQueryResult.results.bindings.map(
    (binding) => binding.uriToMove.value
  );
  for (const uriToMove of urisToMove) {
    await moveHeeftVerkeersteken(uriToMove);
  }
}

const verkeerstekenQuery = `
  {
    ?relWordtAangeduidDoor a onderdeel:WordtAangeduidDoor;
      relatie:doel ?verkersteken;
        relatie:bron ?mobiliteitsmaatregelOntwerp.
    ?relBevatMaatregelOntwerp a onderdeel:BevatMaatregelOntwerp ;
      relatie:bron ?aanvullendReglementOntwerp;
      relatie:doel ?mobiliteitsmaatregelOntwerp.
    ?relHeeftOntwerp a onderdeel:HeeftOntwerp ;
      relatie:bron ?ontwerpVerkeersteken;
      relatie:doel ?aanvullendReglementOntwerp.
  }
      UNION
  {
    ?relHeeftOntwerp a onderdeel:HeeftOntwerp ;
      relatie:bron ?ontwerpVerkeersteken;
      relatie:doel ?verkersteken.
  }
  ?relBevatVerkeersteken a onderdeel:BevatVerkeersteken ;
    relatie:bron ?signalisatieOntwerp;
    relatie:doel ?ontwerpVerkeersteken.
  ?relHeeftBetrokkene a onderdeel:HeeftBetrokkene ;
    relatie:bron ?signalisatieOntwerp;
    relatie:doel ?ovoUri.
`;

async function moveHeeftVerkeersteken(uri: string) {
  const graphQuery = `
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
    PREFIX relatie: <https://wegenenverkeer.data.vlaanderen.be/ns/implementatieelement#RelatieObject.>
    PREFIX onderdeel: <https://wegenenverkeer.data.vlaanderen.be/ns/onderdeel#>
     PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
    select distinct ?adminUnitUuid where {
          GRAPH ${LDES_GRAPH} {
            ${sparqlEscapeUri(uri)} a onderdeel:HeeftVerkeersteken ;
              relatie:doel ?verkersteken.
            ${verkeerstekenQuery}
          }
          GRAPH ${PUBLIC_GRAPH} {
            ?adminUnit owl:sameAs ?ovoUri;
              mu:uuid ?adminUnitUuid.
          }
      }`;
  const queryResult = await querySudo(graphQuery);
  const adminUnitUuid = queryResult.results.bindings[0]?.adminUnitUuid.value;
  if (!adminUnitUuid) return;
  const graph = `http://mu.semte.ch/graphs/awv/ldes/${adminUnitUuid}`;
  const moveQuery = `
    DELETE {
      GRAPH ${sparqlEscapeUri(graph)} {
        ?s ?pOld ?oOld.
      }
    }
    INSERT {
      GRAPH ${sparqlEscapeUri(graph)} {
        ${sparqlEscapeUri(uri)} ?a ?b
      }
    } WHERE {
      GRAPH ${LDES_GRAPH} {
        ${sparqlEscapeUri(uri)} ?a ?b
      }
      OPTIONAL {
        GRAPH ${sparqlEscapeUri(graph)} {
          ?s ?pOld ?oOld.
        }
      }
    }
  `;
  await updateSudo(moveQuery);
  const queryUrisToMove = `
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
    PREFIX relatie: <https://wegenenverkeer.data.vlaanderen.be/ns/implementatieelement#RelatieObject.>
    PREFIX onderdeel: <https://wegenenverkeer.data.vlaanderen.be/ns/onderdeel#>
    PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
    select distinct ?uriToMove where {
     GRAPH ${LDES_GRAPH} {
        ${sparqlEscapeUri(uri)} a onderdeel:HeeftVerkeersteken ;
              relatie:bron ?uriToMove.
      }
      FILTER NOT EXISTS {
        GRAPH ${sparqlEscapeUri(graph)} {
          ?uriToMove ?pred ?obj.
        }
      }
    }
  `;
  const moveQueryResult = await querySudo(queryUrisToMove);
  const urisToMove = moveQueryResult.results.bindings.map(
    (binding) => binding.uriToMove.value
  );
  for (const uriToMove of urisToMove) {
    await moveVariableInstanceWithLiteralValue(uriToMove);
  }
}

async function moveVariableInstanceWithLiteralValue(uri: string) {
  const graphQuery = `
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
    PREFIX relatie: <https://wegenenverkeer.data.vlaanderen.be/ns/implementatieelement#RelatieObject.>
    PREFIX onderdeel: <https://wegenenverkeer.data.vlaanderen.be/ns/onderdeel#>
     PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
    select distinct ?adminUnitUuid where {
          GRAPH ${LDES_GRAPH} {
            ?relHeeftVerkeersteken a onderdeel:HeeftVerkeersteken ;
              relatie:bron ${sparqlEscapeUri(uri)};
              relatie:doel ?verkersteken.
            ${verkeerstekenQuery}
          }
          GRAPH ${PUBLIC_GRAPH} {
            ?adminUnit owl:sameAs ?ovoUri;
              mu:uuid ?adminUnitUuid.
          }
      }`;
  const queryResult = await querySudo(graphQuery);
  const adminUnitUuid = queryResult.results.bindings[0]?.adminUnitUuid.value;
  if (!adminUnitUuid) return;
  const graph = `http://mu.semte.ch/graphs/awv/ldes/${adminUnitUuid}`;
  const moveQuery = `
    DELETE {
      GRAPH ${sparqlEscapeUri(graph)} {
        ?s ?pOld ?oOld.
      }
    }
    INSERT {
      GRAPH ${sparqlEscapeUri(graph)} {
        ${sparqlEscapeUri(uri)} ?a ?b
      }
    } WHERE {
      GRAPH ${LDES_GRAPH} {
        ${sparqlEscapeUri(uri)} ?a ?b
      }
      OPTIONAL {
        GRAPH ${sparqlEscapeUri(graph)} {
          ?s ?pOld ?oOld.
        }
      }
    }
  `;
  await updateSudo(moveQuery);
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
  const moveQueryResult = await querySudo(queryUrisToMove);
  const urisToMove = moveQueryResult.results.bindings.map(
    (binding) => binding.uriToMove.value
  );
  for (const uriToMove of urisToMove) {
    await moveHeeftWaardeVoor(uriToMove);
  }
}

async function moveVariableInstanceWithResourceValue(uri: string) {
  const graphQuery = `
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
    PREFIX relatie: <https://wegenenverkeer.data.vlaanderen.be/ns/implementatieelement#RelatieObject.>
    PREFIX onderdeel: <https://wegenenverkeer.data.vlaanderen.be/ns/onderdeel#>
     PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
    select distinct ?adminUnitUuid where {
          GRAPH ${LDES_GRAPH} {
            ?relHeeftVerkeersteken a onderdeel:HeeftVerkeersteken ;
              relatie:bron ${sparqlEscapeUri(uri)};
              relatie:doel ?verkersteken.
            ${verkeerstekenQuery}
          }
          GRAPH ${PUBLIC_GRAPH} {
            ?adminUnit owl:sameAs ?ovoUri;
              mu:uuid ?adminUnitUuid.
          }
      }`;
  const queryResult = await querySudo(graphQuery);
  const adminUnitUuid = queryResult.results.bindings[0]?.adminUnitUuid.value;
  if (!adminUnitUuid) return;
  const graph = `http://mu.semte.ch/graphs/awv/ldes/${adminUnitUuid}`;
  const moveQuery = `
    DELETE {
      GRAPH ${sparqlEscapeUri(graph)} {
        ?s ?pOld ?oOld.
      }
    }
    INSERT {
      GRAPH ${sparqlEscapeUri(graph)} {
        ${sparqlEscapeUri(uri)} ?a ?b
      }
    } WHERE {
      GRAPH ${LDES_GRAPH} {
        ${sparqlEscapeUri(uri)} ?a ?b
      }
      OPTIONAL {
        GRAPH ${sparqlEscapeUri(graph)} {
          ?s ?pOld ?oOld.
        }
      }
    }
  `;
  await updateSudo(moveQuery);
}

async function moveHeeftWaardeVoor(uri: string) {
  const graphQuery = `
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
    PREFIX relatie: <https://wegenenverkeer.data.vlaanderen.be/ns/implementatieelement#RelatieObject.>
    PREFIX onderdeel: <https://wegenenverkeer.data.vlaanderen.be/ns/onderdeel#>
     PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
    select distinct ?adminUnitUuid where {
          GRAPH ${LDES_GRAPH} {
            ${sparqlEscapeUri(uri)} a onderdeel:HeeftWaardeVoor ;
              relatie:bron ?variableInstanceWithLiteralValue.
            ?relHeeftVerkeersteken a onderdeel:HeeftVerkeersteken ;
              relatie:bron ?variableInstanceWithLiteralValue;
              relatie:doel ?verkersteken.
            ${verkeerstekenQuery}
          }
          GRAPH ${PUBLIC_GRAPH} {
            ?adminUnit owl:sameAs ?ovoUri;
              mu:uuid ?adminUnitUuid.
          }
      }`;
  const queryResult = await querySudo(graphQuery);
  const adminUnitUuid = queryResult.results.bindings[0]?.adminUnitUuid.value;
  if (!adminUnitUuid) return;
  const graph = `http://mu.semte.ch/graphs/awv/ldes/${adminUnitUuid}`;
  const moveQuery = `
    DELETE {
      GRAPH ${sparqlEscapeUri(graph)} {
        ?s ?pOld ?oOld.
      }
    }
    INSERT {
      GRAPH ${sparqlEscapeUri(graph)} {
        ${sparqlEscapeUri(uri)} ?a ?b
      }
    } WHERE {
      GRAPH ${LDES_GRAPH} {
        ${sparqlEscapeUri(uri)} ?a ?b
      }
      OPTIONAL {
        GRAPH ${sparqlEscapeUri(graph)} {
          ?s ?pOld ?oOld.
        }
      }
    }
  `;
  await updateSudo(moveQuery);
}

async function moveVerkeersbordopstelling(uri: string) {
  const graphQuery = `
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
    PREFIX relatie: <https://wegenenverkeer.data.vlaanderen.be/ns/implementatieelement#RelatieObject.>
    PREFIX onderdeel: <https://wegenenverkeer.data.vlaanderen.be/ns/onderdeel#>
     PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
    select distinct ?adminUnitUuid where {
          GRAPH ${LDES_GRAPH} {
            ?rel a onderdeel:HeeftBetrokkene ;
              relatie:bron ${sparqlEscapeUri(uri)};
              relatie:doel ?ovoUri.
            
          }
          GRAPH ${PUBLIC_GRAPH} {
            ?adminUnit owl:sameAs ?ovoUri;
              mu:uuid ?adminUnitUuid.
          }
      }`;
  const queryResult = await querySudo(graphQuery);
  const adminUnitUuid = queryResult.results.bindings[0]?.adminUnitUuid.value;
  if (!adminUnitUuid) return;
  const graph = `http://mu.semte.ch/graphs/awv/ldes/${adminUnitUuid}`;
  const moveQuery = `
    DELETE {
      GRAPH ${sparqlEscapeUri(graph)} {
        ?s ?pOld ?oOld.
      }
    }
    INSERT {
      GRAPH ${sparqlEscapeUri(graph)} {
        ${sparqlEscapeUri(uri)} ?a ?b
      }
    } WHERE {
      GRAPH ${LDES_GRAPH} {
        ${sparqlEscapeUri(uri)} ?a ?b
      }
      OPTIONAL {
        GRAPH ${sparqlEscapeUri(graph)} {
          ?s ?pOld ?oOld.
        }
      }
    }
  `;
  await updateSudo(moveQuery);
}

async function moveHeeftBetrokkene(uri: string) {
  const graphQuery = `
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
    PREFIX relatie: <https://wegenenverkeer.data.vlaanderen.be/ns/implementatieelement#RelatieObject.>
    PREFIX onderdeel: <https://wegenenverkeer.data.vlaanderen.be/ns/onderdeel#>
     PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
    select distinct ?adminUnitUuid where {
          GRAPH ${LDES_GRAPH} {
            ${sparqlEscapeUri(uri)} a onderdeel:HeeftBetrokkene ;
              relatie:bron ?signalisatieOntwerp;
              relatie:doel ?ovoUri.
            
          }
          GRAPH ${PUBLIC_GRAPH} {
            ?adminUnit owl:sameAs ?ovoUri;
              mu:uuid ?adminUnitUuid.
          }
      }`;
  const queryResult = await querySudo(graphQuery);
  const adminUnitUuid = queryResult.results.bindings[0]?.adminUnitUuid.value;
  if (!adminUnitUuid) return;
  const graph = `http://mu.semte.ch/graphs/awv/ldes/${adminUnitUuid}`;
  const moveQuery = `
    DELETE {
      GRAPH ${sparqlEscapeUri(graph)} {
        ?s ?pOld ?oOld.
      }
    }
    INSERT {
      GRAPH ${sparqlEscapeUri(graph)} {
        ${sparqlEscapeUri(uri)} ?a ?b
      }
    } WHERE {
      GRAPH ${LDES_GRAPH} {
        ${sparqlEscapeUri(uri)} ?a ?b
      }
      OPTIONAL {
        GRAPH ${sparqlEscapeUri(graph)} {
          ?s ?pOld ?oOld.
        }
      }
    }
  `;
  await updateSudo(moveQuery);
}

async function moveIsGebaseerdOp(uri: string) {
  const graphQuery = `
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
    PREFIX relatie: <https://wegenenverkeer.data.vlaanderen.be/ns/implementatieelement#RelatieObject.>
    PREFIX onderdeel: <https://wegenenverkeer.data.vlaanderen.be/ns/onderdeel#>
     PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
    select distinct ?adminUnitUuid where {
          GRAPH ${LDES_GRAPH} {
           ${sparqlEscapeUri(uri)} a onderdeel:IsGebaseerdOp ;
              relatie:bron ?verkeersteken.
            ${verkeerstekenQuery}
            
          }
          GRAPH ${PUBLIC_GRAPH} {
            ?adminUnit owl:sameAs ?ovoUri;
              mu:uuid ?adminUnitUuid.
          }
      }`;
  const queryResult = await querySudo(graphQuery);
  const adminUnitUuid = queryResult.results.bindings[0]?.adminUnitUuid.value;
  if (!adminUnitUuid) return;
  const graph = `http://mu.semte.ch/graphs/awv/ldes/${adminUnitUuid}`;
  const moveQuery = `
    DELETE {
      GRAPH ${sparqlEscapeUri(graph)} {
        ?s ?pOld ?oOld.
      }
    }
    INSERT {
      GRAPH ${sparqlEscapeUri(graph)} {
        ${sparqlEscapeUri(uri)} ?a ?b
      }
    } WHERE {
      GRAPH ${LDES_GRAPH} {
        ${sparqlEscapeUri(uri)} ?a ?b
      }
      OPTIONAL {
        GRAPH ${sparqlEscapeUri(graph)} {
          ?s ?pOld ?oOld.
        }
      }
    }
  `;
  await updateSudo(moveQuery);
}
