import { logger } from "../../logger";
import { updateSudo, querySudo } from "@lblod/mu-auth-sudo";
import { sparqlEscapeUri, sparqlEscapeString, uuid as generateUuid } from "mu";
import { environment } from "../../environment";
import { moveIsGebaseerdOp } from "./resourceHandlers/isGebaseerdOp";
import { moveVerkeersbordopstelling } from "./resourceHandlers/verkeersbordopstelling";
import { moveVariableInstanceWithResourceValue } from "./resourceHandlers/variableInstanceWithResourceValue";
import { moveVariableInstanceWithLiteralValue } from "./resourceHandlers/variableInstanceWithLiteralValue";
import { moveHeeftVerkeersteken } from "./resourceHandlers/heeftVerkeersteken";
import { moveVerkeersbordVerkeersteken } from "./resourceHandlers/verkeersbordVerkeersteken";
import { moveWordtAangeduidDoor } from "./resourceHandlers/wordtAangeduidDoor";
import { moveMobiliteitsmaatregelOntwerp } from "./resourceHandlers/mobiliteitsmaatregelOntwerp";
import { moveBevatMaatregelOntwerp } from "./resourceHandlers/bevatMaatregelOntwerp";
import { moveAanvullendReglementOntwerp } from "./resourceHandlers/aanvullendReglementontwerp";
import { moveHeeftOntwerp } from "./resourceHandlers/heeftOntwerp";
import { moveOntwerpVerkeersteken } from "./resourceHandlers/ontwerpVerkeersteken";
import { moveBevatVerkeersteken } from "./resourceHandlers/bevatVerkeersteken";
import { moveSignalisatieOntwerp } from "./resourceHandlers/signalisatieOntwerp";
import { moveHeeftBetrokkene } from "./resourceHandlers/heeftBetrokkene";

export const LDES_GRAPH = "<http://mu.semte.ch/graphs/awv/ldes>";
export const PUBLIC_GRAPH = "<http://mu.semte.ch/graphs/public>";

export const sudoOptions = environment.BYPASS_MU_AUTH
  ? {
      sparqlEndpoint: environment.DIRECT_DATABASE_CONNECTION,
    }
  : {};

async function replaceExistingData() {
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
          environment.getVersionPredicate(),
        )} ?s .
        ?versionedMember ?pNew ?oNew.
        FILTER (?pNew NOT IN ( ${sparqlEscapeUri(
          environment.getVersionPredicate(),
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
    sudoOptions,
  );
  const subjectsQuery = await querySudo<{ s: string }>(
    `
      SELECT DISTINCT ?s WHERE {
          GRAPH ${sparqlEscapeUri(environment.BATCH_GRAPH)} {
            ?stream <https://w3id.org/tree#member> ?versionedMember .
            ?versionedMember ${sparqlEscapeUri(
              environment.getVersionPredicate(),
            )} ?s .
          }
      }
    `,
    {},
    sudoOptions,
  );
  const subjects = subjectsQuery.results.bindings.map(
    (binding) => binding.s.value,
  );

  await generateUuids();

  const urisWithType = await Promise.all([...subjects].map(mapUriToType));
  await moveByType(urisWithType);
}

async function generateUuids() {
  const subjectsWithoutUuidsQuery = await querySudo<{ s: string }>(
    `
    PREFIX mu: <http://mu.semte.ch/vocabularies/core/>

    SELECT DISTINCT ?s WHERE {
      GRAPH ${LDES_GRAPH} {
        ?s a ?type
        FILTER NOT EXISTS {
          ?s mu:uuid ?uuid
        }
      }
    }
  `,
    {},
    sudoOptions,
  );
  const subjectsWithUuid = subjectsWithoutUuidsQuery.results.bindings.map(
    (binding) => ({ subject: binding.s.value, uuid: generateUuid() }),
  );
  if (!subjectsWithUuid.length) return;

  const insertUuids = await updateSudo(
    `
    PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
    INSERT DATA {
      GRAPH ${LDES_GRAPH} {
        ${subjectsWithUuid
          .map(
            (subjects) =>
              `${sparqlEscapeUri(
                subjects.subject,
              )} mu:uuid ${sparqlEscapeString(subjects.uuid)}`,
          )
          .join(". \n")}
      }
    }
  `,
    {},
    sudoOptions,
  );
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
  const queryResult = await querySudo<{ type: string }>(
    signUriQuery,
    {},
    sudoOptions,
  );
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

export const verkeerstekenQuery = `
  {
    ?relWordtAangeduidDoor a onderdeel:WordtAangeduidDoor;
      relatie:doel ?verkeersteken;
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
      relatie:doel ?verkeersteken.
  }
  ?relBevatVerkeersteken a onderdeel:BevatVerkeersteken ;
    relatie:bron ?signalisatieOntwerp;
    relatie:doel ?ontwerpVerkeersteken.
  ?relHeeftBetrokkene a onderdeel:HeeftBetrokkene ;
    relatie:bron ?signalisatieOntwerp;
    relatie:doel ?ovoUri.
`;
