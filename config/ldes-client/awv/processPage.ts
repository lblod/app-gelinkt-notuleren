import { querySudo, updateSudo } from "@lblod/mu-auth-sudo";
import { uuid as generateUuid, sparqlEscapeString, sparqlEscapeUri } from "mu";
import { logger } from "../../logger";
import { moveAanvullendReglementOntwerp } from "./resourceHandlers/aanvullendReglementontwerp";
import { moveBevatMaatregelOntwerp } from "./resourceHandlers/bevatMaatregelOntwerp";
import { moveBevatVerkeersteken } from "./resourceHandlers/bevatVerkeersteken";
import { moveHeeftBetrokkene } from "./resourceHandlers/heeftBetrokkene";
import { moveHeeftOntwerp } from "./resourceHandlers/heeftOntwerp";
import { moveHeeftVerkeersteken } from "./resourceHandlers/heeftVerkeersteken";
import { moveIsGebaseerdOp } from "./resourceHandlers/isGebaseerdOp";
import { moveMobiliteitsmaatregelOntwerp } from "./resourceHandlers/mobiliteitsmaatregelOntwerp";
import { moveOntwerpVerkeersteken } from "./resourceHandlers/ontwerpVerkeersteken";
import { moveSignalisatieOntwerp } from "./resourceHandlers/signalisatieOntwerp";
import { moveVariableInstanceWithLiteralValue } from "./resourceHandlers/variableInstanceWithLiteralValue";
import { moveVariableInstanceWithResourceValue } from "./resourceHandlers/variableInstanceWithResourceValue";
import { moveVerkeersbordopstelling } from "./resourceHandlers/verkeersbordopstelling";
import { moveVerkeersbordVerkeersteken } from "./resourceHandlers/verkeersbordVerkeersteken";
import { moveWordtAangeduidDoor } from "./resourceHandlers/wordtAangeduidDoor";
import {
  BATCH_GRAPH,
  LDES_GRAPH,
  SUDO_OPTIONS,
  VERSION_OF,
} from "./utils/constants";

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
      GRAPH ${BATCH_GRAPH} {
        ?stream <https://w3id.org/tree#member> ?versionedMember .
        ?versionedMember ${VERSION_OF} ?s .
        ?versionedMember ?pNew ?oNew.
        FILTER (?pNew NOT IN ( ${VERSION_OF} ))
      }
      OPTIONAL {
        GRAPH ${LDES_GRAPH} {
          ?s ?pOld ?oOld.
          FILTER (?pOld NOT IN ( mu:uuid))
        }
      }
    }`,
    {},
    SUDO_OPTIONS,
  );
  const subjectsQuery = await querySudo<{ s: string }>(
    `
      SELECT DISTINCT ?s WHERE {
	GRAPH ${BATCH_GRAPH} {
	  ?stream <https://w3id.org/tree#member> ?versionedMember .
	  ?versionedMember ${VERSION_OF} ?s .
	}
      }
    `,
    {},
    SUDO_OPTIONS,
  );
  const subjects = subjectsQuery.results.bindings.map(
    (binding) => binding.s.value,
  );

  await generateUuids();

  const urisWithType = await Promise.all([...subjects].map(mapUriToType));
  await moveByType(urisWithType);
  logger.info("finished regular processing, switching to leftovers");

  const queryStr = `
    SELECT DISTINCT ?s WHERE {
      GRAPH ${LDES_GRAPH} {
	?s ?p ?v.
      }
      FILTER NOT EXISTS {
	GRAPH ?g {
	  ?s ?p ?v.
	}
	FILTER (?g != ${LDES_GRAPH})
      }
    }
    `;
  logger.info(queryStr);
  const leftOverSubjectsQuery = await querySudo<{ s: string }>(queryStr);

  const leftOverSubjects = leftOverSubjectsQuery.results.bindings.map(
    (binding) => binding.s.value,
  );
  logger.info("found leftover subjects", leftOverSubjects);

  const leftOverUrisWithType = await Promise.all(
    [...subjects].map(mapUriToType),
  );
  await moveByType(leftOverUrisWithType);
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
    SUDO_OPTIONS,
  );
  const subjectsWithUuid = subjectsWithoutUuidsQuery.results.bindings.map(
    (binding) => ({ subject: binding.s.value, uuid: generateUuid() }),
  );
  if (!subjectsWithUuid.length) return;

  await updateSudo(
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
    SUDO_OPTIONS,
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
    SUDO_OPTIONS,
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
