import generateReport from "./reportGenerator.js";
import { MONTHS } from "./utils/constants.js";
import { getToday } from "./utils/date.js";
import { generateReportFromData } from "../helpers.js";
import { querySudo as query } from "@lblod/mu-auth-sudo";

export default {
  cronPattern: "0 0 0 * * *",
  name: "ivReport",
  execute: async () => {
    const { day, month, year } = getToday();
    const metadata = {
      title: `IV Statistieken ${day} ${MONTHS[month]} ${year}`,
      description:
        "Dagelijkse statistieken over het aantal creaties en synchronisaties van installatievergaderingen per bestuursorgaan",
      filePrefix: `iv-statistieken-${day}-${month + 1}-${year}`,
    };
    await generateIvReport(metadata);
  },
};

async function generateIvReport(metadata) {
  const queryString = `
    PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
    PREFIX besluit: <http://data.vlaanderen.be/ns/besluit#>
    PREFIX mandaat: <http://data.vlaanderen.be/ns/mandaat#>
    PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
    SELECT ?adminUnitName (COUNT(DISTINCT ?ivMeeting) as ?installatieVergaderingCount) (COUNT(DISTINCT ?syncStatusSuccess) as ?installatieVergaderingSyncedCount) (COUNT(DISTINCT ?syncStatusFailed) as ?installatieVergaderingFailedSyncCount)  WHERE {
      ?ivMeeting a ext:Installatievergadering;
        besluit:isGehoudenDoor ?adminUnit.
      ?adminUnit mandaat:isTijdspecialisatieVan ?adminUnitSpecialised.
      ?adminUnitSpecialised skos:prefLabel ?adminUnitName.
      OPTIONAL {
        ?ivMeeting ext:synchronizationStatus ?syncStatusSuccess.
        ?syncStatusSuccess ext:success "true"^^<http://mu.semte.ch/vocabularies/typed-literals/boolean>
      }
      OPTIONAL {
        ?ivMeeting ext:synchronizationStatus ?syncStatusFailed.
        ?syncStatusFailed ext:success "false"^^<http://mu.semte.ch/vocabularies/typed-literals/boolean>
      }
    } 
  `;

  const queryResponse = await query(queryString);
  const data = queryResponse.results.bindings.map((entry) => {
    return {
      adminUnitName: getValue(entry, "adminUnitName"),
      installatieVergaderingCount: getValue(
        entry,
        "installatieVergaderingCount"
      ),
      installatieVergaderingSyncedCount: getValue(
        entry,
        "installatieVergaderingSyncedCount"
      ),
      installatieVergaderingFailedSyncCount: getValue(
        entry,
        "installatieVergaderingFailedSyncCount"
      ),
    };
  });

  await generateReportFromData(
    data,
    [
      "adminUnitName",
      "installatieVergaderingCount",
      "installatieVergaderingSyncedCount",
      "installatieVergaderingFailedSyncCount",
    ],
    metadata
  );
}

function getValue(entry, property, defaultValue = 0) {
  return entry[property] ? entry[property].value : defaultValue;
}
