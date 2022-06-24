import { generateReportFromData } from "../helpers.js";
import { querySudo as query } from "@lblod/mu-auth-sudo";

export default {
	cronPattern: "0 0 23 L * ?",
	name: "monthlyReport",
	execute: async () => {
		const reportData = {
			title: "Maandelijkse statistieken over het aantal aangemaakte, ondertekende en gepubliceerde documenten",
			description:
				"Monthly statistics on the number of created, signed and published documents",
			filePrefix: "monthlyReport",
		};

		const queryString = `
      PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
      PREFIX besluit: <http://data.vlaanderen.be/ns/besluit#>
      PREFIX dct: <http://purl.org/dc/terms/>
      PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
      PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
      PREFIX sign: <http://mu.semte.ch/vocabularies/ext/signing/>
      PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
      SELECT DISTINCT ?bestuurseenheid 
                      ?bestuurseenheidPrefix 
                      (count(distinct ?zitting) as ?aantalZittingen)
                      (count(distinct ?agendapunt) as ?aantalAgendapunten)
                      (count(distinct ?signedNotulen) as ?aantalOndertekendeNotulen)
                      (count(distinct ?signedAgenda) as ?aantalOndertekendeAgendas)
                      (count(distinct ?signedBesluitenLijst) as ?aantalOndertekendeBesluitenLijsten)
                      (count(distinct ?signedBehandeling) as ?aantalOndertekendeBehandelingen)
                      (count(distinct ?publishedNotulen) as ?aantalGepubliceerdeNotulen)
                      (count(distinct ?publishedAgenda) as ?aantalGepubliceerdeAgendas)
                      (count(distinct ?publishedBesluitenLijst) as ?aantalGepubliceerdeBesluitenLijsten)
                      (count(distinct ?publishedBehandeling) as ?aantalGepubliceerdeBehandelingen)

      WHERE {
        GRAPH <http://mu.semte.ch/graphs/public> {
          ?eenheid a besluit:Bestuurseenheid;
                  mu:uuid ?bestuurseenheidUUID.
          OPTIONAL {
            ?eenheid skos:prefLabel ?bestuurseenheid.
          }
          OPTIONAL {
            ?eenheid besluit:classificatie ?classificatie.
            ?classificatie skos:prefLabel ?bestuurseenheidPrefix.
          }
        }
        BIND(IRI(CONCAT("http://mu.semte.ch/graphs/organizations/", ?bestuurseenheidUUID)) as ?graph)
        GRAPH ?graph {
          OPTIONAL {
            ?zitting a besluit:Zitting .
          }
          OPTIONAL {
            ?agendapunt a besluit:Agendapunt .
          }
          OPTIONAL {
            ?signedResource a sign:SignedResource .
          }
          OPTIONAL {
            ?signedResource ext:signsNotulen ?signedNotulen .
          }
          OPTIONAL {
            ?signedResource ext:signsAgenda ?signedAgenda .
          }
          OPTIONAL {
            ?signedResource ext:signsBesluitenLijst ?signedBesluitenLijst .
          }
          OPTIONAL {
            ?signedResource ext:signsBehandeling ?signedBehandeling .
          }
          OPTIONAL {
            ?publishedResource a sign:PublishedResource .
          }
          OPTIONAL {
            ?publishedResource ext:publishesNotulen ?publishedNotulen .
          }
          OPTIONAL {
            ?publishedResource ext:publishesAgenda ?publishedAgenda .
          }
          OPTIONAL {
            ?publishedResource ext:publishesBesluitenlijst ?publishedBesluitenLijst .
          }
          OPTIONAL {
            ?publishedResource ext:publishesBehandeling ?publishedBehandeling .
          }
        }
      }
    `;

		const queryResponse = await query(queryString);
		console.log(queryResponse);
		const data = queryResponse.results.bindings.map((entry) => {
			return {
				aantalZittingen: getValue(entry, "aantalZittingen"),
				aantalAgendapunten: getValue(entry, "aantalAgendapunten"),
				aantalOndertekendeNotulen: getValue(
					entry,
					"aantalOndertekendeNotulen"
				),
				aantalOndertekendeAgendas: getValue(
					entry,
					"aantalOndertekendeAgendas"
				),
				aantalOndertekendeBesluitenLijsten: getValue(
					entry,
					"aantalOndertekendeBesluitenLijsten"
				),
				aantalOndertekendeBehandelingen: getValue(
					entry,
					"aantalOndertekendeBehandelingen"
				),
				aantalGepubliceerdeNotulen: getValue(
					entry,
					"aantalGepubliceerdeNotulen"
				),
				aantalGepubliceerdeAgendas: getValue(
					entry,
					"aantalGepubliceerdeAgendas"
				),
				aantalGepubliceerdeBesluitenLijsten: getValue(
					entry,
					"aantalGepubliceerdeBesluitenLijsten"
				),
				aantalGepubliceerdeBehandelingen: getValue(
					entry,
					"aantalGepubliceerdeBehandelingen"
				),
				bestuurseenheid:
					entry.bestuurseenheidPrefix && entry.bestuurseenheid
						? `${entry.bestuurseenheidPrefix.value} ${entry.bestuurseenheid.value}`
						: "",
			};
		});

		await generateReportFromData(
			data,
			[
				"bestuurseenheid",
				"aantalZittingen",
				"aantalAgendapunten",
				"aantalOndertekendeNotulen",
				"aantalOndertekendeAgendas",
				"aantalOndertekendeBesluitenLijsten",
				"aantalOndertekendeBehandelingen",
				"aantalGepubliceerdeNotulen",
				"aantalGepubliceerdeAgendas",
				"aantalGepubliceerdeBesluitenLijsten",
        "aantalGepubliceerdeBehandelingen"
			],
			reportData
		);
	},
};

function getValue(entry, property, defaultValue = 0) {
	return entry[property] ? entry[property].value : defaultValue;
}
