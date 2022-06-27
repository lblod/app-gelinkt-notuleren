import {generateReportFromData} from '../helpers.js'
import { querySudo as query } from '@lblod/mu-auth-sudo';

/**
 *
 * Function which generates a report containing the number of created, signed and published documents,
 * grouped by administration and month
 *
 * @param {Date} start starting date from which the report should be created
 * @param {Date} end ending date from which the report should be created
 */
export default async function generateReport(
	start,
	end,
	metadata = {
		title: "Statistieken",
		description:
			"Statistieken over het aantal aangemaakte, ondertekende en gepubliceerde documenten",
		filePrefix: "statistieken",
	}
) {

	const queryString = `
  PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
  PREFIX besluit: <http://data.vlaanderen.be/ns/besluit#>
  PREFIX dct: <http://purl.org/dc/terms/>
  PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
  PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
  PREFIX sign: <http://mu.semte.ch/vocabularies/ext/signing/>
  PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
  PREFIX pav: <http://purl.org/pav/>
  PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
  SELECT ?bestuurseenheid 
        ?bestuurseenheidPrefix
        ?month
        ?year
        (count(distinct ?agendapunt) as ?aantalAgendapunten)
        (count(distinct ?zitting) as ?aantalZittingen)
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
    {
      GRAPH ?graph {
          ?agendapunt a ext:DocumentContainer .
          ?agendapunt ext:editorDocumentFolder <http://mu.semte.ch/application/editor-document-folder/ae5feaed-7b70-4533-9417-10fbbc480a4c> .
      }
      ?agendapunt pav:hasCurrentVersion ?currentVersion .
      ?currentVersion pav:previousVersion*/pav:createdOn ?timestamp .
      filter not exists {
        ?currentVersion pav:previousVersion*/pav:createdOn ?timestamp2
        filter (?timestamp > ?timestamp2)
      } 
    }
    UNION
    {
      GRAPH ?graph {
          ?zitting a besluit:Zitting .
          ?zitting besluit:geplandeStart ?timestamp .
      }  
    }
    UNION
    {
      GRAPH ?graph {
        ?signedResource a sign:SignedResource .
        ?signedResource dct:created ?timestamp .
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
      }   
    }
    UNION
    {
      GRAPH ?graph {
        ?publishedResource a sign:PublishedResource .
        ?publishedResource dct:created ?timestamp .
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
    FILTER(!bound(?timestamp) || (?timestamp > "${start.toISOString()}"^^xsd:dateTime && ?timestamp < "${end.toISOString()}"^^xsd:dateTime))  

  } 
  GROUP BY ?bestuurseenheid ?bestuurseenheidPrefix (month(?timestamp) as ?month) (year(?timestamp) as ?year)
  `;

	const queryResponse = await query(queryString);
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
			maand: getValue(entry, "month"),
			jaar: getValue(entry, "year"),
		};
	});

	await generateReportFromData(
		data,
		[
			"bestuurseenheid",
			"maand",
			"jaar",
			"aantalZittingen",
			"aantalAgendapunten",
			"aantalOndertekendeNotulen",
			"aantalOndertekendeAgendas",
			"aantalOndertekendeBesluitenLijsten",
			"aantalOndertekendeBehandelingen",
			"aantalGepubliceerdeNotulen",
			"aantalGepubliceerdeAgendas",
			"aantalGepubliceerdeBesluitenLijsten",
			"aantalGepubliceerdeBehandelingen",
		],
		metadata
	);
}

function getValue(entry, property, defaultValue = 0) {
	return entry[property] ? entry[property].value : defaultValue;
}
