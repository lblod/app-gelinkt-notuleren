import { updateSudo } from '@lblod/mu-auth-sudo';
import { DIRECT_DATABASE_CONNECTION, GRAPH_STORE_URL, LDES_BASE, WORKING_GRAPH, FIRST_PAGE, CRON_PATTERN, LOG_LEVEL, TIME_PREDICATE, EXTRA_HEADERS } from './environment';

const PREFIXES = `
    PREFIX person: <http://www.w3.org/ns/person#>
    PREFIX persoon: <http://data.vlaanderen.be/ns/persoon#>
    PREFIX dct: <http://purl.org/dc/terms/>
    PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
    PREFIX foaf: <http://xmlns.com/foaf/0.1/>
    PREFIX adms: <http://www.w3.org/ns/adms#>
    PREFIX org: <http://www.w3.org/ns/org#>
    PREFIX mandaat: <http://data.vlaanderen.be/ns/mandaat#>
    PREFIX besluit: <http://data.vlaanderen.be/ns/besluit#>
`

async function moveToPublic(type) {
    const query= `
        DELETE {
            GRAPH <http://mu.semte.ch/graphs/mandaten-staging> {
                ?mandataris a ${type};
                    ?predicateMandataris ?objectManadataris.
            }
        }INSERT {
            GRAPH <http://mu.semte.ch/graphs/lmb-data-public> {
                ?mandataris a ${type};
                    ?predicateMandataris ?objectManadataris.
            }
        }WHERE {
            GRAPH <http://mu.semte.ch/graphs/mandaten-staging> {
                ?mandataris a ${type};
                    ?predicateMandataris ?objectManadataris.
            }

        }
    `
    await updateSudo(query);
}

export async function handleStreamEnd(){
    console.log("Stream ended");
    await moveToPublic('<http://data.vlaanderen.be/ns/mandaat#Mandataris>')
    await moveToPublic('<http://data.vlaanderen.be/ns/mandaat#Fractie>')
    await moveToPublic('<http://www.w3.org/ns/org#Membership>')

    const personPublicQuery = `
        ${PREFIXES}
        INSERT {
            GRAPH <http://mu.semte.ch/graphs/lmb-data-public> {
                ?person a person:Person;
                    ?predicatePerson ?objectPerson.
                ?person persoon:heeftGeboorte ?birthdate.
                ?birthdate ?birthdatePredicate ?birthdateObject.
            }
        }WHERE {
            GRAPH <http://mu.semte.ch/graphs/mandaten-staging> {
                ?person a person:Person;
                    ?predicatePerson ?objectPerson.
                ?person persoon:heeftGeboorte ?birthdate.
                ?birthdate ?birthdatePredicate ?birthdateObject.
            }
            VALUES ?predicatePerson{
                dct:modified
                mu:uuid
                foaf:familyName
                persoon:gebruikteVoornaam
            }   

        }
    
    `
    await updateSudo(personPublicQuery);
    const personQuery = `
        ${PREFIXES}
        DELETE {
            GRAPH <http://mu.semte.ch/graphs/mandaten-staging> {
                ?person a person:Person;
                    ?predicatePerson ?objectPerson.
                ?birthdate ?birthdatePredicate ?birthdateObject.
                ?identifier ?identifierPredicate ?identifierObject.
            }
        }INSERT {
            GRAPH ?g {
                ?person a person:Person;
                    ?predicatePerson ?objectPerson.
                ?birthdate ?birthdatePredicate ?birthdateObject.
                ?identifier ?identifierPredicate ?identifierObject.
            }
        }WHERE {
            GRAPH <http://mu.semte.ch/graphs/mandaten-staging> {
                ?person a person:Person;
                    ?predicatePerson ?objectPerson.
                ?person persoon:heeftGeboorte ?birthdate.
                ?birthdate ?birthdatePredicate ?birthdateObject.
                OPTIONAL {
                    ?person adms:identifier ?identifier.
                    ?identifier ?identifierPredicate ?identifierObject.
                }
            }
            GRAPH <http://mu.semte.ch/graphs/lmb-data-public> {
                ?mandataris mandaat:isBestuurlijkeAliasVan ?person.
                
                ?mandataris org:holds ?mandat.
                
            }
            GRAPH <http://mu.semte.ch/graphs/public> {
                ?temporaryBestuursorgan org:hasPost ?mandat;
                    mandaat:isTijdspecialisatieVan ?bestuursorgan.
                ?bestuursorgan besluit:bestuurt ?adminUnit.
                ?adminUnit mu:uuid ?adminUnitUuid.
            }
            BIND(IRI(CONCAT("http://mu.semte.ch/graphs/lmb-data-private/", ?adminUnitUuid)) AS ?g)

        }
    `
    await updateSudo(personQuery);
    console.log('LDES postproccessed')
  }
