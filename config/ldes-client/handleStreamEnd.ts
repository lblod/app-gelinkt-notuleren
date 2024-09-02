import { updateSudo, querySudo } from '@lblod/mu-auth-sudo';
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

const PAGE_SIZE = 500

async function moveToPublic(type) {
    const subjectQuery = `
        SELECT ?subject WHERE {
            GRAPH <http://mu.semte.ch/graphs/mandaten-staging> {
                ?subject a ${type}.
            }
        }
    `
    const response = await querySudo(subjectQuery, {}, {sparqlEndpoint: "http://virtuoso:8890/sparql"});

    const subjects = response.results.bindings.map((triple) => `<${triple.subject.value}>`)

   

    for(let i = 0; i < subjects.length; i+=PAGE_SIZE) {

        const deleteOldData = `
            DELETE {
                GRAPH <http://mu.semte.ch/graphs/lmb-data-public> {
                    ?subject a ${type};
                        ?predicate ?object.
                }
            }WHERE {
                GRAPH <http://mu.semte.ch/graphs/lmb-data-public> {
                    ?subject a ${type};
                        ?predicate ?object.
                }
                VALUES ?subject {
                    ${subjects.slice(i, i+PAGE_SIZE).join(' ')}
                }
            }
        `
        await updateSudo(deleteOldData);

        const query= `
            DELETE {
                GRAPH <http://mu.semte.ch/graphs/mandaten-staging> {
                    ?subject a ${type};
                        ?predicate ?object.
                }
            }INSERT {
                GRAPH <http://mu.semte.ch/graphs/lmb-data-public> {
                    ?subject a ${type};
                        ?predicate ?object.
                }
            }WHERE {
                GRAPH <http://mu.semte.ch/graphs/mandaten-staging> {
                    ?subject a ${type};
                        ?predicate ?object.
                }
                VALUES ?subject {
                    ${subjects.slice(i, i+PAGE_SIZE).join(' ')}
                }
            }
        `
        await updateSudo(query);

    }
    
}

export async function handleStreamEnd(){
    console.log("Stream ended");
    await moveToPublic('<http://data.vlaanderen.be/ns/mandaat#Mandataris>')
    await moveToPublic('<http://data.vlaanderen.be/ns/mandaat#Fractie>')
    await moveToPublic('<http://www.w3.org/ns/org#Membership>')

    const personPublicSubjectQuery = `
        ${PREFIXES}
        SELECT ?subject WHERE {
            GRAPH <http://mu.semte.ch/graphs/mandaten-staging> {
                ?subject a person:Person.
            }
        }
    `
    const responsePublic = await querySudo(personPublicSubjectQuery, {}, {sparqlEndpoint: "http://virtuoso:8890/sparql"});

    const subjectsPublic = responsePublic.results.bindings.map((triple) => `<${triple.subject.value}>`)

    for(let i = 0; i < subjectsPublic.length; i+=PAGE_SIZE) {

        const personPublicDeleteOldDataQuery = `
            ${PREFIXES}
            DELETE {
                GRAPH <http://mu.semte.ch/graphs/lmb-data-public> {
                    ?person a person:Person;
                        ?predicatePerson ?objectPerson.
                }

            } WHERE {
                GRAPH <http://mu.semte.ch/graphs/lmb-data-public> {
                    ?person a person:Person;
                        ?predicatePerson ?objectPerson.
                }
                VALUES ?predicatePerson{
                    dct:modified
                    mu:uuid
                    foaf:familyName
                    persoon:gebruikteVoornaam
                }   
                VALUES ?person {
                    ${subjectsPublic.slice(i, i+PAGE_SIZE).join(' ')}
                }
            }
        `
        await updateSudo(personPublicDeleteOldDataQuery);
        const personPublicQuery = `
            ${PREFIXES}
            INSERT {
                GRAPH <http://mu.semte.ch/graphs/lmb-data-public> {
                    ?person a person:Person;
                        ?predicatePerson ?objectPerson.
                }
            }WHERE {
                GRAPH <http://mu.semte.ch/graphs/mandaten-staging> {
                    ?person a person:Person;
                        ?predicatePerson ?objectPerson.
                }
                VALUES ?predicatePerson{
                    dct:modified
                    mu:uuid
                    foaf:familyName
                    persoon:gebruikteVoornaam
                }   
                VALUES ?person {
                    ${subjectsPublic.slice(i, i+PAGE_SIZE).join(' ')}
                }
            }
        
        `
        await updateSudo(personPublicQuery);
        

        
    }

    // We move to the person staging because we want to keep all the data, but not influence future public person migration
    // Person in mandaten staging graph = waiting to be moved to lmb-public
    // Person in person staging graph = waiting to be moved to lmb-private
    const moveIdentifiersToPersonStagingQuery = `
    ${PREFIXES}
        DELETE {
            GRAPH <http://mu.semte.ch/graphs/mandaten-staging> {
                ?identifier ?identifierPredicate ?identifierObject.
            } 
        } INSERT {
            GRAPH <http://mu.semte.ch/graphs/person-staging> {
                ?identifier ?identifierPredicate ?identifierObject.
            }
        }WHERE {
            GRAPH <http://mu.semte.ch/graphs/mandaten-staging> {
                ?person a person:Person.
                ?person adms:identifier ?identifier.
                ?identifier ?identifierPredicate ?identifierObject.
            } 
        }
    `
    const moveBirthdaysToPersonStagingQuery = `
    ${PREFIXES}
        DELETE {
            GRAPH <http://mu.semte.ch/graphs/mandaten-staging> {
                ?birthdate ?birthdatePredicate ?birthdateObject.
            } 
        } INSERT {
            GRAPH <http://mu.semte.ch/graphs/person-staging> {
                ?birthdate ?birthdatePredicate ?birthdateObject.
            }
        }WHERE {
            GRAPH <http://mu.semte.ch/graphs/mandaten-staging> {
                ?person a person:Person;
                ?person persoon:heeftGeboorte ?birthdate.
                ?birthdate ?birthdatePredicate ?birthdateObject.
            } 
        }
    `
    const moveToPersonStagingQuery = `
        ${PREFIXES}
        DELETE {
            GRAPH <http://mu.semte.ch/graphs/mandaten-staging> {
                ?person a person:Person;
                    ?predicatePerson ?objectPerson.
              } 
        } INSERT {
            GRAPH <http://mu.semte.ch/graphs/person-staging> {
                ?person a person:Person;
                    ?predicatePerson ?objectPerson.
            }
        }WHERE {
            GRAPH <http://mu.semte.ch/graphs/mandaten-staging> {
                ?person a person:Person;
                    ?predicatePerson ?objectPerson.
            } 
        }

    `
    await updateSudo(moveToPersonStagingQuery, {}, {sparqlEndpoint: "http://virtuoso:8890/sparql"});

    const personPrivateSubjectQuery = `
        ${PREFIXES}
        SELECT ?subject WHERE {
            GRAPH <http://mu.semte.ch/graphs/person-staging> {
                    ?subject a person:Person;
                        ?predicatePerson ?objectPerson.
                    ?subject persoon:heeftGeboorte ?birthdate.
                    ?birthdate ?birthdatePredicate ?birthdateObject.
                    OPTIONAL {
                        ?subject adms:identifier ?identifier.
                        ?identifier ?identifierPredicate ?identifierObject.
                    }
                }
                GRAPH <http://mu.semte.ch/graphs/lmb-data-public> {
                    ?mandataris mandaat:isBestuurlijkeAliasVan ?subject.
                    
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
    const responsePrivate = await querySudo(personPrivateSubjectQuery, {}, {sparqlEndpoint: "http://virtuoso:8890/sparql"});

    const subjectsPrivate = responsePrivate.results.bindings.map((triple) => `<${triple.subject.value}>`)

    for(let i = 0; i < subjectsPrivate.length; i+=PAGE_SIZE) {

        const deleteOldDataPersonPrivateQuery = `
            ${PREFIXES}
            DELETE {
                GRAPH ?g {
                    ?person a person:Person;
                        ?predicatePerson ?objectPerson.
                    ?identifier ?identifierPredicate ?identifierObject.
                }

            } WHERE {
                GRAPH <http://mu.semte.ch/graphs/person-staging> {
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
                VALUES ?person {
                    ${subjectsPrivate.slice(i, i+PAGE_SIZE).join(' ')}
                }

            }
        
        `

        await updateSudo(deleteOldDataPersonPrivateQuery);

        const personPrivateQuery = `
            ${PREFIXES}
            DELETE {
                GRAPH <http://mu.semte.ch/graphs/person-staging> {
                    ?person a person:Person;
                        ?predicatePerson ?objectPerson.
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
                GRAPH <http://mu.semte.ch/graphs/person-staging> {
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
                VALUES ?person {
                    ${subjectsPrivate.slice(i, i+PAGE_SIZE).join(' ')}
                }

            }
        `
        await updateSudo(personPrivateQuery);
    }

    console.log('LDES postproccessed')
  }
