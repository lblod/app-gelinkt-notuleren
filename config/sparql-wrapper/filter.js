import { query, sparqlEscapeUri } from 'mu';


export async function isAuthorized(sessionUri) {
  const checkSessionQuery = `
    PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
    PREFIX mu: <http://mu.semte.ch/vocabularies/core/>

    SELECT DISTINCT ?session_group WHERE {
      ${sparqlEscapeUri(sessionUri)} ext:sessionGroup/mu:uuid ?session_group;
                     ext:sessionRole ?role.
     FILTER(?role in (\"GelinktNotuleren-lezer\",\"GelinktNotuleren-schrijver\", \"GelinktNotuleren-publiceerder\",  \"GelinktNotuleren-ondertekenaar\"))
     }
  `;
  const response = await query(checkSessionQuery, { sudo: true});
  // We want exactly one result, only one session should exist at a certain time.
  const exists = response.results?.bindings?.length === 1;
  console.log(`${sessionUri} exists? ${exists}`);
  return exists;
}
