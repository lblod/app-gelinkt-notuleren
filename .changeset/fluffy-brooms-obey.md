---
"app-gelinkt-notuleren": patch
---

bump prepublisher service

This bump adds a hack for very large meetings: if an 
`ext:optimizeSpaces "true"^^<http://mu.semte.ch/vocabularies/typed-literals/boolean>"` 
is present on the meeting, it aggressively collapses spaces to save on characters when publishing
the notulen.