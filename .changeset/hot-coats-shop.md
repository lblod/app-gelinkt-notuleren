---
"app-gelinkt-notuleren": minor
---

Introduce single `ldes-client` service (based on `lblod/ldes-client`) to replace both the `awv` and `mdb` ldes client services.
This service is able to handle consuming two (or more) LDES feeds at the same time.
