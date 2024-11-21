---
"app-gelinkt-notuleren": patch
---

Downgrade mu-cl-resources to 1.24.0:
Version 1.25.0 contains a bug where `belongsTo` relationships are not correctly cleared when setting such a relationship 2+ times after eachother.
