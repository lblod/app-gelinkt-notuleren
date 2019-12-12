export default [
  {
    match: {
      // form of element is {subject,predicate,object}
      object: { type: "uri", value: "http://mu.semte.ch/vocabularies/ext/signing/PublishedResource" }
    },
    callback: {
      url: "http://besluit-publicatie/publish-tasks", method: "POST"
    },
    options: {
      resourceFormat: "v0.0.1",
      gracePeriod: 1000,
      ignoreFromSelf: true
    }
  }
]
