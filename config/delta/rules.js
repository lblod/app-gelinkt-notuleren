export default [
  {
    match: {
      subject: { }
    },
    callback: {
      url: "http://resource/.mu/delta",
      method: "POST"
    },
    options: {
      resourceFormat: "v0.0.1",
      gracePeriod: 250,
      ignoreFromSelf: true
    }
  },
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
  },
  {
    match: {
      // form of element is {subject,predicate,object}
      predicate: { type: "uri", value: "http://mu.semte.ch/vocabularies/ext/besluit-publicatie-publish-service/status" },
      object: { type: "uri", value: "http://mu.semte.ch/vocabularies/ext/besluit-publicatie-publish-service/status/success" }
    },
    callback: {
      url: "http://publicatie-melding/submit-publication", method: "POST"
    },
    options: {
      resourceFormat: "v0.0.1",
      gracePeriod: 1000,
      ignoreFromSelf: true
    }
  }
]
