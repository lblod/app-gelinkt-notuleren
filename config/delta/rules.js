export default [
  {
    match: {
      subject: {},
    },
    callback: {
      url: "http://resource/.mu/delta",
      method: "POST",
    },
    options: {
      resourceFormat: "v0.0.1",
      gracePeriod: 250,
      ignoreFromSelf: true,
    },
  },

  {
    match: {
      predicate: {
        type: "uri",
        value: "http://www.w3.org/1999/02/22-rdf-syntax-ns#type",
      },
      object: {
        type: "uri",
        value: "http://mu.semte.ch/vocabularies/ext/signing/PublishedResource",
      },
    },
    callback: {
      url: "http://published-resource-producer/delta",
      method: "POST",
    },
    options: {
      resourceFormat: "v0.0.1",
      gracePeriod: 1000,
      ignoreFromSelf: true,
    },
  },
  {
    match: {
      graph: {
        type: 'uri',
        value: 'http://mu.semte.ch/graphs/lmb-mandaten-staging',
      },
    },
    callback: {
      url: 'http://lmb-upsert-ldes-entity/delta',
      method: 'POST',
    },
    options: {
      resourceFormat: 'v0.0.1',
      gracePeriod: 1000,
      ignoreFromSelf: true,
      optOutMuScopeIds: [
        'http://redpencil.data.gift/id/concept/muScope/deltas/initialSync',
      ],
    },
  },
];
