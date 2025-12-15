const config = {
  endpoints: [
    // {
    //   name: "MDB",
    //   LDES_BASE: process.env.MDB_BASE,
    //   FIRST_PAGE: process.env.MDB_FIRST_PAGE,
    //   TARGET_GRAPH: "http://mu.semte.ch/graphs/mandaten-staging",
    //   STATUS_GRAPH: "http://mu.semte.ch/graphs/status",
    //   EXTRA_HEADERS: process.env.MDB_HEADERS,
    //   VERSION_PREDICATE: "http://purl.org/dc/terms/isVersionOf",
    //   TIME_PREDICATE: "http://www.w3.org/ns/prov#generatedAtTime",
    // },
    {
      name: "AWV",
      LDES_BASE: process.env.AWV_BASE,
      FIRST_PAGE: process.env.AWV_FIRST_PAGE,
      STATUS_GRAPH: "http://mu.semte.ch/graphs/awv/ldes/status",
      VERSION_PREDICATE: "http://purl.org/dc/terms/isVersionOf",
      TIME_PREDICATE: "http://purl.org/dc/terms/issued",
      NEXT_PAGE_RELATIONSHIP_RDF_TYPE: "https://w3id.org/tree#Relation",
      SKOLEMIZE_BLANK_NODES: true,
      USE_JWT_AUTH: true,
      JWT_CLIENT_ID: process.env.AWV_JWT_CLIENT_ID,
      JWT_KEY: process.env.AWV_JWT_KEY,
      JWT_KEY_ALGORITHM: "RS256",
      JWT_TOKEN_URL: "https://authenticatie.vlaanderen.be/op/v1/token",
      JWT_TOKEN_REQUEST_AUDIENCE: "https://authenticatie.vlaanderen.be/op",
      JWT_TOKEN_REQUEST_EXPIRY: "10minutes",
      JWT_TOKEN_SCOPE: "awv_toep_services",
      JWT_CLIENT_ASSERTION_TYPE:
        "urn:ietf:params:oauth:client-assertion-type:jwt-bearer",
    },
  ],
};

export default config;
