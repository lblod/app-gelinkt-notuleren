const config = {
  endpoints: [
    {
      name: "MDB",
      LDES_BASE: process.env.MDB_BASE,
      FIRST_PAGE: process.env.MDB_FIRST_PAGE,
      TARGET_GRAPH: "http://mu.semte.ch/graphs/mandaten-staging",
      STATUS_GRAPH: "http://mu.semte.ch/graphs/status",
      EXTRA_HEADERS: process.env.MDB_HEADERS,
      VERSION_PREDICATE: "http://purl.org/dc/terms/isVersionOf",
      TIME_PREDICATE: "http://www.w3.org/ns/prov#generatedAtTime",
    },
  ],
};

export default config;
