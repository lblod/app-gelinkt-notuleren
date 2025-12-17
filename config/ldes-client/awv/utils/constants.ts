import { sparqlEscapeUri } from "mu";
import { environment } from "../../../environment";
export const LDES_GRAPH = "<http://mu.semte.ch/graphs/awv/ldes>";
export const PUBLIC_GRAPH = "<http://mu.semte.ch/graphs/public>";
export const BATCH_GRAPH = sparqlEscapeUri(environment.BATCH_GRAPH);
export const VERSION_OF = sparqlEscapeUri(environment.getVersionPredicate());

export const sudoOptions = environment.BYPASS_MU_AUTH
  ? {
      sparqlEndpoint: environment.DIRECT_DATABASE_CONNECTION,
    }
  : {};
