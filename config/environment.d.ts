// this file exists to declare the type for config/ldes-client service context
// we might find a better solution for this later
interface StreamConfig {
  name: string;
}
interface Environment {
  getVersionPredicate(): string;
  getTimePredicate(): string;
  getCurrentStreamConfig(): StreamConfig;
  BATCH_GRAPH: string;
  BYPASS_MU_AUTH: boolean;
  DIRECT_DATABASE_CONNECTION: string;
}
export const environment: Environment;
