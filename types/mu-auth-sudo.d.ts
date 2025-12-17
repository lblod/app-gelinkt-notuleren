declare module "@lblod/mu-auth-sudo" {
  export type ObjectToBind = Record<string, unknown>;
  export type BindingObject<Obj extends ObjectToBind = ObjectToBind> = {
    [Prop in keyof Obj]: {
      type: string;
      value: string;
    };
  };

  export type SparqlResponse<
    ObjOrIsAsk extends ObjectToBind | true = ObjectToBind,
  > = ObjOrIsAsk extends ObjectToBind
    ? {
        head: {
          vars: string[];
          link: [];
        };
        results: {
          bindings: BindingObject<ObjOrIsAsk>[];
        };
      }
    : {
        head: unknown;
        boolean: boolean;
      };
  interface SudoOptions {
    mayRetry?: boolean;
    sparqlEndpoint?: string;
  }
  export function updateSudo(
    query: string,
    extraHeaders?: Record<string, string>,
    connectionOptions?: SudoOptions,
  ): Promise<void>;

  export const querySudo: <
    ObjOrIsAsk extends ObjectToBind | true = ObjectToBind,
  >(
    query: string,
    extraHeaders?: Record<string, string>,
    connectionOptions?: SudoOptions,
  ) => Promise<SparqlResponse<ObjOrIsAsk>>;
}
