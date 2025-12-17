declare module "mu" {
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

  /**
   * The parameter ObjOrIsAsk should be the type of an object which the returned data will be passed
   * in to. This allows TS to give us a return value that has the same keys mapped to Binding
   * objects. If this is an ASK query, pass `true` instead as the return of this query will be
   * different.
   */
  export interface UserOptions {
    sudo?: boolean;
    scope?: string;
    endpoint?: string;
  }
  export const query: <ObjOrIsAsk extends ObjectToBind | true = ObjectToBind>(
    query: string,
    opts?: UserOptions,
  ) => Promise<SparqlResponse<ObjOrIsAsk>>;
  export const update: (query: string) => Promise<void>;
  export const uuid: () => string;
  export const sparqlEscape: (value: unknown, type: string) => string;
  export const sparqlEscapeString: (value: string) => string;
  export const sparqlEscapeUri: (value: string) => string;
  export const sparqlEscapeInt: (value: number) => string;
  export const sparqlEscapeDecimal: (value: number) => string;
  export const sparqlEscapeFloat: (value: number) => string;
  export const sparqlEscapeDateTime: (value: Date) => string;
  export const sparqlEscapeBool: (value: boolean) => string;
  export const sparqlEscapeDate: (value: Date) => string;
  // this is a tagged template string function
  export const sparql: (
    strings: TemplateStringsArray,
    ...values: unknown[]
  ) => string;
  export const SPARQL: (
    strings: TemplateStringsArray,
    ...values: unknown[]
  ) => string;

  const mu: {
    query: typeof query;
    update: typeof update;
    uuid: typeof uuid;
    sparqlEscape: typeof sparqlEscape;
    sparqlEscapeString: typeof sparqlEscapeString;
    sparqlEscapeUri: typeof sparqlEscapeUri;
    sparqlEscapeInt: typeof sparqlEscapeInt;
    sparqlEscapeDecimal: typeof sparqlEscapeDecimal;
    sparqlEscapeFloat: typeof sparqlEscapeFloat;
    sparqlEscapeDateTime: typeof sparqlEscapeDateTime;
    sparqlEscapeBool: typeof sparqlEscapeBool;
    sparqlEscapeDate: typeof sparqlEscapeDate;
    sparql: typeof sparql;
    SPARQL: typeof SPARQL;
  };
  export default mu;
}
