import { environment } from "../environment";
import { processPage as processPageMDB } from "./mdb/processPage";
import { processPage as processPageAWV } from "./awv/processPage";

export async function processPage() {
  switch (environment.getCurrentStreamConfig().name) {
    case "MDB":
      return processPageMDB();
    case "AWV":
      return processPageAWV();
  }
}
