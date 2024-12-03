import generateReport from "./reportGenerator";
import { MONTHS } from "./utils/constants";
import { getLastMonth, getMonthDates } from "./utils/date";

export default {
  name: "historicalReport",
  execute: async () => {
    const start = new Date(1970, 0, 1);
    const { month, year } = getLastMonth();
    const end = getMonthDates(month, year).end;
    const metadata = {
      title: `Historische statistieken t.e.m. ${MONTHS[month]} ${year}`,
      description:
        "Historische statistieken over het aantal aangemaakte, ondertekende en gepubliceerde documenten",
      filePrefix: `historische-statistieken`,
    };
    await generateReport(start, end, metadata);
  },
};
