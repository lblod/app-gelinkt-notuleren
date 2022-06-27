import generateReport from "./reportGenerator.js";
import { MONTHS } from "./utils/constants.js";
import { getLastMonth, getMonthDates } from "./utils/date.js";

export default {
	cronPattern: "0 0 12 1 * *",
	name: "monthlyReport",
	execute: async () => {
		const { month, year } = getLastMonth();
		const { start, end } = getMonthDates(month, year);
		console.log(start, end);
		const metadata = {
			title: `Statistieken ${MONTHS[month]} ${year}`,
			description:
				"Maandelijkse statistieken over het aantal aangemaakte, ondertekende en gepubliceerde documenten",
			filePrefix: `statistieken-${month + 1}-${year}`,
		};
		await generateReport(start, end, metadata);
	},
};
