import { generateReportFromData } from "../helpers.js";
import { querySudo as query } from "@lblod/mu-auth-sudo";
import generateReport from "./reportGenerator.js";

const months = [
	"Januari",
	"Februari",
	"Maart",
	"April",
	"Mei",
	"Juni",
	"Juli",
	"Augustus",
	"September",
	"Oktober",
	"November",
  "December"
];
export default {
	cronPattern: "0 0 12 1 * *",
	name: "monthlyReport",
	execute: async () => {
		const date = new Date();
    date.setDate(date.getDate() - 1)
		const month = date.getMonth() + 1;
		const year = date.getFullYear();
    await generateReport(month, year);
	},
};

function getValue(entry, property, defaultValue = 0) {
	return entry[property] ? entry[property].value : defaultValue;
}
