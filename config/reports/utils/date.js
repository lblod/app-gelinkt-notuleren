export function getLastMonth() {
	const date = new Date();
	console.log(date.toISOString())
	date.setDate(1);
	date.setMonth(date.getMonth() - 1);
	return { month: date.getMonth(), year: date.getFullYear() };
}

export function getMonthDates(month, year) {
	const start = new Date(year, month, 1);
	const end = new Date(year, month + 1, 0);
	return { start, end };
}
