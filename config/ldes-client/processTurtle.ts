export default function processTurtle(ttl: string) {
  return ttl.replace(/<[^>]+>/g, (substr) => substr.replace(/\\u0020/g, '+'));
}
