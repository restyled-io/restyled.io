const request = require("request");
const zlib = require("zlib");

const prepareLogs = (event) => {
  const eventData = JSON.parse(
    zlib.unzipSync(Buffer.from(event.awslogs.data, "base64"))
  );

  const lines = eventData.logEvents.map((event) => {
    const parsed = parseLevel(event.message);

    return {
      timestamp: event.timestamp,
      env: "production",
      app: eventData.logGroup,
      file: eventData.logStream,
      level: parsed ? parsed.level : null,
      line: parsed ? parsed.message : event.message,
    };
  });

  return { lines: lines };
};

const parseLevel = (message) => {
  const regex = /^(error|warn|info|debug) /i;
  const result = message.match(regex);

  if (result !== null && result !== undefined) {
    return {
      level: result[1].toUpperCase().trim(),
      message: message.slice(result[0].length),
    };
  }

  return null;
};

const request_ = async (options) => {
  return new Promise((resolve, reject) => {
    request(options, (error, response, body) => {
      if (error) {
        reject(error);
      } else {
        resolve({ response, body });
      }
    });
  });
};

exports.handler = async (event) => {
  const lines = prepareLogs(event);
  const { response, body } = await request_({
    url: "https://logs.logdna.com/logs/ingest",
    qs: { hostname: "cloudwatch" },
    method: "POST",
    body: JSON.stringify(lines),
    auth: { username: process.env.LOGDNA_KEY },
    headers: {
      "content-type": "application/json; charset=UTF-8",
    },
  });

  return JSON.parse(body);
};
