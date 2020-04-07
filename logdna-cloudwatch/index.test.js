require("dotenv").config();

const zlib = require("zlib");
const index = require("./index");

const makeEvent = (payload) => {
  const zipped = zlib.gzipSync(JSON.stringify(payload));
  const base64 = Buffer.from(zipped).toString("base64");

  return {
    awslogs: {
      data: base64,
    },
  };
};

test("integration", async () => {
  const timestamp = new Date().getTime();
  const event = makeEvent({
    messageType: "DATA_MESSAGE",
    owner: "123456789012",
    logGroup: "testGroup",
    logStream: "testStream",
    subscriptionFilters: ["LambdaStream_cloudwatchlogs-node"],
    logEvents: [
      {
        id: "34622316099697884706540976068822859012661220141643892546",
        timestamp: timestamp,
        message: "INFO This is Log Line created from a test suite",
      },
    ],
  });

  const { status } = await index.handler(event);

  expect(status).toBe("ok");
});
