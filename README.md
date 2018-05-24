# restyled.io

Website and backend for Restyled, https://restyled.io.

## Development & Testing

1. Start persistence services

   ```console
   docker-compose up -d
   ```

1. Create and seed the database, install dependencies:

   ```console
   make db.create db.seed setup
   ```

1. Build, test, and lint the application:

   ```console
   make
   ```

   From here, you can use any `stack`-based development work-flow.

1. Run (just) the website:

   ```console
   stack install yesod-bin
   stack exec yesod devel
   ```

   Visit `http://localhost:3000`.

## End-to-end Processing

To process real `restyled-io/demo` Pull Requests:

1. Ensure you have the latest restyler Docker image

   ```console
   (cd ../restyler && make image.build)
   ```

   Individual Restylers will be pulled as needed.

1. Run the website and backend

   ```console
   stack exec yesod devel
   ```

   ```console
   stack exec restyled.io-backend
   ```

1. Run ngrok

   ```console
   ngrok http -subdomain restyled 3000
   ```

   Visit `https://restyled.ngrok.io`.

1. Open a PR on `restyled-io/demo`, or re-deliver an existing Webhook.

This process assumes the following:

1. You have access to the development GitHub App and have configured your `.env`
1. You have authenticated `ngrok` to use the `restyled` subdomain

## Deployment & Self-Hosting

See [restyled-ops](https://github.com/restyled-io/ops).

---

[CHANGELOG](./CHANGELOG.md) | [LICENSE](./LICENSE)
