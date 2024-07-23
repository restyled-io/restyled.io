# restyled.io

Website and backend for Restyled, https://restyled.io.

## Development & Testing

1. Start persistence services:

   ```console
   docker-compose up -d
   ```

1. Initialize dependencies, the database, build, lint and test:

   ```console
   just bootstrap
   ```

From here, you can use any `stack`-based development and testing work-flow.

## Fully-functional Website

1. Setup local secrets in `.env`

   ```console
   cp .env.example .env.development
   $EDITOR .env.development
   ```

1. Run the site and backend locally:

   ```console
   just watch
   ```

1. Run ngrok:

   _This is required for OAuth login and receiving webhooks from our development
   GitHub Application. You will need to get the ngrok authentication token out
   of band somehow._

   ```console
   ngrok authtoken <YOUR_AUTHTOKEN>
   ngrok http -subdomain restyled 3000
   ```

   Visit `https://restyled.ngrok.io`.

## End-to-end Processing

To process real `restyled-io/demo` Pull Requests:

1. Ensure a Restyler image is available to use:

   To use a locally-built image:

   ```console
   (cd ../restyler && docker build --tag restyled/restyler .)
   ```

   To use a deployed image, set `RESTYLER_IMAGE` in `.env.development`.

   _At this time, individual Restylers will always be pulled from deployed
   sources._

1. Trigger a restyling:

   - Open a PR on `restyled-io/demo`,
   - Re-deliver an existing Webhook, or
   - Use `curl`, probably with files in `fixtures/`:

     ```console
     curl \
       --header "Accept: application/json" \
       --header "Content-Type: application/json" \
       --data @"path/to.json" "https://restyled.ngrok.io/webhooks"
     ```

## LICENSE

Restyled is source-available, [Commons Claused][cc] licensed. For a detailed
description of another project's rationale for this licensing model, one with
which I agree, see [here][level].

[cc]: https://commonsclause.com/
[level]: https://web.archive.org/web/20181120030157/https://leveljournal.com/source-available-licensing

---

[CHANGELOG](./CHANGELOG.md) | [LICENSE](./LICENSE) | [CONTRIBUTING][]

[contributing]:
  https://github.com/restyled-io/restyled.io/wiki/Contributing-to-Restyled
