# restyled.io

Homepage for Restyled, https://restyled.io.

## Development & Testing

Basic development requires the following pre-requisites:

1. [Stack](https://docs.haskellstack.org/en/stable/README/)
1. A PostgreSQL instance available on `localhost:5432`, with a `postgres` user
   with the password `"password"`
1. A Redis instance available on `localhost:6379`

*NOTE*: A `docker-compose.yml` exists for help with the latter 2.

Create and seed the database, install dependencies:

```console
make db.create db.seed setup
```

Build, test, and lint the application:

```console
make
```

Run the website:

```console
stack install yesod-bin
stack exec yesod devel
```

To run the backend:

```console
stack exec restyled.io-backend
```

**NOTE**: actually processing Jobs would require a valid `.env` file and a
restyler Docker image. To make use of such functionality would *also* require
some way to get GitHub events to your local instance, which is beyond the scope
of this README.

## Deployment & Self-Hosting

See [restyled-ops](https://github.com/restyled-io/ops).

---

[CHANGELOG](./CHANGELOG.md) | [LICENSE](./LICENSE)
