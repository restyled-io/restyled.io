# restyled.io

Homepage for Restyled, https://restyled.io.

## Development & Testing

Development requires the following pre-requisites:

1. [Stack](https://docs.haskellstack.org/en/stable/README/)
1. A PostgreSQL instance available on `localhost:5432`, with a `postgres` user
   with the password `"password"`
1. A Redis instance available on `localhost:6379`

*NOTE*: A `docker-compose.yml` exists for help with the latter 2.

Reset/create the database, install dependencies, build, lint, and test the app:

```console
make
```

From here, follow general Stack/Yesod development practices.


```console
stack build --pedantic --test --file-watch
```

```console
stack exec yesod devel
```

```console
stack repl --ghci-options="-DDEVELOPMENT -O0 -fobject-code"
```

## Deployment & Self-Hosting

See [restyled-ops](https://github.com/restyled-io/ops).

---

[CHANGELOG](./CHANGELOG.md) | [LICENSE](./LICENSE)
