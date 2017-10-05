# restyled.io

Homepage for Restyled, https://restyled.io.

## Development & Testing

Development requires the following pre-requisites:

1. [Stack](https://docs.haskellstack.org/en/stable/README/)
1. A PostgreSQL instance available on `localhost:5432`, with a `postgres` user
   with the password `"password"`

   For example:

   ```console
   docker run \
     --detach \
     --name postgres \
     --publish 5432:5432 \
     --env POSTGRES_PASSWORD=password
     postgres
   ```

1. A Redis instance available on `localhost:6379`

   For example:

   ```console
   docker run \
     --detach \
     --name redis \
     --publish 6379:6379 \
     redis
   ```

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

---

[CHANGELOG](./CHANGELOG.md) | [LICENSE](./LICENSE)
