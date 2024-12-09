# restyled.io

Historical placeholder, to point users elsewhere.

## Development & Testing

```console
docker compose up --build
```

``console
$BROWSER http://localhost:3000
```

> [!NOTE]
> `nginx/html/` is volume-mounted so changes will be seen immediately.

## Deployment

Push to `main`, let the `CI` workflow do its thing. Requires `HEROKU_\*`
organization-level secrets.

## LICENSE

AGPLv3. See [COPYING](./COPYING).
