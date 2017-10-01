# Restyled.io Documentation

**Note**: these docs are aspirational, not all of what's document here is
implemented. Anything that is implemented is subject to change.

## Configuration

Configuration is performed by committing a `.restyled.yaml` into your
repository.

### Top-Level

| Key           | Type          | Description
| ---           | ---           | ---
| **enabled**   | `true|false`  | Should we run at all?
| **restylers** | `[Restyler]`  | What Restylers should we run?

### Restyler

A `Restyler` has the following properties:

| Key           | Type          | Description
| ---           | ---           | ---
| **command**   | `String`      | Command to run
| **arguments** | `[String]`    | Additional arguments to pass to `command`, before the files to restyle
| **include**   | `[Pattern]`   | Patterns used to filter which files are restyled

A `Restyler` can be specified in YAML by 3 different forms:

1. A simple `String`, the name of a known Restyler

    ```yaml
    ---
    restylers:
    - stylish-haskell
    ```

    In this case `stylish-haskell` will be used to restyle Haskell files.

1. As above, but used as a key into an `Object` of overrides:

    ```yaml
    ---
    restylers:
    - stylish-haskell:
        include:
          - "**/*.lhs"
    ```

    In this case, we've accepted other defaults, but changed things to operate
    on Literate Haskell files instead.

1. Fully configured:

    ```yaml
    ---
    restylers:
    - command: stylish-haskell
      arguments:
      - --inplace
      include:
      - "**/*.hs"
    ```

    In this case, all keys are required.

### List-Syntax

If you don't need to specify anything besides the `restylers`, your
configuration file can itself be only the list of `Restyler`s:

```yaml
---
- stylish-haskell
- prettier
```

```yaml
---
- stylish-haskell:
    arguments:
    - --inplace
    - --config
    - hs-styles.yaml
- prettier
```

## Disable

Disabling Restyled.io for a Repository can be done in two ways:

1. Disable it in [Repository Settings](#):

    *TODO: screenshot(s)*

1. Commit a configuration file to your repository, setting the `enabled` key:

    **.restyled.yaml**

    ```yaml
    ---
    enabled: false
    ```

Disabling a single Restyler can be done by setting `include` to the empty list:

```yaml
- stylish-haskell:
    include: []
```

Restyled.io can be fully removed for an entire Organization by un-installing the
GitHub App at *https://github.com/organizations/:org/settings/installations*.
