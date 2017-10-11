# Restyled - Ops

Commands for the deployment and operation of Restyled infrastructure.

*This uses AWS Cloud Formation; familiarity is assumed.*

## Usage

```console
stack build
stack exec ops -- COMMAND [OPTION, ...]
```

## Commands

### `template`

Output the Cloud Formation template to `stdout`.

Accepts a named environment (e.g. `staging`), or the argument `custom` with a
set of options for configuring the template:

```
--app MyCompany
--name Restyled
--domain my-company.com
--subdomain restyled
--certificate arn://...
--ami ami-...
--cluster-size 5
--instance-type t2.medium
--instance-role ecsInstanceRole
--service-role ecsServiceRole
--log-level INFO
--image-tag latest
--app-count 3
--backend-count 5
```

### `create-stack`

Create a Cloud Formation Stack from our template.

Accepts the same environment name (or options) as `template`, the stack name
and, and all non-defaulted parameters:

```
--stack-name RestylerStaging
--github-app-id 5851
--github-app-key ~/downloads/restyled-io-staging.2017-09-27.private-key.pem
--database-url postgres://...
--redis-url redis://...
```

### `update-stack-template`

Update an existing Stack's template.

Accepts the same arguments as `template` and a `--stack-name`. **NOTE**: any
options left out will revert to defaults for the environment; they will not
preserve existing values, as one might expect.

### `update-stack-parameters`

Update an existing Stack's parameter values.

Accepts a mix of template-defaulted parameters (like `template`) and
non-defaulted parameters (like `create-stack`). Unlike `update-stack-template`,
omitted options will preserve existing values.

```
--stack-name RestylerStaging
--image-tag latest
--app-count 3
--backend-count 5
--github-app-id 5851
--github-app-key ~/downloads/restyled-io-staging.2017-09-27.private-key.pem
--database-url postgres://..
--redis-url redis://..
```

## Examples

### Create a new beta environment to test out a branch

**NOTE**: this relies on `custom` starting with defaults appropriate for my own
AWS infrastructure, so we only have to override the values that matter.

```console
stack exec ops -- create-stack custom
  --name Beta
  --subdomain beta
  --image-tag my-feature
  --github-app-id 1234 \
  --github-app-key ~/downloads/restyled-io-beta.2017-09-27.private-key.pem \
  --database-url postgres://<snip> \
  --redis-url redis://<snip> \
```

### Create a fully customized stack

This is our "behind your own firewall" offering.

```console
stack exec ops -- template custom
  --app MyCompany \
  --name Restyled \
  --domain my-company.com \
  --subdomain restyled \
  --certificate arn://... \
  --ami ami-... \
  --cluster-size 5 \
  --instance-type t2.medium \
  --instance-role ecsInstanceRole \
  --service-role ecsServiceRole \
  --log-level INFO \
  --image-tag latest \
  --app-count 3 \
  --backend-count 5 \
```

### Deploy a new version to an existing stack

```console
stack exec ops -- update-stack-parameters \
  --stack-name RestyledProd --image-tag b1234
```

### Scale up an existing stack

```console
stack exec ops -- update-stack-parameters \
  --stack-name RestyledProd --app-count 5
```
