# Restyled - Ops

Commands for the deployment and operation of Restyled infrastructure.

*This uses AWS Cloud Formation; familiarity is assumed.*

## Usage

```console
stack build
stack exec ops -- template
stack exec ops -- create-stack KEY=VALUE [KEY=VALUE, ...]
stack exec ops -- update-stack [KEY=VALUE, ...]
```

See `create-stack --help` for all available parameters and their defaults.

## Examples

### Create a Production stack, specifying only required values

```console
stack exec ops -- create-stack --stack-name RestyledProd \
  GitHubAppId=1234 \
  GitHubAppKey=~/downloads/restyled-io.2017-09-27.private-key.pem \
  DBPassword=<snip>
```

### Create a Beta stack, to test out a branch

```console
stack exec ops -- create-stack --stack-name RestyledBeta \
  Environment=Beta \
  Subdomain=beta \
  ImageTag=my-feature \
  GitHubAppId=1234 \
  GitHubAppKey=~/downloads/restyled-io-beta.2017-09-27.private-key.pem \
  DBPassword=<snip>
```

### Deploy a new version to an existing stack

```console
stack exec ops -- update-stack --stack-name RestyledProd \
  ImageTag=b1234
```

### Scale up an existing stack

```console
stack exec ops -- update-stack --stack-name RestyledProd \
  AppsAppServiceCount=5 \
  AppsBackendServiceCount=5
```
