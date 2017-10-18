# Restyled - Ops

Commands for the deployment and operation of Restyled infrastructure.

*This uses AWS Cloud Formation; familiarity is assumed.*

## Usage

```console
stack build
stack exec ops -- COMMAND [OPTION, ...]
```

## Examples

### Deployment

```console
stack exec ops -- deploy --stack-name RestyledProd --image-tag b1234
```

### Scaling

For now, use the AWS console, or `aws cloudformation` CLI to update the
`AppsAppServiceCount` or `AppsBackendServiceCount` parameter(s).

### "Behind my own firewall" Installation

To create and operate Restyled in your own AWS account, use the template
directly:

```console
stack exec ops -- template > /path/to/template.json
```

Most parameters are self-explanatory, except for:

- **CertificateARN**:

  It's assumed you've already set up a hosted zone for the domain you intended
  to deploy to and created an Amazon-managed certificate. When creating your
  Stack include this `Domain` and the ARN to the certificate in the
  `CertificateARN` parameter.

- **GitHubAppKeyBase64**:

  Since Cloud Formation parameters can't handle newlines, we accept this
  parameter as base64-encoded. For example:

  ```console
  base64 /path/to/private-key.pem | tr -d '\n' | xclip -selection clipboard
  ```

*NOTE*: Currently, such an installation still talks to `github.com`, but it's on
the roadmap to support a custom GitHub host, e.g. for use with GH:E.
