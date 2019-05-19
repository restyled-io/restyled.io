BEGIN;

DELETE from repo;
INSERT INTO repo (
  owner, -- (Name Owner)
  name, -- (Name Repo)
  installation_id, -- (Id Installation)
  is_private, -- (Bool)
  debug_enabled -- (Bool)
) VALUES (
  'restyled-io',
  'demo',
  58920,
  FALSE,
  FALSE
), (
  'restyled-io',
  'restyler',
  58920,
  FALSE,
  TRUE
), (
  'restyled-io',
  'ops',
  58920,
  TRUE, -- private
  FALSE
);

DELETE FROM plan;
INSERT INTO plan (
  type, -- (PlanType)
  owner, -- (Name Owner)
  repo, -- (Name Repo)
  active_at, -- Maybe UTCTime
  expires_at, -- Maybe UTCTime
  message -- TEXT
) VALUES (
  'trial',
  'restyled-io',
  'restyled.io',
  NULL,
  NULL,
  'Example plan'
), (
  'trial',
  'restyled-io',
  'restyled.io',
  NOW() - ('600 seconds' :: interval),
  NULL,
  'Never expires'
), (
  'trial',
  'restyled-io',
  'ops',
  NULL,
  NOW() + ('1500 seconds' :: interval),
  'Expires soon'
), (
  'trial',
  'restyled-io',
  'ops',
  NULL,
  NOW() - ('300 seconds' :: interval),
  'Expired'
);

-- DELETE dependent job_log_line first
DELETE FROM job_log_line;
DELETE FROM job;

INSERT INTO job (
  installation_id, -- (Id Installation)
  owner, -- (Name Owner)
  repo, -- (Name Repo)
  pull_request, -- (Id PullRequest)
  created_at, -- UTCTime
  updated_at, -- UTCTime
  completed_at, -- UTCTime Maybe
  exit_code, -- Int Maybe
  stdout, -- Text Maybe
  stderr -- Text Maybe
) VALUES (
  -- Errored
  58920,
  'restyled-io',
  'demo',
  1,
  NOW() - ('600 seconds' :: interval),
  NOW() - ('37 seconds' :: interval),
  NOW() - ('37 seconds' :: interval),
  2,
  '',
  $$Process unsuccessful (ExitFailure 127)
Command: docker
Arguments: [--rm, --net=none, ...]
stdout:
stderr:
  docker: command not found
    1:some/stack
    75:trace/there

Please see https://google.com

Please see

  - https://google.com
  - https://google.com
  - https://google.com
$$
), (
  -- Successful
  58920,
  'restyled-io',
  'demo',
  1,
  NOW() - ('300 seconds' :: interval),
  NOW() - ('42 seconds' :: interval),
  NOW() - ('42 seconds' :: interval),
  0,
  $$checking out the thing...
[Error] restyle this
[Info] restyle that
[Debug] some debug stuff
[Warn] no style differences
[Debug] some debug stuff
[Info] No style differences
$$,
  'git: commit: nothing to commit'
), (
  -- In progress
  58920,
  'restyled-io',
  'demo',
  2,
  NOW() - ('10 seconds' :: interval),
  NOW() - ('10 seconds' :: interval),
  NULL,
  NULL,
  NULL,
  NULL
);

WITH last_job AS (
  SELECT id FROM job
    WHERE owner = 'restyled-io'
      AND repo = 'demo'
      AND pull_request = 2
  LIMIT 1
)
INSERT INTO job_log_line (
  job,
  created_at,
  stream,
  content
)
SELECT
  id as job,
  NOW() - ('10 seconds' :: interval) as created_at,
  'stdout' as stream,
  '[Info] restyling the things' as content
FROM last_job
UNION
SELECT
  id as job,
  NOW() - ('5 seconds' :: interval) as created_at,
  'stderr' as stream,
  '[Warn] unable to restyle' as content
FROM last_job
UNION
SELECT
  id as job,
  NOW() - ('4 seconds' :: interval) as created_at,
  'stderr' as stream,
  '[Error] restyling failed' as content
FROM last_job;

-- We don't seed an enabled example because for it to be functional would mean
-- having secrets in the seeds, and we'd rather not have a non-functional
-- machine in the seeded database.
DELETE FROM restyle_machine;
INSERT INTO restyle_machine (
  name,
  enabled,
  host,
  ca_cert,
  cert,
  key
) VALUES (
  'disabled-example',
  false,
  'tcp://123.123.123:123',
  $$-- CA --
$$,
  $$-- CERT --
$$,
  $$-- Key --
$$
);

DELETE FROM marketplace_account;
DELETE FROM marketplace_plan;
INSERT INTO marketplace_plan (
  github_id,
  name,
  description
) VALUES (
  0,
  'Friends & Family',
  'Manually managed discount plan'
);
INSERT INTO marketplace_account (
  github_id,
  github_login,
  marketplace_plan
)
SELECT
  50812 as github_id,
  'pbrisbin' as github_login,
  marketplace_plan.id
FROM marketplace_plan
WHERE marketplace_plan.github_id = 0;

COMMIT;
