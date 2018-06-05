BEGIN;

DELETE FROM signup;
INSERT INTO signup (
  email
) VALUES (
  'me@example.com'
), (
  'you@example.com'
);

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
  NOW(),
  NOW(),
  NULL,
  NULL,
  NULL,
  NULL
);

COMMIT;
