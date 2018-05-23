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
  TRUE
), (
  'restyled-io',
  'restyler',
  58920,
  FALSE,
  TRUE
), (
  'pbrisbin',
  'some-great-repo-but-a-long-name',
  58920,
  FALSE,
  TRUE
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
[INFO ] restyle this
[INFO ] restyle that
[WARN ] no style differences
[DEBUG] abort
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
