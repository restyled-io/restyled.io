COPY (
  WITH recent_jobs AS (
    SELECT
      date_trunc('day', job.created_at) as day
      , job.owner || '/' || job.repo AS slug
      , CASE
        WHEN job.completed_at IS NULL AND count(job_log_line.*) = 2
        THEN 1
        ELSE NULL
        END as stuck
    FROM job
    LEFT OUTER JOIN job_log_line ON job_log_line.job = job.id
    WHERE job.created_at < date_trunc('day', current_date)
      AND job.created_at > current_date - interval '30' day
    GROUP BY (job.id)
  )

  SELECT
    to_char(day, 'YYYY-MM-DD') as day
    , count(stuck) AS stuck
    , count(*) AS total
    , ROUND(((count(stuck)::float / count(*)) * 100)::numeric, 2) as rate
  FROM recent_jobs
  --WHERE slug = '...'
  GROUP BY (day)
  ORDER BY day DESC, rate DESC
) TO STDOUT (FORMAT CSV)
