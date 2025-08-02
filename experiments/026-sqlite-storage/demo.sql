-- demo.sql - Demo SQLite operations

-- Insert sample repositories
INSERT OR IGNORE INTO repositories (url, name, description, language) 
VALUES 
    ('https://github.com/aygp-dr/repomind', 'repomind', 'Intelligent repository analysis', 'Scheme'),
    ('https://github.com/test/project', 'test-project', 'Test project', 'Python');

-- Insert analyses
INSERT INTO analyses (repo_id, summary, quality_score)
SELECT id, 'Well-structured Scheme project with experiments', 0.92
FROM repositories WHERE name = 'repomind';

-- Insert metrics
INSERT INTO metrics (repo_id, metric_name, value)
SELECT id, 'response_time', 1.23
FROM repositories WHERE name = 'repomind';

INSERT INTO metrics (repo_id, metric_name, value)
SELECT id, 'cache_hit_rate', 0.43
FROM repositories WHERE name = 'repomind';

-- Query demonstrations
.echo on
.headers on
.mode column

-- All repositories
SELECT '=== All Repositories ===' as '';
SELECT id, name, language, description FROM repositories;

-- Analyses with repository info
SELECT '=== Repository Analyses ===' as '';
SELECT r.name, a.summary, a.quality_score, a.created_at
FROM analyses a
JOIN repositories r ON a.repo_id = r.id;

-- Metrics summary
SELECT '=== Metrics Summary ===' as '';
SELECT r.name, m.metric_name, m.value
FROM metrics m
JOIN repositories r ON m.repo_id = r.id
ORDER BY r.name, m.metric_name;