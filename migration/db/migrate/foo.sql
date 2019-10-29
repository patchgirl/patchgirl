WITH RECURSIVE request_node_with_its_parent AS ( --
  SELECT id, '{}'::int[] AS parents, 0 AS level
  FROM request_node
  WHERE request_node_parent_id IS NULL
  AND request_collection_id = 1
  UNION ALL
  SELECT c.id, parents || c.request_node_parent_id, level + 1
  FROM request_node_with_its_parent p
  JOIN request_node c
  ON c.request_node_parent_id = p.id
), request_node_id AS (
  SELECT id
  FROM request_node_with_its_parent
), parent_ids AS (
  SELECT DISTINCT UNNEST(parents) AS id
  FROM request_node_with_its_parent
), leave_ids AS (
  SELECT r.id
  FROM request_node_id r
  LEFT JOIN parent_ids p ON r.id = p.id
  WHERE p.id IS NULL
), request_node_from_leaves AS (
  SELECT c.request_node_parent_id,
  json_agg(
    jsonb_build_object('name', c.name, 'tag', c.tag, 'id', c.id, 'http_url', c.http_url, 'http_method', c.http_method, 'http_body', c.http_body, 'http_headers', c.http_headers)
  )::jsonb AS js
  FROM leave_ids l
  JOIN request_node c USING(id)
  GROUP BY c.request_node_parent_id
  UNION ALL
  SELECT
    c.request_node_parent_id,
    jsonb_build_object('name', c.name, 'tag', c.tag, 'id', c.id, 'http_url', c.http_url, 'http_method', c.http_method, 'http_headers', c.http_headers, 'http_body', c.http_body)
    || jsonb_build_object('children', js) AS js
  FROM request_node_from_leaves tree
  JOIN request_node c ON c.id = tree.request_node_parent_id
)
SELECT jsonb_pretty(jsonb_agg(js))
FROM request_node_from_leaves
WHERE request_node_parent_id IS NULL;
