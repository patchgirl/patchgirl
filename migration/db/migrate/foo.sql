
WITH RECURSIVE request_node_with_its_parent AS (
  SELECT id, '{}'::int[] AS parents, 0 AS level
  FROM request_node n
  INNER JOIN request_collection_to_request_node cn ON(cn.request_node_id = n.id)
  WHERE request_node_parent_id IS NULL
  AND cn.request_collection_id = 1
  UNION ALL
  SELECT c.id, parents || c.request_node_parent_id, level + 1
  FROM request_node_with_its_parent p
  JOIN request_node c
  ON c.request_node_parent_id = p.id

), parent_ids AS (
  SELECT DISTINCT UNNEST(parents) AS id
  FROM request_node_with_its_parent

), leave_ids AS (
  SELECT r.id, level
  FROM request_node_with_its_parent r
  LEFT JOIN parent_ids p ON r.id = p.id
  WHERE p.id IS NULL

), request_node_from_leaves AS (
  (
    -- root leaves
    SELECT c.request_node_parent_id,
    request_node_as_js(c) AS js
    FROM leave_ids l
    JOIN request_node c USING(id)
    WHERE level = 0

    UNION ALL

    -- leaves
    SELECT c.request_node_parent_id,
    json_agg(request_node_as_js(c))::jsonb AS js
    FROM leave_ids l
    JOIN request_node c USING(id)
    WHERE level > 0
    GROUP BY request_node_parent_id
  )
  UNION ALL
  SELECT
    c.request_node_parent_id,
    request_node_as_js(c) || jsonb_build_object('children', js) AS js
  FROM request_node_from_leaves tree
  JOIN request_node c ON c.id = tree.request_node_parent_id
)

SELECT jsonb_pretty(jsonb_agg(js))
FROM request_node_from_leaves
WHERE request_node_parent_id IS NULL;
