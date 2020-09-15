-- * extension

CREATE EXTENSION CITEXT;
CREATE EXTENSION PGCRYPTO;
CREATE EXTENSION "uuid-ossp";


-- * account


CREATE TABLE account(
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    github_id INTEGER NOT NULL UNIQUE,
    email CITEXT UNIQUE
);


-- * request node


CREATE TYPE request_node_type AS ENUM ('RequestFolder', 'RequestFile');
CREATE TYPE http_method_type AS ENUM ('Get', 'Post', 'Put', 'Delete', 'Patch', 'Head', 'Options');
CREATE TYPE header_type AS (
  header_key TEXT,
  header_value TEXT
);

CREATE TABLE request_node(
  id UUID PRIMARY KEY,
  request_node_parent_id UUID REFERENCES request_node(id) ON DELETE CASCADE,
  tag request_node_type NOT NULL,
  name TEXT NOT NULL,
  http_url TEXT,
  http_method http_method_type,
  http_headers header_type[],
  http_body TEXT,
  CHECK (
    (tag = 'RequestFolder' AND http_url IS NULL AND http_method IS NULL AND http_headers IS NULL AND http_body IS NULL) OR
    (tag = 'RequestFile' AND http_url IS NOT NULL AND http_method IS NOT NULL AND http_headers IS NOT NULL AND http_body IS NOT NULL)
  )
);


-- * request collection


CREATE TABLE request_collection(
  id SERIAL PRIMARY KEY,
  account_id UUID REFERENCES account(id) ON DELETE CASCADE
);

CREATE TABLE request_collection_to_request_node(
  request_collection_id INTEGER REFERENCES request_collection(id) ON DELETE CASCADE,
  request_node_id UUID REFERENCES request_node(id) ON DELETE CASCADE,
  PRIMARY KEY (request_collection_id, request_node_id)
);


-- * pg node


CREATE TYPE pg_node_type AS ENUM ('PgFolder', 'PgFile');

CREATE TABLE pg_node(
  id UUID PRIMARY KEY,
  pg_node_parent_id UUID REFERENCES pg_node(id) ON DELETE CASCADE,
  tag pg_node_type NOT NULL,
  name TEXT NOT NULL,
  sql TEXT,
  pg_host TEXT,
  pg_password TEXT,
  pg_port TEXT,
  pg_user TEXT,
  pg_dbname TEXT,
  CHECK (
    (tag = 'PgFolder' AND sql IS NULL) OR
    ( tag = 'PgFile' AND
      sql IS NOT NULL AND
      pg_host IS NOT NULL AND
      pg_password IS NOT NULL AND
      pg_port IS NOT NULL AND
      pg_user IS NOT NULL AND
      pg_dbname IS NOT NULL
    )
  )
);


-- * pg collection


CREATE TABLE pg_collection(
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  account_id UUID REFERENCES account(id) ON DELETE CASCADE
);

CREATE TABLE pg_collection_to_pg_node(
  pg_collection_id UUID REFERENCES pg_collection(id) ON DELETE CASCADE,
  pg_actor_id UUID REFERENCES pg_node(id) ON DELETE CASCADE,
  PRIMARY KEY (pg_collection_id, pg_actor_id)
);


-- * scene node


CREATE TYPE scene_type AS ENUM ('HttpActor', 'PgActor');

CREATE TABLE scene_node(
  id UUID PRIMARY KEY,
  scene_node_parent_id UUID REFERENCES scene_node(id),
  actor_type scene_type NOT NULL,
  http_actor_id UUID REFERENCES request_node(id),
  pg_actor_id UUID REFERENCES pg_node(id),
  prescript TEXT NOT NULL,
  postscript TEXT NOT NULL,
  CHECK (
    (actor_type = 'HttpActor' AND http_actor_id IS NOT NULL AND pg_actor_id IS NULL) OR
    (actor_type = 'PgActor' AND pg_actor_id IS NOT NULL AND http_actor_id IS NULL)
  )
);


-- * environment


CREATE TABLE environment(
    id UUID PRIMARY KEY,
    name TEXT NOT NULL
);

CREATE TABLE account_environment(
    account_id UUID REFERENCES account(id) ON DELETE CASCADE,
    environment_id UUID REFERENCES environment(id) ON DELETE CASCADE,
    PRIMARY KEY (account_id, environment_id)
);

CREATE TABLE key_value(
    id UUID PRIMARY KEY,
    environment_id UUID REFERENCES environment(id) ON DELETE CASCADE,
    key TEXT NOT NULL,
    value TEXT NOT NULL,
    hidden BOOLEAN NOT NULL
);


-- * scenario node


CREATE TYPE scenario_node_type AS ENUM ('ScenarioFolder', 'ScenarioFile');

CREATE TABLE scenario_node(
  id UUID PRIMARY KEY,
  tag scenario_node_type NOT NULL,
  environment_id UUID REFERENCES environment(id),
  name TEXT NOT NULL,
  scenario_node_parent_id UUID REFERENCES scenario_node(id) ON DELETE CASCADE,
  scene_node_id UUID REFERENCES scene_node(id) ON DELETE CASCADE
  CHECK (
    (tag = 'ScenarioFile') OR
    (tag = 'ScenarioFolder' AND environment_id IS NULL AND scene_node_id IS NULL)
  )
);


-- * scenario collection


CREATE TABLE scenario_collection(
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  account_id UUID REFERENCES account(id) ON DELETE CASCADE
);

CREATE TABLE scenario_collection_to_scenario_node(
  scenario_collection_id UUID REFERENCES scenario_collection(id) ON DELETE CASCADE,
  scenario_node_id UUID REFERENCES scenario_node(id) ON DELETE CASCADE,
  PRIMARY KEY (scenario_collection_id, scenario_node_id)
);


-- * request node as json


CREATE OR REPLACE FUNCTION root_request_nodes_as_json(rc_id int) RETURNS jsonb[] AS $$
DECLARE result jsonb[];
BEGIN
  SELECT array_agg (
    CASE WHEN tag = 'RequestFolder' THEN
      jsonb_build_object(
        'id', id,
        'name', name,
        'tag', tag,
        'children', COALESCE(request_nodes_as_json(id), '{}'::jsonb[])
      )
    ELSE
      jsonb_build_object(
        'id', id,
        'name', name,
        'tag', tag,
        'http_body', http_body,
        'http_url', http_url,
        'http_method', http_method,
        'http_headers', http_headers
      )
    END
  ) INTO result
  FROM request_node rn
  INNER JOIN request_collection_to_request_node rcrn ON rcrn.request_node_id = rn.id
  WHERE rcrn.request_collection_id = rc_id;
  RETURN result;
END;
$$ LANGUAGE plpgsql;


CREATE OR REPLACE FUNCTION request_nodes_as_json(node_id uuid) RETURNS jsonb[] AS $$
DECLARE result jsonb[];
BEGIN
  SELECT array_agg (
    CASE WHEN tag = 'RequestFolder' THEN
      jsonb_build_object(
        'id', id,
        'name', name,
        'tag', tag,
        'children', COALESCE(request_nodes_as_json(id), '{}'::jsonb[])
      )
    ELSE
      jsonb_build_object(
        'id', id,
        'name', name,
        'tag', tag,
        'http_body', http_body,
        'http_url', http_url,
        'http_method', http_method,
        'http_headers', http_headers
      )
    END
  ) INTO result
  FROM request_node
  WHERE request_node_parent_id = node_id;
  RETURN result;
END;
$$ LANGUAGE plpgsql;


-- * pg node as json


CREATE OR REPLACE FUNCTION root_pg_nodes_as_json(rc_id uuid) RETURNS jsonb[] AS $$
DECLARE result jsonb[];
BEGIN
  SELECT array_agg (
    CASE WHEN tag = 'PgFolder' THEN
      jsonb_build_object(
        'id', id,
        'name', name,
        'tag', tag,
        'children', COALESCE(pg_nodes_as_json(id), '{}'::jsonb[])
      )
    ELSE
      jsonb_build_object(
        'id', id,
        'name', name,
        'tag', tag,
        'sql', sql,
        'pg_host', pg_host,
        'pg_password', pg_password,
        'pg_port', pg_port,
        'pg_user', pg_user,
        'pg_dbname', pg_dbname
      )
    END
  ) INTO result
  FROM pg_node rn
  INNER JOIN pg_collection_to_pg_node rcrn ON rcrn.pg_actor_id = rn.id
  WHERE rcrn.pg_collection_id = rc_id;
  RETURN result;
END;
$$ LANGUAGE plpgsql;


CREATE OR REPLACE FUNCTION pg_nodes_as_json(node_id uuid) RETURNS jsonb[] AS $$
DECLARE result jsonb[];
BEGIN
  SELECT array_agg (
    CASE WHEN tag = 'PgFolder' THEN
      jsonb_build_object(
        'id', id,
        'name', name,
        'tag', tag,
        'children', COALESCE(pg_nodes_as_json(id), '{}'::jsonb[])
      )
    ELSE
      jsonb_build_object(
        'id', id,
        'name', name,
        'tag', tag,
        'sql', sql,
        'pg_host', pg_host,
        'pg_password', pg_password,
        'pg_port', pg_port,
        'pg_user', pg_user,
        'pg_dbname', pg_dbname
      )
    END
  ) INTO result
  FROM pg_node
  WHERE pg_node_parent_id = node_id;
  RETURN result;
END;
$$ LANGUAGE plpgsql;


-- * scenario node as json


CREATE OR REPLACE FUNCTION root_scenario_nodes_as_json(rc_id uuid) RETURNS jsonb[] AS $$
DECLARE result jsonb[];
BEGIN
  SELECT array_agg (
    CASE WHEN tag = 'ScenarioFolder' THEN
      jsonb_build_object(
        'id', id,
        'name', name,
        'tag', tag,
        'children', COALESCE(scenario_nodes_as_json(id), '{}'::jsonb[])
      )
    ELSE
      jsonb_build_object(
        'id', id,
        'name', name,
        'tag', tag,
        'scene_nodes', root_scene_node_as_json(scene_node_id),
        'environment_id', environment_id
      )
    END
  ) INTO result
  FROM scenario_node rn
  INNER JOIN scenario_collection_to_scenario_node rcrn ON rcrn.scenario_node_id = rn.id
  WHERE rcrn.scenario_collection_id = rc_id;
  RETURN result;
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION scenario_nodes_as_json(node_id uuid) RETURNS jsonb[] AS $$
DECLARE result jsonb[];
BEGIN
  SELECT array_agg (
    CASE WHEN tag = 'ScenarioFolder' THEN
      jsonb_build_object(
        'id', id,
        'name', name,
        'tag', tag,
        'children', COALESCE(scenario_nodes_as_json(id), '{}'::jsonb[])
      )
    ELSE
      jsonb_build_object(
        'id', id,
        'name', name,
        'tag', tag,
        'scene_nodes', root_scene_node_as_json(scene_node_id),
        'environment_id', environment_id
      )
    END
  ) INTO result
  FROM scenario_node
  WHERE scenario_node_parent_id = node_id;
  RETURN result;
END;
$$ LANGUAGE plpgsql;


-- * scene node as json


CREATE OR REPLACE FUNCTION scene_node_as_json(node_id uuid) RETURNS jsonb[] AS $$
DECLARE result jsonb[];
BEGIN
  SELECT
    array_agg(
      jsonb_build_object(
        'id', id,
        'scene_node_parent_id', scene_node_parent_id,
        'actor_type', actor_type,
        'http_actor_id', http_actor_id,
        'pg_actor_id', pg_actor_id,
        'prescript', prescript,
        'postscript', postscript
      )
    ) || scene_node_as_json(id)
  INTO result
  FROM scene_node
  WHERE scene_node_parent_id = node_id
  GROUP BY id;
  RETURN result;
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION root_scene_node_as_json(node_id uuid) RETURNS jsonb[] AS $$
DECLARE result jsonb[];
BEGIN
  SELECT
    array_agg(
      jsonb_build_object(
        'id', id,
        'scene_node_parent_id', scene_node_parent_id,
        'actor_type', actor_type,
        'http_actor_id', http_actor_id,
        'pg_actor_id', pg_actor_id,
        'prescript', prescript,
        'postscript', postscript
      )
    ) || scene_node_as_json(node_id)
  INTO result
  FROM scene_node
  WHERE id = node_id;
  RETURN result || ARRAY[]::jsonb[]; -- always returns empty array if result is NULL;
END;
$$ LANGUAGE plpgsql;


-- * test


CREATE TABLE user_test(
    id SERIAL PRIMARY KEY,
    firstname TEXT NOT NULL,
    lastname TEXT NOT NULL
);
