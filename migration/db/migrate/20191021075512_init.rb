class Init < ActiveRecord::Migration[5.2]
  def up
    if Rails.env.test? then
      execute %{
        ALTER ROLE postgres SET client_min_messages TO WARNING; -- to avoid log when truncating in test
      }
    end

    execute %{

      CREATE EXTENSION CITEXT;
      CREATE EXTENSION PGCRYPTO;

      -- account


      CREATE TABLE account(
          id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
          github_id INTEGER NOT NULL UNIQUE,
          email CITEXT UNIQUE,
          signup_token TEXT NOT NULL DEFAULT MD5(random()::text),
          password TEXT
      );


      -- request node


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


      CREATE TABLE request_collection(
        id SERIAL PRIMARY KEY,
        account_id UUID REFERENCES account(id) ON DELETE CASCADE
      );

      CREATE TABLE request_collection_to_request_node(
        request_collection_id INTEGER REFERENCES request_collection(id) ON DELETE CASCADE,
        request_node_id UUID REFERENCES request_node(id) ON DELETE CASCADE,
        PRIMARY KEY (request_collection_id, request_node_id)
      );


      -- environment


      CREATE TABLE environment(
          id SERIAL PRIMARY KEY,
          name TEXT NOT NULL
      );

      CREATE TABLE account_environment(
          account_id UUID REFERENCES account(id) ON DELETE CASCADE,
          environment_id INTEGER REFERENCES environment(id) ON DELETE CASCADE,
          PRIMARY KEY (account_id, environment_id)
      );

      CREATE TABLE key_value(
          id SERIAL PRIMARY KEY,
          environment_id INTEGER REFERENCES environment(id) ON DELETE CASCADE,
          key TEXT NOT NULL,
          value TEXT NOT NULL
      );

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

    }
  end

  def down
    execute %{
      DROP TABLE request_node;
      DROP TABLE request_collection_to_request_node;

      DROP FUNCTION request_nodes_as_json(integer);

      REMOVE request_node_type;
      REMOVE http_method_type;
    }
  end
end
