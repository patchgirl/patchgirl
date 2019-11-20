class Init < ActiveRecord::Migration[5.2]
  def up
    execute %{

      CREATE TYPE request_node_type AS ENUM ('RequestFolder', 'RequestFile');
      CREATE TYPE http_method_type AS ENUM ('Get', 'Post', 'Put', 'Delete', 'Patch', 'Head', 'Options');
      CREATE TYPE header_type AS (
        header_key TEXT,
        header_value TEXT
      );

      CREATE TABLE request_node(
        id SERIAL PRIMARY KEY,
        request_node_parent_id INTEGER,
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

      CREATE TABLE request_collection_to_request_node(
        request_collection_id INTEGER,
        request_node_id INTEGER REFERENCES request_node,
        PRIMARY KEY (request_collection_id, request_node_id)
      );

      CREATE TABLE account(
          id SERIAL PRIMARY KEY
      );

      CREATE TABLE environment(
          id SERIAL PRIMARY KEY,
          name TEXT NOT NULL
      );

      CREATE TABLE account_environment(
          account_id INTEGER REFERENCES account,
          environment_id INTEGER REFERENCES environment,
          PRIMARY KEY (account_id, environment_id)
      );

      CREATE TABLE key_value(
          id SERIAL PRIMARY KEY,
          environment_id INTEGER REFERENCES environment(id),
          key TEXT NOT NULL,
          value TEXT NOT NULL
      );

      CREATE OR REPLACE FUNCTION request_node_as_js(someRow request_node) RETURNS jsonb AS $$
      BEGIN
        RETURN CASE WHEN someRow.tag = 'RequestFolder' THEN
          jsonb_build_object(
            'id', someRow.id,
            'name', someRow.name,
            'tag', someRow.tag,
            'children', json_build_array()
          )
        ELSE
          jsonb_build_object(
            'id', someRow.id,
            'name', someRow.name,
            'tag', someRow.tag,
            'http_url', someRow.http_url,
            'http_method', someRow.http_method,
            'http_headers', someRow.http_headers,
            'http_body', someRow.http_body
          )
        END;
      END;
      $$ LANGUAGE plpgsql;
    }

  end

  def down
    execute %{
      DROP TABLE request_node;
      DROP TABLE request_collection_to_request_node;

      DROP FUNCTION request_node_as_js;

      REMOVE request_node_type;
      REMOVE http_method_type;
    }
  end
end
