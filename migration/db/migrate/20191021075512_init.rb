class Init < ActiveRecord::Migration[5.2]
  def up
    execute %{

      CREATE TYPE request_node_type AS ENUM ('RequestFolder', 'RequestFile');
      CREATE TYPE http_method_type AS ENUM ('Get', 'Post', 'Put', 'Delete', 'Patch', 'Head', 'Options');

      CREATE TABLE request_node(
        id serial PRIMARY KEY,
        request_node_parent_id INTEGER,
        tag request_node_type,
        name TEXT,
        http_url TEXT,
        http_method http_method_type,
        http_headers TEXT,
        http_body TEXT
      );

      CREATE TABLE request_collection_to_request_node(
        request_collection_id INTEGER,
        request_node_id INTEGER REFERENCES request_node,
        PRIMARY KEY (request_collection_id, request_node_id)
      );

      CREATE OR REPLACE FUNCTION request_node_as_js(someRow request_node) RETURNS jsonb AS $$
      BEGIN
        RETURN CASE WHEN someRow.tag = 'RequestFolder' THEN
          jsonb_build_object(
            'name', someRow.name,
            'tag', someRow.tag
          )
        ELSE
          jsonb_build_object(
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
