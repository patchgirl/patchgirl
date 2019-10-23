class Init < ActiveRecord::Migration[5.2]
  def up
    execute %{
      CREATE TYPE request_node_type AS ENUM ('RequestFolder', 'RequestFile');

      CREATE TABLE request_node(
        id serial PRIMARY KEY,
        request_collection_id INTEGER,
        request_node_parent_id INTEGER,
        tag request_node_type,
        name TEXT
      );
    }
  end

  def down
    execute %{
      DROP TABLE request_node;

      REMOVE request_node_type;
    }
  end
end
