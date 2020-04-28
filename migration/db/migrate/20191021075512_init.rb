class Init < ActiveRecord::Migration[5.2]

  def up
    if Rails.env.test? then
      execute %{
        ALTER ROLE postgres SET client_min_messages TO WARNING; -- to avoid log when truncating in test
      }
    end

    init = File.expand_path('./init.sql', __dir__)
    init_content = File.open(init).read
    execute init_content
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
