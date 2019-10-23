class Init < ActiveRecord::Migration[5.2]
  def up
    execute %{
      CREATE TABLE request_collection (id serial PRIMARY KEY, tree JSON NOT NULL);
    }
  end

  def down
    execute %{
      DROP TABLE request_collection;
    }
  end
end
