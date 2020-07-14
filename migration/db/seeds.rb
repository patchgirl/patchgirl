# * util


# ** request folder


def insert_request_folder(id, request_node_parent_id, tag, name)
  %{
    INSERT INTO request_node (
      id,
      request_node_parent_id,
      tag,
      name
    ) values (
      '#{id}',
      #{request_node_parent_id.nil? ? 'NULL' : "'#{request_node_parent_id}'"},
      '#{tag}',
      '#{name}'
    );
  }
end


# ** request file


def insert_request_file(id, request_node_parent_id, tag, name, http_url, http_method, http_headers, http_body)

  %{
    INSERT INTO request_node (
      id,
      request_node_parent_id,
      tag,
      name,
      http_url,
      http_method,
      http_headers,
      http_body
    ) values (
     '#{id}',
      #{request_node_parent_id.nil? ? 'NULL' : "'#{request_node_parent_id}'"},
      '#{tag}',
      '#{name}',
      '#{http_url}',
      '#{http_method}',
      #{http_headers},
      '#{http_body}'
    );
  }
end


# ** pg folder


def insert_pg_folder(id, pg_node_parent_id, tag, name)
  %{
    INSERT INTO pg_node (
      id,
      pg_node_parent_id,
      tag,
      name
    ) values (
      '#{id}',
      #{pg_node_parent_id.nil? ? 'NULL' : "'#{pg_node_parent_id}'"},
      '#{tag}',
      '#{name}'
    );
  }
end


# ** pg file


def insert_pg_file(id, pg_node_parent_id, name, sql, host, password, port, user, dbname)

  %{
    INSERT INTO pg_node (
      id,
      pg_node_parent_id,
      tag,
      name,
      sql,
      pg_host,
      pg_password,
      pg_port,
      pg_user,
      pg_dbname
    ) values (
     '#{id}',
      #{pg_node_parent_id.nil? ? 'NULL' : "'#{pg_node_parent_id}'"},
      'PgFile',
      '#{name}',
      '#{sql}',
      '#{host}',
      '#{password}',
      '#{port}',
      '#{user}',
      '#{dbname}'
    );
  }
end


# ** scenario folder


def insert_scenario_folder(id, tag, name, scenario_node_parent_id)
  %{
    INSERT INTO scenario_node (
      id,
      tag,
      name,
      scenario_node_parent_id,
      environment_id
    ) values (
      '#{id}',
      '#{tag}',
      '#{name}',
      #{scenario_node_parent_id.nil? ? 'NULL' : "'#{scenario_node_parent_id}'"},
      NULL
    );
  }
end


# ** scenario file


def insert_scenario_file(id, tag, name, scenario_node_parent_id, environment_id, scene_node_id)

  %{
    INSERT INTO scenario_node (
      id,
      tag,
      name,
      scenario_node_parent_id,
      environment_id,
      scene_node_id
    ) values (
     '#{id}',
      '#{tag}',
      '#{name}',
      #{scenario_node_parent_id.nil? ? 'NULL' : "'#{scenario_node_parent_id}'"},
      #{environment_id},
      #{scene_node_id.nil? ? 'NULL' : "'#{scene_node_id}'"}
    );
  }
end


# ** scene node


def insert_http_scene_node(id, scene_node_parent_id, scene_node_id, prescript, postscript)

  %{
    INSERT INTO scene_node (
      actor_type,
      id,
      scene_node_parent_id,
      http_actor_id,
      prescript,
      postscript
    ) values (
     'HttpScene',
     '#{id}',
      #{scene_node_parent_id.nil? ? 'NULL' : "'#{scene_node_parent_id}'"},
     '#{scene_node_id}',
     '#{prescript}',
     '#{postscript}'
    );
  }
end

def insert_pg_scene_node(id, scene_node_parent_id, scene_node_id, prescript, postscript)

  %{
    INSERT INTO scene_node (
      actor_type,
      id,
      scene_node_parent_id,
      pg_actor_id,
      prescript,
      postscript
    ) values (
     'PgScene',
     '#{id}',
      #{scene_node_parent_id.nil? ? 'NULL' : "'#{scene_node_parent_id}'"},
     '#{scene_node_id}',
     '#{prescript}',
     '#{postscript}'
    );
  }
end

# ** delete visitor data


def delete_visitor_data
  %{
     -- delete visitor account
     DELETE FROM account WHERE id = '00000000-0000-1000-a000-000000000000';
     -- delete orphan environment
     DELETE FROM environment
     WHERE id IN (
       SELECT id FROM environment
       LEFT JOIN account_environment ON id = environment_id
       WHERE environment_id IS NULL
    );
    -- delete orphan request_node
    DELETE FROM request_node
    WHERE id IN (
      SELECT id
      FROM request_node
      LEFT JOIN request_collection_to_request_node ON id = request_node_id
      WHERE request_node_id IS NULL
    );
  }
end

ActiveRecord::Migration[5.2].execute delete_visitor_data


# * account


ActiveRecord::Migration[5.2].execute %{
    INSERT INTO account (
      id,
      github_id,
      email
    ) values (
      '00000000-0000-1000-a000-000000000000',
      0,
      'visitor@patchgirl.io'
    );
}


# * environment


ActiveRecord::Migration[5.2].execute %{
    INSERT INTO environment (
      name
    ) values (
      'prod'
    );

    INSERT INTO environment (
      name
    ) values (
      'dev'
    );
}

ActiveRecord::Migration[5.2].execute %{
    INSERT INTO account_environment (
      account_id,
      environment_id
    ) values (
      '00000000-0000-1000-a000-000000000000',
      1
    );

    INSERT INTO account_environment (
      account_id,
      environment_id
    ) values (
      '00000000-0000-1000-a000-000000000000',
      2
    );
}


ActiveRecord::Migration[5.2].execute %{

    INSERT INTO key_value (
      environment_id,
      key,
      value
    ) values (
      1,
      'host',
      'reqres.in/api'
    );

    INSERT INTO key_value (
      environment_id,
      key,
      value
    ) values (
      1,
      'user',
      'eve.holt'
    );

    INSERT INTO key_value (
      environment_id,
      key,
      value
    ) values (
      2,
      'host',
      'reqres.in/api'
    );

    INSERT INTO key_value (
      environment_id,
      key,
      value
    ) values (
      2,
      'user',
      'whatever'
    );
}


# * request node


json_headers = %{ARRAY[('Content-Type','application/json')]::header_type[]}
headers = %{ARRAY[('key1','value1')]::header_type[]}
empty_headers = %{ARRAY[]::header_type[]}
body = %{{
  "email": "{{user}}@reqres.in",
  "password": "cityslicka"
}}

request_nodes = [
  insert_request_folder('58954f35-49ac-45b7-bcf6-c8df1af4b12c', nil, 'RequestFolder', "users"),
  insert_request_file(  'e46ee2de-f1ce-4b13-b1ec-b529ae87da54', '58954f35-49ac-45b7-bcf6-c8df1af4b12c', 'RequestFile', "list users", "https://{{host}}/users", "Get", headers, ""),
  insert_request_file(  'e5324e42-76e5-4fa4-8243-0348dba8c1a8', '58954f35-49ac-45b7-bcf6-c8df1af4b12c', 'RequestFile', "single user", "https://{{host}}/users/2", "Get", empty_headers, ""),
  insert_request_file(  '5ff67d3c-28a2-4aa1-b474-4b10dabd2852', '58954f35-49ac-45b7-bcf6-c8df1af4b12c', 'RequestFile', "create user", "https://{{host}}/users", "Post", headers, ""),
  insert_request_file(  '718a67f1-9ff2-4d09-a14a-1b9f4c029a26', '58954f35-49ac-45b7-bcf6-c8df1af4b12c', 'RequestFile', "update user", "https://{{host}}/users/2", "Put", headers, ""),
  insert_request_file(  '913d508c-fef3-4034-98da-9e328debb196', '58954f35-49ac-45b7-bcf6-c8df1af4b12c', 'RequestFile', "delete user", "https://{{host}}/users/2", "Delete", headers, ""),
  insert_request_folder('da0a3654-5e30-471f-ba03-f87760976981', nil, 'RequestFolder', "session"),
  insert_request_file(  'b3b24406-a7c0-4c68-bdcc-279e843340a0', 'da0a3654-5e30-471f-ba03-f87760976981', 'RequestFile', "login successful", "https://{{host}}/login", "Post", json_headers, body),
  insert_request_file(  '6a55626d-d1ec-4255-851d-2b8e18f4bdc4', 'da0a3654-5e30-471f-ba03-f87760976981', 'RequestFile', "login unsuccessful", "https://{{host}}/login", "Post", json_headers, ""),
]

request_nodes.each do |request_node_query|
  puts request_node_query
  ActiveRecord::Migration[5.2].execute request_node_query
end


# * pg node


pg_nodes = [
  insert_pg_folder('cb2c1df8-68f0-4a61-b7c7-f75194604976', nil, 'PgFolder', "users"),
  insert_pg_file(  '0c37579e-6a6c-4e9f-ae2c-47a7e7270d14', 'cb2c1df8-68f0-4a61-b7c7-f75194604976', "select users",  "select * from user_test;", "", "", "", "", ""),
  insert_pg_file(  'aa517710-150f-4707-a8cc-a24af252acd7', 'cb2c1df8-68f0-4a61-b7c7-f75194604976', "single user", "select * from user_test;", "", "", "", "", ""),
]

pg_nodes.each do |pg_node_query|
  puts pg_node_query
  ActiveRecord::Migration[5.2].execute pg_node_query
end


# * scene_node


scene_nodes = [
  insert_http_scene_node('782358aa-e293-4976-b754-6d8db4762dd1', nil, '913d508c-fef3-4034-98da-9e328debb196', '', ''),
  insert_http_scene_node('1932b624-ab1e-48b9-b779-1a3d1fe774c9', '782358aa-e293-4976-b754-6d8db4762dd1', 'e46ee2de-f1ce-4b13-b1ec-b529ae87da54', '', ''),
  insert_http_scene_node('ef6ed855-81f9-4a6c-9cd5-75fd3b819c91', '1932b624-ab1e-48b9-b779-1a3d1fe774c9', 'b3b24406-a7c0-4c68-bdcc-279e843340a0', '', '')
]

scene_nodes.each do |scene_node_query|
  puts scene_node_query
  ActiveRecord::Migration[5.2].execute scene_node_query
end


# * scenario node


scenario_nodes = [
  insert_scenario_folder('5b679f6c-698c-4b01-8ba8-75975e347558', 'ScenarioFolder', "users", nil),
  insert_scenario_file(  '0a0341c1-0f82-4c39-afbd-0e3f92ae003d', 'ScenarioFile', 'scenario1', '5b679f6c-698c-4b01-8ba8-75975e347558', 1, '782358aa-e293-4976-b754-6d8db4762dd1'),
  insert_scenario_file(  'd9c1b525-8a4c-4b8e-aac0-31eaac7bd1ae', 'ScenarioFile', 'scenario2', '5b679f6c-698c-4b01-8ba8-75975e347558', 1, nil),
  insert_scenario_folder('6b9f367f-77dc-4b79-9742-346a48528b64', 'ScenarioFolder', "session", nil),
]

scenario_nodes.each do |scenario_node_query|
  puts scenario_node_query
  ActiveRecord::Migration[5.2].execute scenario_node_query
end


# * request collection


ActiveRecord::Migration[5.2].execute %{
    INSERT INTO request_collection (account_id) values ('00000000-0000-1000-a000-000000000000');
  }

ActiveRecord::Migration[5.2].execute %{
    INSERT INTO request_collection_to_request_node (
      request_collection_id,
      request_node_id
    ) values (1,'58954f35-49ac-45b7-bcf6-c8df1af4b12c');

    INSERT INTO request_collection_to_request_node (
      request_collection_id,
      request_node_id
    ) values (1,'da0a3654-5e30-471f-ba03-f87760976981');
  }


# * pg collection


ActiveRecord::Migration[5.2].execute %{
    INSERT INTO pg_collection (id, account_id) values ('d45a8a8d-c0a3-439d-ac65-3f2992e61b97', '00000000-0000-1000-a000-000000000000');
  }

ActiveRecord::Migration[5.2].execute %{
    INSERT INTO pg_collection_to_pg_node (
      pg_collection_id,
      pg_actor_id
    ) values ('d45a8a8d-c0a3-439d-ac65-3f2992e61b97','cb2c1df8-68f0-4a61-b7c7-f75194604976');
  }


# * scenario collection


ActiveRecord::Migration[5.2].execute %{
    INSERT INTO scenario_collection (account_id, id)
    values ('00000000-0000-1000-a000-000000000000', 'a9e3fbc2-de07-40a5-afd8-2460ef1e202c');
  }

ActiveRecord::Migration[5.2].execute %{
    INSERT INTO scenario_collection_to_scenario_node (
      scenario_collection_id,
      scenario_node_id
    ) values ('a9e3fbc2-de07-40a5-afd8-2460ef1e202c','5b679f6c-698c-4b01-8ba8-75975e347558');

    INSERT INTO scenario_collection_to_scenario_node (
      scenario_collection_id,
      scenario_node_id
    ) values ('a9e3fbc2-de07-40a5-afd8-2460ef1e202c','6b9f367f-77dc-4b79-9742-346a48528b64');
  }
