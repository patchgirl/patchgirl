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

def delete_visitor_data
  %{
     -- delete visitor account
     DELETE FROM account WHERE id = 1;
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

#ActiveRecord::Migration[5.2].execute delete_visitor_data


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


# account


ActiveRecord::Migration[5.2].execute %{
    INSERT INTO account (
      email,
      password
    ) values (
      'visitor@patchgirl.io',
      crypt('123', gen_salt('bf', 8))
    );
}

ActiveRecord::Migration[5.2].execute %{
    INSERT INTO account (
      email,
      password
    ) values (
      'foo@mail.com',
      crypt('123', gen_salt('bf', 8))
    );
}

ActiveRecord::Migration[5.2].execute %{
    INSERT INTO account (
      email,
      password
    ) values (
      'signup@mail.com',
      NULL
    );
}

# request collection


ActiveRecord::Migration[5.2].execute %{
    INSERT INTO request_collection (account_id) values (1);
  }

ActiveRecord::Migration[5.2].execute %{
    INSERT INTO request_collection (account_id) values (2);
  }

ActiveRecord::Migration[5.2].execute %{
    INSERT INTO request_collection (account_id) values (3);
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

# environment

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
      1,
      1
    );

    INSERT INTO account_environment (
      account_id,
      environment_id
    ) values (
      1,
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
