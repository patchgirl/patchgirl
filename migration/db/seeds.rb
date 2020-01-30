def insert_request_folder(id, request_node_parent_id, tag, name)
  %{
    INSERT INTO request_node (
      id,
      request_node_parent_id,
      tag,
      name
    ) values (
      #{id},
      #{request_node_parent_id.nil? ? 'NULL' : request_node_parent_id},
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
      #{id},
      #{request_node_parent_id.nil? ? 'NULL' : request_node_parent_id},
      '#{tag}',
      '#{name}',
      '#{http_url}',
      '#{http_method}',
      #{http_headers},
      '#{http_body}'
    );
  }
end

def insert_request_folder2(id, request_node_parent_id, tag, name)
  %{
    INSERT INTO request_node2 (
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

def insert_request_file2(id, request_node_parent_id, tag, name, http_url, http_method, http_headers, http_body)

  %{
    INSERT INTO request_node2 (
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

headers = %{ARRAY[('key1','value1')::header_type]}
empty_headers = %{ARRAY[]::header_type[]}

request_nodes = [
  insert_request_folder(1, nil, 'RequestFolder', "testApi"),
  insert_request_folder(2, nil, 'RequestFolder', "2/"),
  insert_request_folder(3, 1, 'RequestFolder', "1.1/"),
  insert_request_file(4, 3, 'RequestFile', "1.1.1", "https://{{host}}/requestCollection/1", "Get", headers, ""),
  insert_request_file(5, 3, 'RequestFile', "swapi/people", "https://swapi.co/api/people/1/", "Get", empty_headers, ""),
  insert_request_file(6, 1, 'RequestFile', "delete - no content", "https://{{host}}/test/deleteNoContent", "Delete", headers, ""),
  insert_request_file(7, 1, 'RequestFile', "get - not found", "https://{{host}}/test/getNotFound", "Get", headers, ""),
  insert_request_file(8, 1, 'RequestFile', "get - internal server error", "https://{{host}}/test/getInternalServerError", "Get", headers, ""),
]

request_nodes2 = [
  insert_request_folder2('02d6410a-bb1d-423f-b1c0-5da946c9fb8c', nil, 'RequestFolder', "testApi"),
  insert_request_folder2('58954f35-49ac-45b7-bcf6-c8df1af4b12c', nil, 'RequestFolder', "2/"),
  insert_request_folder2('57ee16c9-f0bc-44d7-823b-fcd301fef335', '58954f35-49ac-45b7-bcf6-c8df1af4b12c', 'RequestFolder', "1.1/"),
  insert_request_file2(  'e46ee2de-f1ce-4b13-b1ec-b529ae87da54', '57ee16c9-f0bc-44d7-823b-fcd301fef335', 'RequestFile', "1.1.1", "https://{{host}}/requestCollection/1", "Get", headers, ""),
  insert_request_file2(  'e5324e42-76e5-4fa4-8243-0348dba8c1a8', '57ee16c9-f0bc-44d7-823b-fcd301fef335', 'RequestFile', "swapi/people", "https://swapi.co/api/people/1/", "Get", empty_headers, ""),
  insert_request_file2(  '5ff67d3c-28a2-4aa1-b474-4b10dabd2852', '58954f35-49ac-45b7-bcf6-c8df1af4b12c', 'RequestFile', "delete - no content", "https://{{host}}/test/deleteNoContent", "Delete", headers, ""),
  insert_request_file2(  '718a67f1-9ff2-4d09-a14a-1b9f4c029a26', '58954f35-49ac-45b7-bcf6-c8df1af4b12c', 'RequestFile', "get - not found", "https://{{host}}/test/getNotFound", "Get", headers, ""),
  insert_request_file2(  '913d508c-fef3-4034-98da-9e328debb196', '58954f35-49ac-45b7-bcf6-c8df1af4b12c', 'RequestFile', "get - internal server error", "https://{{host}}/test/getInternalServerError", "Get", headers, ""),
]

request_nodes.each do |request_node_query|
  puts request_node_query
  ActiveRecord::Migration[5.2].execute request_node_query
end

request_nodes2.each do |request_node_query|
  puts request_node_query
  ActiveRecord::Migration[5.2].execute request_node_query
end


# account


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
    INSERT INTO request_collection (
      id,
      account_id
    ) values (1,1);
  }

ActiveRecord::Migration[5.2].execute %{
    INSERT INTO request_collection_to_request_node (
      request_collection_id,
      request_node_id
    ) values (1,1);
    INSERT INTO request_collection_to_request_node (
      request_collection_id,
      request_node_id
    ) values (1,2);
  }

# environment

ActiveRecord::Migration[5.2].execute %{
    INSERT INTO environment (
      name
    ) values (
      'dev'
    );

    INSERT INTO environment (
      name
    ) values (
      'stage'
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
      'localhost:3000'
    );

    INSERT INTO key_value (
      environment_id,
      key,
      value
    ) values (
      1,
      'key2',
      'value2'
    );

    INSERT INTO key_value (
      environment_id,
      key,
      value
    ) values (
      2,
      'key3',
      'value3'
    );
}
