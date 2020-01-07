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

request_nodes.each do |request_node_query|
  puts request_node_query
  ActiveRecord::Migration[5.2].execute request_node_query
end

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

ActiveRecord::Migration[5.2].execute %{
    INSERT INTO account (
      id,
      email,
      password
    ) values (
      1,
      'foo@mail.com',
      crypt('123', gen_salt('bf', 8))
    );
}

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
