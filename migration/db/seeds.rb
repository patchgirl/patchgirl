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
  insert_request_folder(1, nil, 'RequestFolder', "1/"),
  insert_request_folder(2, nil, 'RequestFolder', "2/"),
  insert_request_folder(3, 1, 'RequestFolder', "1.1/"),
  insert_request_file(4, 3, 'RequestFile', "1.1.1", "api.com", "Get", headers, ""),
  insert_request_file(5, 3, 'RequestFile', "1.1.2", "api.com", "Post", empty_headers, ""),
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
