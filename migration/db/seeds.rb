def insert_request_node(id, request_collection_id, request_node_parent_id, tag, name)
  %{
    INSERT INTO request_node (
      id,
      request_collection_id,
      request_node_parent_id,
      tag,
      name
    ) values (
      #{id},
      #{request_collection_id.nil? ? 'NULL' : request_collection_id},
      #{request_node_parent_id.nil? ? 'NULL' : request_node_parent_id},
      '#{tag}',
      '#{name}'
    );
  }
end

request_nodes = [
  insert_request_node(1, 1, nil, 'RequestFolder', "1/"),
  insert_request_node(2, 1, nil, 'RequestFolder', "2/"),
  insert_request_node(3, nil, 1, 'RequestFolder', "1.1/"),
  insert_request_node(4, nil, 3, 'RequestFile', "1.1.1"),
]

request_nodes.each do |request_node_query|
  puts request_node_query
  ActiveRecord::Migration[5.2].execute request_node_query
end
