def insert_request_node(id, request_node_parent_id, tag, name)
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

request_nodes = [
  insert_request_node(1, nil, 'RequestFolder', "1/"),
  insert_request_node(2, nil, 'RequestFolder', "2/"),
  insert_request_node(3, 1, 'RequestFolder', "1.1/"),
  insert_request_node(4, 3, 'RequestFile', "1.1.1"),
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
