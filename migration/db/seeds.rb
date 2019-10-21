request_nodes = %{
'[
  {
    "tag": "RequestFile",
    "name": "someRequest1",
    "url": "someUrl1",
    "method": "Get",
    "headers": [],
    "body": ""
  }, {
    "tag": "RequestFolder",
    "name": "someFolder1",
    "children": [
      {
        "tag": "RequestFile",
        "name": "someRequest2",
        "url": "someUrl2",
        "method": "Get",
        "headers": [],
        "body": ""
      }
    ]
  }
]'
}

ActiveRecord::Migration[5.2].execute %{
INSERT INTO request_collection (id, tree) values (1, #{request_nodes})
}
