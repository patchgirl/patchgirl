-- * delete former visitor data


-- delete visitor account
DELETE FROM account WHERE id = '00000000-0000-1000-a000-000000000000';

-- delete orphan scenario_node
DELETE FROM scenario_node
WHERE id IN (
  SELECT id
  FROM scenario_node
  LEFT JOIN scenario_collection_to_scenario_node ON id = scenario_node_id
  WHERE scenario_node_id IS NULL
);

-- delete orphan scene_node
DELETE FROM scene_node
WHERE id IN (
  SELECT scene_node.id
  FROM scene_node
  LEFT JOIN scenario_node ON scene_node.id = scene_node_id
  WHERE scenario_node.id IS NULL
);

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

-- delete orphan pg_node
DELETE FROM pg_node
WHERE id IN (
  SELECT id
  FROM pg_node
  LEFT JOIN pg_collection_to_pg_node ON id = pg_actor_id
  WHERE pg_actor_id IS NULL
);


-- * insert account


INSERT INTO account (
  id,
  github_id,
  email
) values (
  '00000000-0000-1000-a000-000000000000',
  0,
  'visitor@patchgirl.io'
);


-- * environment


INSERT INTO environment (id, name)
VALUES ('98fa7543-aaaf-41a2-9b42-54129bd96551', 'prod');
INSERT INTO environment (id, name)
VALUES ('38668b92-647d-4108-92c8-b539fdc7a7bd', 'dev');
INSERT INTO environment (id, name)
VALUES ('2b9271c8-2e6c-4886-af8b-9c7eff43fe79', 'staging');


INSERT INTO account_environment (account_id, environment_id)
VALUES ('00000000-0000-1000-a000-000000000000', '98fa7543-aaaf-41a2-9b42-54129bd96551');
INSERT INTO account_environment (account_id, environment_id)
VALUES ('00000000-0000-1000-a000-000000000000', '38668b92-647d-4108-92c8-b539fdc7a7bd');
INSERT INTO account_environment (account_id, environment_id)
VALUES ('00000000-0000-1000-a000-000000000000', '2b9271c8-2e6c-4886-af8b-9c7eff43fe79');


-- * key values


INSERT INTO key_value (id, environment_id, key, value, hidden)
VALUES (gen_random_uuid(), '98fa7543-aaaf-41a2-9b42-54129bd96551', 'apiHost', 'reqres.in/api', false);
INSERT INTO key_value (id, environment_id, key, value, hidden)
VALUES (gen_random_uuid(), '38668b92-647d-4108-92c8-b539fdc7a7bd', 'apiHost', 'localhost', false);
INSERT INTO key_value (id, environment_id, key, value, hidden)
VALUES (gen_random_uuid(), '2b9271c8-2e6c-4886-af8b-9c7eff43fe79', 'apiHost', 'localhost', false);

INSERT INTO key_value (id, environment_id, key, value, hidden)
VALUES (gen_random_uuid(), '98fa7543-aaaf-41a2-9b42-54129bd96551', 'dbHost', 'myPosgresHost', true);
INSERT INTO key_value (id, environment_id, key, value, hidden)
VALUES (gen_random_uuid(), '38668b92-647d-4108-92c8-b539fdc7a7bd', 'dbHost', 'localhost', false);
INSERT INTO key_value (id, environment_id, key, value, hidden)
VALUES (gen_random_uuid(), '2b9271c8-2e6c-4886-af8b-9c7eff43fe79', 'dbHost', 'localhost', false);

INSERT INTO key_value (id, environment_id, key, value, hidden)
VALUES (gen_random_uuid(), '98fa7543-aaaf-41a2-9b42-54129bd96551', 'dbPort', '5432', true);
INSERT INTO key_value (id, environment_id, key, value, hidden)
VALUES (gen_random_uuid(), '38668b92-647d-4108-92c8-b539fdc7a7bd', 'dbPort', '5433', false);
INSERT INTO key_value (id, environment_id, key, value, hidden)
VALUES (gen_random_uuid(), '2b9271c8-2e6c-4886-af8b-9c7eff43fe79', 'dbPort', '5432', false);

INSERT INTO key_value (id, environment_id, key, value, hidden)
VALUES (gen_random_uuid(), '98fa7543-aaaf-41a2-9b42-54129bd96551', 'dbUser', 'postgres', true);
INSERT INTO key_value (id, environment_id, key, value, hidden)
VALUES (gen_random_uuid(), '38668b92-647d-4108-92c8-b539fdc7a7bd', 'dbUser', 'dev', false);
INSERT INTO key_value (id, environment_id, key, value, hidden)
VALUES (gen_random_uuid(), '2b9271c8-2e6c-4886-af8b-9c7eff43fe79', 'dbUser', 'postgres', false);

INSERT INTO key_value (id, environment_id, key, value, hidden)
VALUES (gen_random_uuid(), '98fa7543-aaaf-41a2-9b42-54129bd96551', 'dbPassword', 'somePassw0rd!', true);
INSERT INTO key_value (id, environment_id, key, value, hidden)
VALUES (gen_random_uuid(), '38668b92-647d-4108-92c8-b539fdc7a7bd', 'dbPassword', '', false);
INSERT INTO key_value (id, environment_id, key, value, hidden)
VALUES (gen_random_uuid(), '2b9271c8-2e6c-4886-af8b-9c7eff43fe79', 'dbPassword', '', false);

INSERT INTO key_value (id, environment_id, key, value, hidden)
VALUES (gen_random_uuid(), '98fa7543-aaaf-41a2-9b42-54129bd96551', 'dbName', 'myDB', true);
INSERT INTO key_value (id, environment_id, key, value, hidden)
VALUES (gen_random_uuid(), '38668b92-647d-4108-92c8-b539fdc7a7bd', 'dbName', 'dev', false);
INSERT INTO key_value (id, environment_id, key, value, hidden)
VALUES (gen_random_uuid(), '2b9271c8-2e6c-4886-af8b-9c7eff43fe79', 'dbName', 'test', false);


-- * requests


-- *** users/


INSERT INTO request_node (id, request_node_parent_id, tag, name)
VALUES ('58954f35-49ac-45b7-bcf6-c8df1af4b12c', NULL, 'Folder', 'users');

INSERT INTO request_node (id, request_node_parent_id, tag, name, http_url, http_method, http_headers, http_body)
VALUES ('e46ee2de-f1ce-4b13-b1ec-b529ae87da54', '58954f35-49ac-45b7-bcf6-c8df1af4b12c', 'File', 'list users', 'https://{{apiHost}}/test/users', 'Get', ARRAY[('key1','value1')]::header_type[], '');

INSERT INTO request_node (id, request_node_parent_id, tag, name, http_url, http_method, http_headers, http_body)
VALUES ('e5324e42-76e5-4fa4-8243-0348dba8c1a8', '58954f35-49ac-45b7-bcf6-c8df1af4b12c', 'File', 'show user', 'https://{{apiHost}}/test/users/{{userId}}', 'Get', ARRAY[]::header_type[], '');

INSERT INTO request_node (id, request_node_parent_id, tag, name, http_url, http_method, http_headers, http_body)
VALUES ('5ff67d3c-28a2-4aa1-b474-4b10dabd2852', '58954f35-49ac-45b7-bcf6-c8df1af4b12c', 'File', 'create user', 'https://{{apiHost}}/test/users', 'Post', ARRAY[('Content-Type','application/json')]::header_type[], '{
  "firstname": "John",
  "lastname": "Doe"
}');

INSERT INTO request_node (id, request_node_parent_id, tag, name, http_url, http_method, http_headers, http_body)
VALUES ('718a67f1-9ff2-4d09-a14a-1b9f4c029a26', '58954f35-49ac-45b7-bcf6-c8df1af4b12c', 'File', 'update user', 'https://{{apiHost}}/test/users/{{userId}}', 'Put', ARRAY[('Content-Type','application/json')]::header_type[], '{
  "firstname": "Jane",
  "lastname": "Doe"
}');

INSERT INTO request_node (id, request_node_parent_id, tag, name, http_url, http_method, http_headers, http_body)
VALUES ('913d508c-fef3-4034-98da-9e328debb196', '58954f35-49ac-45b7-bcf6-c8df1af4b12c', 'File', 'delete user', 'https://{{apiHost}}/test/users/{{userId}}', 'Delete', ARRAY[('Content-Type','application/json')]::header_type[], '');


-- *** products/


INSERT INTO request_node (id, request_node_parent_id, tag, name)
VALUES ('da0a3654-5e30-471f-ba03-f87760976981', NULL, 'Folder', 'products');

-- create
INSERT INTO request_node (id, request_node_parent_id, tag, name, http_url, http_method, http_headers, http_body)
VALUES ('b3b24406-a7c0-4c68-bdcc-279e843340a0', 'da0a3654-5e30-471f-ba03-f87760976981', 'File', 'create product', 'https://{{apiHost}}/test/products', 'Post', ARRAY[('Content-Type','application/json')]::header_type[], '{
  "name": "ball",
  "quantity": 10,
  "price": 10
}');

-- delete
INSERT INTO request_node (id, request_node_parent_id, tag, name, http_url, http_method, http_headers, http_body)
VALUES ('588f9c59-6528-4432-9f12-aba40b09e0ec', 'da0a3654-5e30-471f-ba03-f87760976981', 'File', 'delete product', 'https://{{apiHost}}/test/products/{{productId}}', 'Delete', ARRAY[('Content-Type','application/json')]::header_type[], '');

-- show
INSERT INTO request_node (id, request_node_parent_id, tag, name, http_url, http_method, http_headers, http_body)
VALUES ('64b08934-88cb-4fa1-8fed-7d40d2552f4c', 'da0a3654-5e30-471f-ba03-f87760976981', 'File', 'show product', 'https://{{apiHost}}/test/products/{{productId}}', 'Get', ARRAY[('Content-Type','application/json')]::header_type[], '');

-- update
INSERT INTO request_node (id, request_node_parent_id, tag, name, http_url, http_method, http_headers, http_body)
VALUES ('ebd8df25-b372-4b61-a7c0-3ac1391e42aa', 'da0a3654-5e30-471f-ba03-f87760976981', 'File', 'update product', 'https://{{apiHost}}/test/products/{{productId}}', 'Put', ARRAY[('Content-Type','application/json')]::header_type[], '{
  "quantity": 20,
  "price": 20
}');

-- list
INSERT INTO request_node (id, request_node_parent_id, tag, name, http_url, http_method, http_headers, http_body)
VALUES ('9d5873f3-6436-4b5b-96c6-2bb6457e207d', 'da0a3654-5e30-471f-ba03-f87760976981', 'File', 'list products', 'https://{{apiHost}}/test/products', 'Get', ARRAY[('Content-Type','application/json')]::header_type[], '');

-- *** basket/


INSERT INTO request_node (id, request_node_parent_id, tag, name)
VALUES ('47f796ea-68f6-4c1f-8b6d-624ad031e2d8', NULL, 'Folder', 'basket');

-- show
INSERT INTO request_node (id, request_node_parent_id, tag, name, http_url, http_method, http_headers, http_body)
VALUES ('8eb08e68-ebe3-4e20-ad38-2be6b2c98c8d', '47f796ea-68f6-4c1f-8b6d-624ad031e2d8', 'File', 'show basket', 'https://{{apiHost}}/test/basket/{{userId}}', 'Get', ARRAY[('Content-Type','application/json')]::header_type[], '');

-- add to basket
INSERT INTO request_node (id, request_node_parent_id, tag, name, http_url, http_method, http_headers, http_body)
VALUES ('c4295e57-20eb-4bf0-ba0b-f5f070dfe862', '47f796ea-68f6-4c1f-8b6d-624ad031e2d8', 'File', 'add to basket', 'https://{{apiHost}}/test/basket/{{userId}}', 'Post', ARRAY[('Content-Type','application/json')]::header_type[], '{
  "productId": {{productId}},
  "quantity": 100
}');

-- remove from basket
INSERT INTO request_node (id, request_node_parent_id, tag, name, http_url, http_method, http_headers, http_body)
VALUES ('044e39f8-e234-4e85-9690-4cb35b16ed5f', '47f796ea-68f6-4c1f-8b6d-624ad031e2d8', 'File', 'remove from basket', 'https://{{apiHost}}/test/basket/{{userId}}', 'Delete', ARRAY[('Content-Type','application/json')]::header_type[], '{
  "productId": {{productId}},
  "productQuantity": 1
}');

-- * pg


-- ** users


INSERT INTO pg_node (id, pg_node_parent_id, tag, name)
VALUES ('cb2c1df8-68f0-4a61-b7c7-f75194604976', NULL, 'Folder', 'users');

INSERT INTO pg_node (id, pg_node_parent_id, tag, name, sql, pg_host, pg_password, pg_port, pg_user, pg_dbname)
VALUES ('0c37579e-6a6c-4e9f-ae2c-47a7e7270d14', 'cb2c1df8-68f0-4a61-b7c7-f75194604976', 'File', 'select all users',  'SELECT * FROM user_test;', '{{dbHost}}', '{{dbPassword}}', '{{dbPort}}', '{{dbUser}}', '{{dbName}}');

INSERT INTO pg_node (id, pg_node_parent_id, tag, name, sql, pg_host, pg_password, pg_port, pg_user, pg_dbname)
VALUES ('aa517710-150f-4707-a8cc-a24af252acd7', 'cb2c1df8-68f0-4a61-b7c7-f75194604976', 'File', 'select single users',  'SELECT * FROM user_test where id = 1 ;', '{{dbHost}}', '{{dbPassword}}', '{{dbPort}}', '{{dbUser}}', '{{dbName}}');

INSERT INTO pg_node (id, pg_node_parent_id, tag, name, sql, pg_host, pg_password, pg_port, pg_user, pg_dbname)
VALUES ('579f1274-045e-403f-b987-22caea511d9b', 'cb2c1df8-68f0-4a61-b7c7-f75194604976', 'File', 'delete all users',  'DELETE FROM user_test;', '{{dbHost}}', '{{dbPassword}}', '{{dbPort}}', '{{dbUser}}', '{{dbName}}');

INSERT INTO pg_node (id, pg_node_parent_id, tag, name, sql, pg_host, pg_password, pg_port, pg_user, pg_dbname)
VALUES ('cfb334e7-212c-4b8a-9f39-814538f96046', 'cb2c1df8-68f0-4a61-b7c7-f75194604976', 'File', 'insert user',  'INSERT INTO user_test (firstname, lastname, role) values (''john'', ''doe'', ''manager'');', '{{dbHost}}', '{{dbPassword}}', '{{dbPort}}', '{{dbUser}}', '{{dbName}}');


-- ** products


INSERT INTO pg_node (id, pg_node_parent_id, tag, name)
VALUES ('5e49588f-65e6-415b-9b0a-677b9db87a5e', NULL, 'Folder', 'products');

INSERT INTO pg_node (id, pg_node_parent_id, tag, name, sql, pg_host, pg_password, pg_port, pg_user, pg_dbname)
VALUES ('e9dd7586-e0a4-4a3b-a2b7-3f5be1950ee6', '5e49588f-65e6-415b-9b0a-677b9db87a5e', 'File', 'delete all products',  'DELETE FROM product_test;', '{{dbHost}}', '{{dbPassword}}', '{{dbPort}}', '{{dbUser}}', '{{dbName}}');

-- ** basket


INSERT INTO pg_node (id, pg_node_parent_id, tag, name)
VALUES ('fcd4b4af-ad04-4efa-b48e-a5fd56d5133b', NULL, 'Folder', 'basket');

INSERT INTO pg_node (id, pg_node_parent_id, tag, name, sql, pg_host, pg_password, pg_port, pg_user, pg_dbname)
VALUES ('9d07ef17-ca17-471b-b4e2-e26d8c97328d', 'fcd4b4af-ad04-4efa-b48e-a5fd56d5133b', 'File', 'delete user basket',  'DELETE FROM basket_product_test WHERE id = {{userId}};', '{{dbHost}}', '{{dbPassword}}', '{{dbPort}}', '{{dbUser}}', '{{dbName}}');

INSERT INTO pg_node (id, pg_node_parent_id, tag, name, sql, pg_host, pg_password, pg_port, pg_user, pg_dbname)
VALUES ('b87cf618-e4a8-441e-bb48-e65456aed260', 'fcd4b4af-ad04-4efa-b48e-a5fd56d5133b', 'File', 'delete all baskets',  'DELETE FROM basket_product_test;', '{{dbHost}}', '{{dbPassword}}', '{{dbPort}}', '{{dbUser}}', '{{dbName}}');


-- * scene

-- ** reset


INSERT INTO scene_node(
  id,
  scene_node_parent_id,
  actor_type,
  http_actor_id,
  pg_actor_id,
  prescript,
  postscript
) VALUES (
  'e7b29477-a1b0-4ef3-b035-08af0f0cbea1',
  NULL,
  'PgActor',
  NULL,
  'e9dd7586-e0a4-4a3b-a2b7-3f5be1950ee6',
  '',
  ''
);

INSERT INTO scene_node(
  id,
  scene_node_parent_id,
  actor_type,
  http_actor_id,
  pg_actor_id,
  prescript,
  postscript
) VALUES (
  '68e108c3-0674-43b0-b5f1-90cc686f7bb4',
  'e7b29477-a1b0-4ef3-b035-08af0f0cbea1',
  'PgActor',
  NULL,
  'b87cf618-e4a8-441e-bb48-e65456aed260',
  '',
  ''
);

INSERT INTO scene_node(
  id,
  scene_node_parent_id,
  actor_type,
  http_actor_id,
  pg_actor_id,
  prescript,
  postscript
) VALUES (
  '0165aa2e-af7d-4de0-b507-098384327820',
  '68e108c3-0674-43b0-b5f1-90cc686f7bb4',
  'PgActor',
  NULL,
  '579f1274-045e-403f-b987-22caea511d9b',
  '',
  ''
);


-- ** simple project


INSERT INTO scene_node(
  id,
  scene_node_parent_id,
  actor_type,
  http_actor_id,
  pg_actor_id,
  prescript,
  postscript
) VALUES (
  '7d8285a2-ac9c-4485-b96a-7ccc766638ca',
  NULL,
  'HttpActor',
  '5ff67d3c-28a2-4aa1-b474-4b10dabd2852',
  NULL,
  '',
  'var userId = httpResponseBodyAsJson["id"];
set("userId", userId);
assertEqual(httpResponseStatus, 200);'
);

INSERT INTO scene_node(
  id,
  scene_node_parent_id,
  actor_type,
  http_actor_id,
  pg_actor_id,
  prescript,
  postscript
) VALUES (
  '7a7d9c29-815f-46d3-bbcd-061cde184671',
  '7d8285a2-ac9c-4485-b96a-7ccc766638ca',
  'HttpActor',
  'b3b24406-a7c0-4c68-bdcc-279e843340a0',
  NULL,
  '',
  'var productId = httpResponseBodyAsJson["id"];
set("productId", productId);
assertEqual(httpResponseStatus, 200);'
);

INSERT INTO scene_node(
  id,
  scene_node_parent_id,
  actor_type,
  http_actor_id,
  pg_actor_id,
  prescript,
  postscript
) VALUES (
  'ba8081b6-0222-4b6a-8886-aff2abce77cc',
  '7a7d9c29-815f-46d3-bbcd-061cde184671',
  'HttpActor',
  'c4295e57-20eb-4bf0-ba0b-f5f070dfe862',
  NULL,
  '',
  'assertEqual(httpResponseStatus, 200);'
);

INSERT INTO scene_node(
  id,
  scene_node_parent_id,
  actor_type,
  http_actor_id,
  pg_actor_id,
  prescript,
  postscript
) VALUES (
  '536c41f3-03a5-429d-ad9c-4f19019681a8',
  'ba8081b6-0222-4b6a-8886-aff2abce77cc',
  'HttpActor',
  '8eb08e68-ebe3-4e20-ad38-2be6b2c98c8d',
  NULL,
  '',
  'assertEqual(httpResponseStatus, 200);'
);


-- * scenario

-- ** reset all

INSERT INTO scenario_node (id, scenario_node_parent_id, tag, name)
VALUES ('fcd4b4af-ad04-4efa-b48e-a5fd56d5133b', NULL, 'Folder', 'Shopping web app');


INSERT INTO scenario_node (id, scenario_node_parent_id, tag, name)
VALUES ('5bdc3a0e-1731-4213-8020-644bc7bff6e1', 'fcd4b4af-ad04-4efa-b48e-a5fd56d5133b', 'Folder', 'reset everything');

INSERT INTO scenario_node (id, scenario_node_parent_id, tag, name, environment_id, scene_node_id)
VALUES ('9d07ef17-ca17-471b-b4e2-e26d8c97328d', '5bdc3a0e-1731-4213-8020-644bc7bff6e1', 'File', 'delete all users & products', '38668b92-647d-4108-92c8-b539fdc7a7bd', 'e7b29477-a1b0-4ef3-b035-08af0f0cbea1');

INSERT INTO scenario_node (id, scenario_node_parent_id, tag, name)
VALUES ('31333648-03d3-4ddd-a4d0-9a726d9562c4', 'fcd4b4af-ad04-4efa-b48e-a5fd56d5133b', 'Folder', 'create simple project');

INSERT INTO scenario_node (id, scenario_node_parent_id, tag, name, environment_id, scene_node_id)
VALUES ('de687457-8cff-459f-9cc9-95c17baafde3', '31333648-03d3-4ddd-a4d0-9a726d9562c4', 'File', 'create a user with products in their basket', '38668b92-647d-4108-92c8-b539fdc7a7bd', '7d8285a2-ac9c-4485-b96a-7ccc766638ca');


-- * request collection


INSERT INTO request_collection (id, account_id)
VALUES (1, '00000000-0000-1000-a000-000000000000');

INSERT INTO request_collection_to_request_node (request_collection_id, request_node_id)
VALUES (1,'58954f35-49ac-45b7-bcf6-c8df1af4b12c');
INSERT INTO request_collection_to_request_node (request_collection_id, request_node_id)
VALUES (1,'da0a3654-5e30-471f-ba03-f87760976981');
INSERT INTO request_collection_to_request_node (request_collection_id, request_node_id)
VALUES (1,'47f796ea-68f6-4c1f-8b6d-624ad031e2d8');


-- * pg collection


INSERT INTO pg_collection (id, account_id)
VALUES ('d45a8a8d-c0a3-439d-ac65-3f2992e61b97', '00000000-0000-1000-a000-000000000000');

INSERT INTO pg_collection_to_pg_node (pg_collection_id, pg_actor_id)
VALUES ('d45a8a8d-c0a3-439d-ac65-3f2992e61b97','cb2c1df8-68f0-4a61-b7c7-f75194604976');
INSERT INTO pg_collection_to_pg_node (pg_collection_id, pg_actor_id)
VALUES ('d45a8a8d-c0a3-439d-ac65-3f2992e61b97','5e49588f-65e6-415b-9b0a-677b9db87a5e');
INSERT INTO pg_collection_to_pg_node (pg_collection_id, pg_actor_id)
VALUES ('d45a8a8d-c0a3-439d-ac65-3f2992e61b97','fcd4b4af-ad04-4efa-b48e-a5fd56d5133b');


-- * scenario


INSERT INTO scenario_collection (account_id, id)
VALUES ('00000000-0000-1000-a000-000000000000', 'a9e3fbc2-de07-40a5-afd8-2460ef1e202c');

INSERT INTO scenario_collection_to_scenario_node (scenario_collection_id, scenario_node_id)
VALUES ('a9e3fbc2-de07-40a5-afd8-2460ef1e202c','fcd4b4af-ad04-4efa-b48e-a5fd56d5133b');
