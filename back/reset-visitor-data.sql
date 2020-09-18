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


-- * requests


-- *** users/


INSERT INTO request_node (id, request_node_parent_id, tag, name)
VALUES ('58954f35-49ac-45b7-bcf6-c8df1af4b12c', NULL, 'RequestFolder', 'users');

INSERT INTO request_node (id, request_node_parent_id, tag, name, http_url, http_method, http_headers, http_body)
VALUES ('e46ee2de-f1ce-4b13-b1ec-b529ae87da54', '58954f35-49ac-45b7-bcf6-c8df1af4b12c', 'RequestFile', 'list users', 'https://{{apiHost}}/users', 'Get', ARRAY[('key1','value1')]::header_type[], '');

INSERT INTO request_node (id, request_node_parent_id, tag, name, http_url, http_method, http_headers, http_body)
VALUES ('e5324e42-76e5-4fa4-8243-0348dba8c1a8', '58954f35-49ac-45b7-bcf6-c8df1af4b12c', 'RequestFile', 'single user', 'https://{{apiHost}}/users/2', 'Get', ARRAY[]::header_type[], '');

INSERT INTO request_node (id, request_node_parent_id, tag, name, http_url, http_method, http_headers, http_body)
VALUES ('5ff67d3c-28a2-4aa1-b474-4b10dabd2852', '58954f35-49ac-45b7-bcf6-c8df1af4b12c', 'RequestFile', 'create user', 'https://{{apiHost}}/users', 'Post', ARRAY[('key1','value1')]::header_type[], '');

INSERT INTO request_node (id, request_node_parent_id, tag, name, http_url, http_method, http_headers, http_body)
VALUES ('718a67f1-9ff2-4d09-a14a-1b9f4c029a26', '58954f35-49ac-45b7-bcf6-c8df1af4b12c', 'RequestFile', 'update user', 'https://{{apiHost}}/users/2', 'Put', ARRAY[]::header_type[], '');

INSERT INTO request_node (id, request_node_parent_id, tag, name, http_url, http_method, http_headers, http_body)
VALUES ('913d508c-fef3-4034-98da-9e328debb196', '58954f35-49ac-45b7-bcf6-c8df1af4b12c', 'RequestFile', 'delete user', 'https://{{apiHost}}/users/2', 'Delete', ARRAY[]::header_type[], '');


-- *** products/


INSERT INTO request_node (id, request_node_parent_id, tag, name)
VALUES ('da0a3654-5e30-471f-ba03-f87760976981', NULL, 'RequestFolder', 'products');

-- create
INSERT INTO request_node (id, request_node_parent_id, tag, name, http_url, http_method, http_headers, http_body)
VALUES ('b3b24406-a7c0-4c68-bdcc-279e843340a0', 'da0a3654-5e30-471f-ba03-f87760976981', 'RequestFile', 'create', 'https://{{apiHost}}/test/products', 'Post', ARRAY[('Content-Type','application/json')]::header_type[], '{
  "name": "ball",
  "quantity": 10,
  "price": 10
}');

-- delete
INSERT INTO request_node (id, request_node_parent_id, tag, name, http_url, http_method, http_headers, http_body)
VALUES ('588f9c59-6528-4432-9f12-aba40b09e0ec', 'da0a3654-5e30-471f-ba03-f87760976981', 'RequestFile', 'delete', 'https://{{apiHost}}/test/products/{{productId}}', 'Delete', ARRAY[('Content-Type','application/json')]::header_type[], '');

-- show
INSERT INTO request_node (id, request_node_parent_id, tag, name, http_url, http_method, http_headers, http_body)
VALUES ('64b08934-88cb-4fa1-8fed-7d40d2552f4c', 'da0a3654-5e30-471f-ba03-f87760976981', 'RequestFile', 'show', 'https://{{apiHost}}/test/products/{{productId}}', 'Get', ARRAY[('Content-Type','application/json')]::header_type[], '');

-- update
INSERT INTO request_node (id, request_node_parent_id, tag, name, http_url, http_method, http_headers, http_body)
VALUES ('ebd8df25-b372-4b61-a7c0-3ac1391e42aa', 'da0a3654-5e30-471f-ba03-f87760976981', 'RequestFile', 'update', 'https://{{apiHost}}/test/products/{{productId}}', 'Put', ARRAY[('Content-Type','application/json')]::header_type[], '{
  "quantity": 20,
  "price": 20
}');

-- list
INSERT INTO request_node (id, request_node_parent_id, tag, name, http_url, http_method, http_headers, http_body)
VALUES ('9d5873f3-6436-4b5b-96c6-2bb6457e207d', 'da0a3654-5e30-471f-ba03-f87760976981', 'RequestFile', 'list', 'https://{{apiHost}}/test/products', 'Get', ARRAY[('Content-Type','application/json')]::header_type[], '');

-- * pg


INSERT INTO pg_node (id, pg_node_parent_id, tag, name)
VALUES ('cb2c1df8-68f0-4a61-b7c7-f75194604976', NULL, 'PgFolder', 'users');

INSERT INTO pg_node (id, pg_node_parent_id, tag, name, sql, pg_host, pg_password, pg_port, pg_user, pg_dbname)
VALUES ('0c37579e-6a6c-4e9f-ae2c-47a7e7270d14', 'cb2c1df8-68f0-4a61-b7c7-f75194604976', 'PgFile', 'all users',  'SELECT * FROM user_test;', '{{dbHost}}', '{{dbPassword}}', '{{dbPort}}', '{{dbUser}}', '{{dbName}}');

INSERT INTO pg_node (id, pg_node_parent_id, tag, name, sql, pg_host, pg_password, pg_port, pg_user, pg_dbname)
VALUES ('aa517710-150f-4707-a8cc-a24af252acd7', 'cb2c1df8-68f0-4a61-b7c7-f75194604976', 'PgFile', 'single users',  'SELECT * FROM user_test where id = 1 ;', '{{dbHost}}', '{{dbPassword}}', '{{dbPort}}', '{{dbUser}}', '{{dbName}}');

INSERT INTO pg_node (id, pg_node_parent_id, tag, name, sql, pg_host, pg_password, pg_port, pg_user, pg_dbname)
VALUES ('579f1274-045e-403f-b987-22caea511d9b', 'cb2c1df8-68f0-4a61-b7c7-f75194604976', 'PgFile', 'delete users',  'DELETE FROM user_test;', '{{dbHost}}', '{{dbPassword}}', '{{dbPort}}', '{{dbUser}}', '{{dbName}}');

INSERT INTO pg_node (id, pg_node_parent_id, tag, name, sql, pg_host, pg_password, pg_port, pg_user, pg_dbname)
VALUES ('cfb334e7-212c-4b8a-9f39-814538f96046', 'cb2c1df8-68f0-4a61-b7c7-f75194604976', 'PgFile', 'insert user',  'INSERT INTO user_test (firstname, lastname) values (''john'', ''doe'');', '{{dbHost}}', '{{dbPassword}}', '{{dbPort}}', '{{dbUser}}', '{{dbName}}');


-- * request collection


INSERT INTO request_collection (id, account_id)
VALUES (1, '00000000-0000-1000-a000-000000000000');

INSERT INTO request_collection_to_request_node (request_collection_id, request_node_id)
VALUES (1,'58954f35-49ac-45b7-bcf6-c8df1af4b12c');

INSERT INTO request_collection_to_request_node (request_collection_id, request_node_id)
VALUES (1,'da0a3654-5e30-471f-ba03-f87760976981');


-- * pg collection


INSERT INTO pg_collection (id, account_id)
VALUES ('d45a8a8d-c0a3-439d-ac65-3f2992e61b97', '00000000-0000-1000-a000-000000000000');

INSERT INTO pg_collection_to_pg_node (pg_collection_id, pg_actor_id)
VALUES ('d45a8a8d-c0a3-439d-ac65-3f2992e61b97','cb2c1df8-68f0-4a61-b7c7-f75194604976');


-- * scenario


INSERT INTO scenario_collection (account_id, id)
VALUES ('00000000-0000-1000-a000-000000000000', 'a9e3fbc2-de07-40a5-afd8-2460ef1e202c');


-- * environment


INSERT INTO environment (id, name)
VALUES ('98fa7543-aaaf-41a2-9b42-54129bd96551', 'prod');

INSERT INTO environment (id, name)
VALUES ('38668b92-647d-4108-92c8-b539fdc7a7bd', 'dev');

INSERT INTO account_environment (account_id, environment_id)
VALUES ('00000000-0000-1000-a000-000000000000', '98fa7543-aaaf-41a2-9b42-54129bd96551');

INSERT INTO account_environment (account_id, environment_id)
VALUES ('00000000-0000-1000-a000-000000000000', '38668b92-647d-4108-92c8-b539fdc7a7bd');


-- * key values


INSERT INTO key_value (id, environment_id, key, value, hidden)
VALUES (gen_random_uuid(), '98fa7543-aaaf-41a2-9b42-54129bd96551', 'apiHost', 'reqres.in/api', false);
INSERT INTO key_value (id, environment_id, key, value, hidden)
VALUES (gen_random_uuid(), '38668b92-647d-4108-92c8-b539fdc7a7bd', 'apiHost', 'localhost', false);

INSERT INTO key_value (id, environment_id, key, value, hidden)
VALUES (gen_random_uuid(), '98fa7543-aaaf-41a2-9b42-54129bd96551', 'dbHost', 'myPosgresHost', true);
INSERT INTO key_value (id, environment_id, key, value, hidden)
VALUES (gen_random_uuid(), '38668b92-647d-4108-92c8-b539fdc7a7bd', 'dbHost', 'localhost', false);

INSERT INTO key_value (id, environment_id, key, value, hidden)
VALUES (gen_random_uuid(), '98fa7543-aaaf-41a2-9b42-54129bd96551', 'dbPort', '5432', true);
INSERT INTO key_value (id, environment_id, key, value, hidden)
VALUES (gen_random_uuid(), '38668b92-647d-4108-92c8-b539fdc7a7bd', 'dbPort', '5432', false);

INSERT INTO key_value (id, environment_id, key, value, hidden)
VALUES (gen_random_uuid(), '98fa7543-aaaf-41a2-9b42-54129bd96551', 'dbUser', 'postgres', true);
INSERT INTO key_value (id, environment_id, key, value, hidden)
VALUES (gen_random_uuid(), '38668b92-647d-4108-92c8-b539fdc7a7bd', 'dbUser', 'postgres', false);

INSERT INTO key_value (id, environment_id, key, value, hidden)
VALUES (gen_random_uuid(), '98fa7543-aaaf-41a2-9b42-54129bd96551', 'dbPassword', 'somePassw0rd!', true);
INSERT INTO key_value (id, environment_id, key, value, hidden)
VALUES (gen_random_uuid(), '38668b92-647d-4108-92c8-b539fdc7a7bd', 'dbPassword', '', false);

INSERT INTO key_value (id, environment_id, key, value, hidden)
VALUES (gen_random_uuid(), '98fa7543-aaaf-41a2-9b42-54129bd96551', 'dbName', 'myDB', true);
INSERT INTO key_value (id, environment_id, key, value, hidden)
VALUES (gen_random_uuid(), '38668b92-647d-4108-92c8-b539fdc7a7bd', 'dbName', 'test', false);
