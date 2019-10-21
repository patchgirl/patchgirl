ActiveRecord::Migration[5.2].execute %{

INSERT INTO request_collection (id, tree) values
(1, '[{"tag":"RequestFolder","children":[],"name":"folder1"},{"tag":"RequestFolder","children":[{"tag":"RequestFolder","children":[],"name":"folder2.2"},{"tag":"RequestFolder","children":[{"tag":"RequestFile","url":"{{url}}/api/people/1","name":"file1"},{"tag":"RequestFile","url":"swapi.co/api/people/2","name":"file2"}],"name":"folder3"}],"name":"folder2"}]')

}
