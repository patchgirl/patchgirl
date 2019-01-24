module Postman.App exposing (..)

import Postman.Model exposing (Model)
import Postman.Message exposing (Msg(..))

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Import -> (imported, Cmd.none)

imported : String
imported = """
{
  "variables": [],
  "info": {
    "name": "swapi",
    "_postman_id": "457d1d75-3bca-e9a3-d6a5-622cf211f664",
    "description": "",
    "schema": "https://schema.getpostman.com/json/collection/v2.0.0/collection.json"
  },
  "item": [
    {
      "name": "get person",
      "request": {
	"url": "https://swapi.co/api/people/1",
	"method": "GET",
	"header": [],
	"body": {},
	"description": ""
      },
      "response": []
    },
    {
      "name": "delete person",
      "request": {
	"url": "https://swapi.co/api/people/1",
	"method": "DELETE",
	"header": [],
	"body": {},
	"description": ""
      },
      "response": []
    }
  ]
}
"""
