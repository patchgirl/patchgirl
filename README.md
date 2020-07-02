# PatchGirl

[![Build Status](https://travis-ci.com/patchgirl/patchgirl.svg?branch=master)](https://travis-ci.com/patchgirl/patchgirl)

A postman/postwoman like, web app to test your APIs !

## How to use

To use PatchGirl, you only need to go to the [app page 🚀](https://patchgirl.io).
If you want to make request to an API running on your computer (e.g: localhost), you need to run the [patchgirl-runner app](https://github.com/patchgirl/patchgirl/releases).
Running the patchgirl-runner app is simple, just download it and execute it in a terminal 🚀 (nb: only linux platforms are supported as of today, MacOS and Windows are on the roadmap).

## Features

✅: available<br/>
🔧: in development<br/>
📝: in the roadmap<br/>

| available | feature                              | description                                                                                                                                   |
|-----------|--------------------------------------|-----------------------------------------------------------------------------------------------------------------------------------------------|
| ✅        | sign in via github                   |                                                                                                                                               |
| ✅        | environment                          | Manually set variables in an environment. Switch the current environment so you can use the associated variables                              |
| ✅        | request collections                  | Save and organize your requests in collections                                                                                                |
| ✅        | scenario of request                  | Play multiple requests one after another                                                                                                      |
| ✅        | pre request script                   | Play a script right before executing a request (eg: Include timestamp in the request headers, send a random string in the URL parameters,...) |
| ✅        | post request script                  | Play a script right after executing a request (eg: Assign the response result to a variable, assert that the response status is a 200,...)    |
| ✅        | desktop app (only Linux as of today) | Lightweight cross platform desktop app (So it's always available even when no internet connection, solve localhost CORS issues                |
| ✅        | call to localhost (with desktop app) | Localhost targeted request do not work because of [CORS](https://developer.mozilla.org/fr/docs/Web/HTTP/CORS)                                 |
| 📝        | Graphql                              | You can currently send graphql request by manually building a POST request but there is no schema preview or field autocomplete feature yet   |
| 📝        | websocket                            |                                                                                                                                               |
| 📝        | server send events                   |                                                                                                                                               |
| 📝        | API documentation                    | Generate documentation from a set of requests                                                                                                 |
| 📝        | Team mode                            | Share your collections with your team                                                                                                         |
| 📝        | Keyboard shortcuts                   |                                                                                                                                               |


📢 Scenario of tests

Play multiple requests one after another. This is useful if you want to:
- Automate your development environment db (eg: create many users,...)
- Automate the test of your API (eg: create a user then show it then delete it)

📜 pre-request script

Run a javascript script right before a request is executed. This is useful if you want to:
 - Include timestamp in the request headers
 - Send a random string in the URL parameters

📜 post-request script

Run a javascript script after a request is executed. This is useful if you want to:
 - Assert for a response status or body
 - Set variables according on the response body

📦 Desktop app

Install a desktop app (Only Linux at the moment) so you can use PatchGirl locally. This is useful if you want to:
- Use PatchGirl without internet access
- Call localhost url (This would also solve browser CORS restriction)
- Have faster request execution (no round trip to a remote server, everything is done on your computer)

## Architecture

![architecture](architecture_schema.svg)

PatchGirl is only built with functional programing languages:
- **NixOS** for the operating system
- **Haskell** for the backend API
- **Elm** for the front end single page app
