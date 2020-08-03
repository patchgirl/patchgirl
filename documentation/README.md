# How to build and run PatchGirl on your computer

This document helps you setup PatchGirl locally.

## Requirements

To build PatchGirl, you'll the following tools and languages:
- elm compiler
- haskell stack

## Building

PatchGirl is composed of 2 main parts, the back that run the web api and the front that handles the UI.
You first want to build the back:

```bash
cd back/
make both # build both the patchgirl web api and patchgirl web runner
```

Then you want to generate the client that makes front/back communication possible:

```bash
cd front/
make client
```

Last but not least, you need to build the front single page app:

```bash
make app
```

## Running

To run this project, you need a config file `back/web.dhall`. You can use the default:

```bash
cd back/
cp web.dhall.dist web.dhall
```

You'll need to setup some reverse proxy. This is what I use for development with Nginx:
```nginx
server {
	server_name dev.patchgirl.io;
	listen 80;
	listen [::]:80;
    return 301 https://dev.patchgirl.io$request_uri;
}

server {
	server_name dev.patchgirl.io;
	listen 443 ssl default_server;
	listen [::]:443 ssl default_server;

    include snippets/snakeoil.conf;
	root ~user/patchgirl/public;
    index index.html;

    location / {
    }

    location /metrics {
        proxy_pass http://localhost:3000;
    }

	location /api {
        proxy_pass http://localhost:3000;
    }

    location /test {
        proxy_pass http://localhost:3000;
    }


	location /public/ {
      	root ~user/patchgirl;
    }
}

```

With the following line in my `/etc/hosts`:

```bash
127.0.0.1 dev.patchgirl.io
```
