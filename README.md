# apiTester

## To build the backend API

### install nix

- ```apt-get install -y libgmp-dev```
- ```nix-env -i stack```

### build the haskell API

- go to the `api/` directory
- compile with ```stack build```

## To Run the front end

- install elm 0.19 https://guide.elm-lang.org/install.html
- go to the `front/` directory
- compile `elm make elm/Main.elm --output=../back/public/js/app.js`
- then open the `index.html` file in your browser
