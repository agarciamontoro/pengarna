Pengar
======

A simple web app for your hledger-controlled money.

# Development

Install [`elm`](https://guide.elm-lang.org/install.html) and [`elm-live`](https://github.com/wking-io/elm-live). Make sure you have [`hledger web`](https://github.com/simonmichael/hledger) serving the API with CORS enabled (see [this issue](https://github.com/simonmichael/hledger/issues/1031) to know how to build a custom hledger with it) and run:

```
elm-live src/Main.elm --open --pushstate --start-page=custom_index.html -- --debug --output=elm.js
```

This should open the web app in your browser.