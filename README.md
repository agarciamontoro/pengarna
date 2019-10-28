Pengarna
========

Pengarna (/ˈpɛŋaɳa/) is a simple web app for your hledger-controlled money.

# Development

Install [`elm`](https://guide.elm-lang.org/install.html) and [`elm-live`](https://github.com/wking-io/elm-live). Make sure you have the latest (from source!) [`hledger-web`](https://github.com/simonmichael/hledger) installed and execute it with CORS enabled:

```bash
hledger web --serve-api --cors "*"
```

Then, run:

```
elm-live src/Main.elm --open --pushstate --start-page=custom_index.html -- --debug --output=elm.js
```

This should open the web app in your browser.
