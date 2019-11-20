Pengarna
========

Pengarna (/ˈpɛŋaɳa/) is a simple web app for your hledger-controlled money. It uses the API served by `hledger web`, rendering your balances and transactions in a human-readable way.

> Pengarna is a work-in-progress, so expect rough edges, hardcoded values and very specific code that does not generalize well enough to your use case.

# Configuration

## Dependencies

- [`hledger`](https://github.com/simonmichael/hledger) (the latest stable release does not contain the possibility to add CORS to the API, so make sure you build it from source, using the latest commit in master)
- [`elm`](https://guide.elm-lang.org/install.html)

**Optional but recommended**:

- [`elm-live`](https://github.com/wking-io/elm-live): if you want to contribute to the code and have a comfortable development environment.
- [`elm-doc`](https://github.com/ento/elm-doc): if you want to generate the documentation of the modules locally.

## API

The application expects a working server listening for petitions to hledger's API. In order to set the API server up, run the following command, where the value of the `--host` option is the hostname the machine will be contacted in. By default, it launches a server listening in port `5000`.

```bash
hledger-web --serve-api --cors "*" --host "localhost"
```

For now, the host URL is hardcoded into `src/Api.elm`, so make sure that you set the value in the `host` function to the correct URL. In this case, it should be `http://localhost:5000`.

# Development

Running just `make` will launch `elm-live`, that opens the application in your browser and watches for any change in your source files, re-compiling when needed. This is the same as running `make live`.

There are other `make` commands available:

- `make debug` compiles the project into a debug `elm.js`.
- `make prod` compiles the project into a production-ready, optimized and minified `elm.min.js`.
- `make doc` generates the documentation locally, for which you need `elm-doc` installed.

# Deploying

There is one more `make` command: `make deploy`, which compiles the project ready for production and deploys it, via SSH, into a remote machine that is serving the web application.

This set-up assumes that you have SSH access (configured without password) to a remote machine that contains a directory that is being served. The configuration of this data is in the following `Makefile` variables:
- `REMOTE`: a string containing the user and hostname to connect to the machine; for example, if your user is `jane` and the machine hostname is `192.168.1.10`, this variable should be set to `jane@192.168.1.10`.
- `REMOTE_DIR`: a string containing the full path to the directory from where the web application is served.
