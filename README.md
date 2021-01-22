# RealWorld Miso example

This application is built in Miso-JSaddle. While it's feature complete, please
submit bug reports,there are some bugs that I hope the Miso community will help
with before we can post it publicly.

1. Why does updating the address bar not lead to a change in the application
   state, unless you do it twice?
2. Why does including other-modules in the cabal file lead to nix failing to build?
3. Why does the jsaddle app constructed with `stack build` behave differently
   from that build by nix? It fails to decode json objects correctly.
4. Why do links stop working sometimes in dev mode?
5. Why did hot reloading stop working?

## How to run the app

In all cases these instructions will result in the app running at
`http://localhost:8080`

### Development with `nix`
```bash
nix-shell --run reload
```

You can serve the application live in dev mode with hot code reloading.

### Release version w/ `GHCJS`
```bash
nix-build -A release
```

Serve up with any regular webserver, for example:

```
(cd result/bin/app.jsexe; python3 -m http.server 8080)
```

### Development with `stack`

This is particularly useful when running in your favorite IDE. First remove the
existing `miso` package.

```bash
stack exec -- ghc-pkg unregister --force miso
```

Then add the `jsaddle` flag to `package.yaml`.

```yaml
flags:
  miso:
    jsaddle: true
```

After this, you can build as you normally would

```bash
stack build
```
