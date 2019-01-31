
The `Concordium_p2p_rpc` module is generated from the protobufs definition in `p2p-client/src/proto/*.proto` file.

To do so, you'll need the `protoc`, `go` and the [elm-protobuf](https://package.elm-lang.org/packages/tiziano88/elm-protobuf/latest/) binary installed.

```
protoc --elm_out=~/work/dashboard/src *.proto
```

### OS X install

I got things to work by:

- `brew install protobuf`
- Installing `go` from https://golang.org/dl/ using the macOS package installer
- Putting the `protoc-gen-elm` [pre-compiled binary](https://github.com/tiziano88/elm-protobuf/releases) in `~/.local/bin`
- Running `protoc --elm_out=~/work/dashboard/src *.proto` in the `p2p-client`'s `src/proto` folder (adjust elm_out path according to your local checkout path for dashboard)
