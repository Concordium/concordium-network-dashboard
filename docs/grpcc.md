
`grpcc` doesn't seem to be able to figure out where the protobuf includes are, so I did the following brutalism to get things working (there's a PR to fix this: https://github.com/njpatel/grpcc/pull/61)...

```
cd /usr/local/Cellar/protobuf/3.6.1.3_1/include
cp -rp google ~/work/p2p-client/src/proto
```

Then you can, perhaps checking the exposed port via `docker ps` first;

```
grpcc -i --directory src/proto --proto concordium_p2p_rpc.proto --address 127.0.0.1:8891
```

```
client.peerUptime({}, createMetadata({ authentication: 'rpcadmin'}), printReply);
```
