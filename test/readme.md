
### Websocket testing with Artillery

To get the `artillery` cli;

```
npm install -g artillery
```

To run the configuration specified in `artillery.yml`;

```
artillery run test/artillery.yml
```

:warning: You probably don't want to blindly run this, look at the contents of the test first.

It's worth reading https://artillery.io/docs/basic-concepts/ to understand the philosophy behind how artillery does load testing.
