# Pollution Info Service

Based on [example-servant-elm](https://github.com/haskell-servant/example-servant-elm)


## Usage

**[Important]** 
To set up this project you need to have installed:

* [stack](https://docs.haskellstack.org/en/stable/README/)
* [elm](https://www.npmjs.com/package/elm) via npm

---

1. Clone repository
```bash
$ git clone https://github.com/mat646/pollution-info-service.git
```

2. Change directory to created folder
```bash
$ cd pollution-info-service/
```

3. Download dependencies
```bash
$ make setup
```

4. Build project
```bash
$ make build
```

5. Start server
```bash
$ make server-start
```

6. Go to website
```
http://localhost:3000/
```
## Tests

Included tests can be executed with stack
```bash
$ make test
```

## Documentation

Available at: [**pollution-info-service-docs**](https://mat646.github.io/pollution-info-service-docs/)

Created with [Haddock](https://www.haskell.org/haddock/) build in stack.
