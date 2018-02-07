# Pollution Info Service

Based on [example-servant-elm](https://github.com/haskell-servant/example-servant-elm)

See it in action [**pollution-info-service**](http://178.62.236.241:3000/)

![Example](https://image.ibb.co/nsxNGH/Screenshot_2018_2_7_Main.png)

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

## Deployment

Project can be containerized with docker and deployed on cloud application platforms such as [DigitalOcean](https://www.digitalocean.com/).

**[Important]** 
To create image of project you need to have docker installed.

1. Clone repository
```bash
$ git clone https://github.com/mat646/pollution-info-service.git
```

2. Change directory to created folder
```bash
$ cd pollution-info-service/
```

3. Create docker image
```bash
$ docker build -t pollution-info-service .
```

3. Start image
```bash
$ docker run --rm -p 3000:3000 pollution-info-service
```

## Tests

Included tests can be executed with stack
```bash
$ make test
```

## Documentation

Available at: [**pollution-info-service-docs**](https://mat646.github.io/pollution-info-service-docs/)

Created with [Haddock](https://www.haskell.org/haddock/) build in stack.

## Worth seeing

* [blog post about servant on heroku](https://arow.info/blog/posts/2017-03-30-servant-on-heroku#fnref2)
* [proper way to install node.js on Ubuntu](https://www.digitalocean.com/community/tutorials/how-to-install-node-js-on-ubuntu-16-04)
