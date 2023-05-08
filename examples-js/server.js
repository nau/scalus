const http = require('http');
const fs = require('fs').promises;

const requestListener = function (req, res) {
  console.log(req.url)
  if (req.url === "/examples-js-fastopt.js") {
    fs.readFile(__dirname + "/target/scala-3.2.2/examples-js-fastopt.js")
        .then(contents => {
            res.setHeader("Content-Type", "text/javascript");
            res.writeHead(200);
            res.end(contents);
        })
  } else if (req.url === "/apikey") {
    fs.readFile(__dirname + "/../blockfrost_api_key.txt")
        .then(contents => {
            res.setHeader("Content-Type", "text/plain");
            res.writeHead(200);
            res.end(contents);
        })
  } else {
    fs.readFile(__dirname + "/index.html")
        .then(contents => {
            res.setHeader("Content-Type", "text/html");
            res.writeHead(200);
            res.end(contents);
        })
  }
}

const server = http.createServer(requestListener);
server.listen(8080);