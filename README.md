# Storage Server and Client.

## Server - Storage.
Based on a service that is running in production, rewritten in Elixir.  

## Client - FirmwareUploader.
Based on a utility tool for above server, originally called by cron job daily to aggregate new firmware files onto a single server, rewritten in Erlang.

## How to install.
```
$ git clone https://github.com/Holmqvist1990/firmware_uploader.git
```

## How to start server.
Requires [Elixir](https://elixir-lang.org/install.html).
```
$ cd storage_server
$ mix deps.get
$ iex -S mix
```

## How to run client.
Requires [Erlang](https://www.erlang.org/downloads).  
Server must be running.
```
$ escript firmware_uploader.erl
```
