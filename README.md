# Firmware Storage, server and client.
Based on a service for uploading and storing compressed firmware binaries the cloud. Rewritten in Erlang and Elixir.

## Server - Storage.
ETS store via REST API.

## Client - FirmwareUploader.
Firmware aggregator service normally run by a cron job, uploads firmwares to server.

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

## Sequence of operations.
```
1. Server starts two GenServers:
    - HTTP REST API via `plug_cowboy` (`lib/storage/storage/router.ex`).
    - ETS cache (`lib/storage/storage/firmware_cache.ex`).
    
Supervisor strategy is one_for_one, as corruption in one should not interfere with the other.  
This is especially true for the REST API, as it is stateless.

2. ETS loads three files into memory, for two customers:
    - "example" -> [example_1_0_0.zip, example_1_0_1.zip]
    - "another" -> [another_1_0_0.zip]

The structure of these files in memory is of a map:
    %{:name => filename, :data => <<binary>>}

3. FirmwareUploader is run (by hand) via terminal:
    $ escript.exe firmware_uploader.erl

It determines that server is missing one firmware (`example_1_0_2.zip`) and proceeds to upload it.  
Should the program be run again (without stopping the server), no new files are uploaded.
```
