# Storage Server.

Based on a service that is running in production, rewritten in Elixir.

Uploads, serves and stores firmware for different device models over a range of customers.

## How to start.
Requires [Elixir](https://elixir-lang.org/install.html).
```
> mix deps.get
> iex -S mix
```

## Original.
```
- Permanent storage in Azure.
- Interfaces other cloud services and handheld devices.
```

## This version.
```
- Memory storage via ETS.
- Called by `../firmware_uploader.erl`.
```