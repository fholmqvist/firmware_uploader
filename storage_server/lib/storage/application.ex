defmodule Storage.Application do
  use Application

  def start(_type, _args) do
    children = [
      Plug.Cowboy.child_spec(scheme: :http, plug: Storage.Router, options: [port: 8007]),
      Storage.FirmwareCache
    ]

    Supervisor.start_link(children, [strategy: :one_for_all, name: Storage.Supervisor])
  end
end
