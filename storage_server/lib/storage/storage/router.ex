defmodule Storage.Router do
  use Plug.Router

  alias Storage.Firmware
  alias Storage.FirmwareCache

  plug(Plug.Logger, log: :debug)
  plug(:match)
  plug(:dispatch)

  # There isn't enough code in here to justify a separation
  # of concern just yet, but if this file gets expanded the
  # logic for what happens on each route should be broken
  # out to a dedicated service.

  # This would help readability and maintainability as
  # each file would have more clear and direct responsibilities.

  # For a larger project, it would most likely help to start
  # with the router and subsequent service(s) already separated.

  get "/" do
    send_resp(conn, 200, "")
  end

  get "/api/firmware/for/:customer" do
    case FirmwareCache.find(customer) do
      {:ok, values} ->
        result = values
          |> Enum.map(&(Map.get(&1, :name, "")))
          |> Enum.join(",")
        IO.puts result
        send_resp(conn, 200, result)

      :error ->
        send_resp(conn, 404, "")
    end
  end

  get "/api/firmware/:filename" do
    case FirmwareCache.get_firmware(filename) do
      {:ok, firmware} -> send_resp(conn, 200, firmware)
      {:error, _} -> send_resp(conn, 404, "")
    end
  end

  post "/api/firmware/:filename" do
    {:ok, body, conn} = read_body(conn, [])
    customer = Firmware.customer_from_filename(filename)
    firmware = Firmware.from_filename_and_data(filename, body)
    case FirmwareCache.add_firmware(customer, firmware) do
      {:ok, _} -> send_resp(conn, 200, "")
      {:error, _} -> send_resp(conn, 404, "")
    end
  end

  match _ do
    send_resp(conn, 404, "not found")
  end
end
