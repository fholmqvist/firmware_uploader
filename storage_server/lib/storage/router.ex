defmodule Storage.Router do
  use Plug.Router

  alias Storage.Firmware
  alias Storage.FirmwareCache

  plug(Plug.Logger, log: :debug)
  plug(:match)
  plug(:dispatch)


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
