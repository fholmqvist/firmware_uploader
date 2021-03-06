defmodule Storage.FirmwareCache do
  use GenServer

  alias Storage.Firmware

  def start_link(_args), do: GenServer.start_link(__MODULE__, :ok, name: __MODULE__)

  def init(:ok) do
    table = :ets.new(:firmware, [:named_table, :protected])
    insert_dummy_data(table)
    {:ok, table}
  end

  def find(key) do
    case :ets.lookup(:firmware, key) do
      [{^key, items}] -> {:ok, items}

      [] -> :error
    end
  end

  def add_customer(customer), do: GenServer.call(__MODULE__, {:add_customer, customer})

  def add_firmware(customer, firmware), do: GenServer.call(__MODULE__, {:add_firmware, customer, firmware})

  def get_firmware(filename), do: GenServer.call(__MODULE__, {:get_firmware, filename})

  def handle_call({:add_customer, customer}, _from, table) do
    case find(customer) do
      {:ok, firmwares} ->
        {:reply, {:ok, firmwares}, table}

      :error ->
        :ets.insert(table, {customer, []})
        {:reply, {:ok, []}, table}
      end
  end

  def handle_call({:add_firmware, customer, firmware}, _from, table) do
    case find(customer) do
      {:ok, firmwares} ->
        firmwares = [firmware | firmwares]
        :ets.insert(table, {customer, firmwares})
        {:reply, {:ok, firmwares}, table}

      :error ->
        {:reply, {:error, :customer_not_found}, table}
      end
  end

  def handle_call({:get_firmware, filename}, _from, table) do
    case find(Firmware.customer_from_filename(filename)) do
      {:ok, firmwares} ->
        firmware = get_firmware_data_from_firmwares(firmwares, filename)
        {:reply, {:ok, firmware}, table}

      :error ->
        {:reply, {:error, :customer_not_found}, table}
    end
  end

  defp get_firmware_data_from_firmwares(firmwares, filename) do
    case firmwares
        |> Enum.filter(fn f -> Map.get(f, :name, %{}) == filename end)
        |> Enum.map(&(Map.get(&1, :data, "")))
        |> List.first() do
          nil -> ""
          val -> val
        end
  end

  defp insert_dummy_data(table) do
    examples = [
      Firmware.load_from_disk("example_1_0_0.zip"),
      Firmware.load_from_disk("example_1_0_1.zip")
    ]
    :ets.insert(table, {"example", examples})

    anothers = [
      Firmware.load_from_disk("another_1_0_0.zip")
    ]
    :ets.insert(table, {"another", anothers})
  end


end
