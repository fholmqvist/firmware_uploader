defmodule Storage.Firmware do
  def customer_from_filename(filename) do
    filename
    |> String.replace(".zip", "")
    |> String.split("_")
    |> List.first()
  end

  def from_filename_and_data(filename, data) do
    %{:name => filename, :data => data}
  end

  def load_from_disk(filename) do
    %{:name => filename, :data => File.read!("../example_files/" <> filename)}
  end
end
