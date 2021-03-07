defmodule FirmwareTest do
  use ExUnit.Case
  alias Storage.Firmware

  describe "customer_from_filename" do
    test "normal" do
      assert Firmware.customer_from_filename("valid_1_0_0.zip") == "valid"
    end

    test "debug file with empty versioning" do
      assert Firmware.customer_from_filename("valid___.zip") == "valid"
    end

    test "debug file raw" do
      assert Firmware.customer_from_filename("valid.zip") == "valid"
    end
  end

  test "from_filename_and_data" do
    name = "example_4_5_6"
    binary = <<0::0, 0::0, 0::0, 0::0>>
    firmware = Firmware.from_filename_and_data(name, binary)

    assert Map.get(firmware, :name) == name
    assert Map.get(firmware, :data) == binary
  end
end
