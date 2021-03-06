defmodule DummyServerTest do
  use ExUnit.Case
  doctest DummyServer

  test "greets the world" do
    assert DummyServer.hello() == :world
  end
end
