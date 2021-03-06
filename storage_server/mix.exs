defmodule Storage.MixProject do
  use Mix.Project

  def project do
    [
      app: :storage,
      version: "0.1.0",
      elixir: "~> 1.9",
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  def application do
    [
      extra_applications: [:logger, :plug_cowboy],
      mod: {Storage.Application, []}
    ]
  end

  defp deps do
    [
      {:plug_cowboy, "~> 2.4.1"}
    ]
  end
end
