defmodule CodeFormatter.Mixfile do
  use Mix.Project

  def project() do
    [
      app: :code_formatter,
      version: "0.1.0",
      elixir: "~> 1.6-dev",
      start_permanent: Mix.env == :prod,
      deps: deps()
    ]
  end

  def application() do
    [
      extra_applications: [:logger]
    ]
  end

  defp deps() do
    []
  end
end
