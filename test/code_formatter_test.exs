defmodule CodeFormatterTest do
  use ExUnit.Case
  doctest CodeFormatter

  test "greets the world" do
    assert CodeFormatter.hello() == :world
  end
end
