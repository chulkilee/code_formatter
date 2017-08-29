defmodule CodeFormatter.IntegrationTest do
  use ExUnit.Case, async: true

  import CodeFormatter.Case

  test "empty documents" do
    assert_format "   ", ""
    assert_format "\n", ""
    assert_format ";", ""
  end

  test "function with multiple calls and case" do
    assert_same """
    def equivalent(string1, string2) when is_binary(string1) and is_binary(string2) do
      quoted1 = Code.string_to_quoted!(string1)
      quoted2 = Code.string_to_quoted!(string2)
      case not_equivalent(quoted1, quoted2) do
        {left, right} -> {:error, left, right}
        nil -> :ok
      end
    end
    """
  end

  test "function with long pipeline" do
    assert_same ~S"""
    def to_algebra!(string, opts \\ []) when is_binary(string) and is_list(opts) do
      string
      |> Code.string_to_quoted!(wrap_literals_in_blocks: true, unescape: false)
      |> block_to_algebra(state(opts))
      |> elem(0)
    end
    """
  end
end
