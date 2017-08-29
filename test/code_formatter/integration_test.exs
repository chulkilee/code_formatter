defmodule CodeFormatter.IntegrationTest do
  use ExUnit.Case, async: true

  import CodeFormatter.Case

  test "empty documents" do
    assert_format "   ", ""
    assert_format "\n", ""
    assert_format ";", ""
  end

  test "function with multiple calls" do
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
end
