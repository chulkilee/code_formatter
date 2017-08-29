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

  test "function with arrows over multiple lines" do
    assert_same ~S"""
    defp quoted_to_algebra({:__block__, meta, [list]}, _context, state) when is_list(list) do
      case meta[:format] do
        :list_heredoc ->
          string = list |> List.to_string() |> escape_string(:heredoc)
          {@single_heredoc |> line(string) |> concat(@single_heredoc) |> force_break(), state}

        :charlist ->
          string = list |> List.to_string() |> escape_string(@single_quote)
          {@single_quote |> concat(string) |> concat(@single_quote), state}

        _other ->
          list_to_algebra(list, state)
      end
    end
    """
  end

  test "anonymous function with single clause and blocks" do
    assert_same """
    {args_doc, state} = Enum.reduce(args, {[], state}, fn quoted, {acc, state} ->
      {doc, state} = quoted_to_algebra(quoted, :block, state)
      doc = doc |> concat(nest(break(""), :reset)) |> group()
      {[doc | acc], state}
    end)
    """
  end
end
