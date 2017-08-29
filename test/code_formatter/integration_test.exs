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

  test "function with long guards" do
    assert_same """
    defp module_attribute_read?({:@, _, [{var, _, var_context}]}) when
           is_atom(var) and is_atom(var_context) do
      Code.Identifier.classify(var) == :callable_local
    end
    """
  end

  test "anonymous function with single clause and blocks" do
    assert_same """
    {args_doc, state} =
      Enum.reduce(args, {[], state}, fn quoted, {acc, state} ->
        {doc, state} = quoted_to_algebra(quoted, :block, state)
        doc = doc |> concat(nest(break(""), :reset)) |> group()
        {[doc | acc], state}
      end)
    """
  end

  test "cond with long clause args" do
    assert_same """
    cond do
      parent_prec == prec and parent_assoc == side ->
        binary_op_to_algebra(op, op_string, left, right, context, state, op_info, nesting)

      parent_op in @required_parens_on_binary_operands or parent_prec > prec or
          parent_prec == prec and parent_assoc != side ->
        {operand, state} =
          binary_op_to_algebra(op, op_string, left, right, context, state, op_info, 2)

        {concat(concat("(", nest(operand, 1)), ")"), state}

      true ->
        binary_op_to_algebra(op, op_string, left, right, context, state, op_info, 2)
    end
    """
  end

  test "type with multiple |" do
    assert_same """
    @type t ::
            binary
            | :doc_nil
            | :doc_line
            | doc_string
            | doc_cons
            | doc_nest
            | doc_break
            | doc_group
            | doc_color
            | doc_force
            | doc_cancel
    """
  end

  test "function with operator and pipeline" do
    assert_same """
    defp apply_cancel_break?({fun, meta, args}) when is_atom(fun) and is_list(args) do
      meta[:terminator] in [@double_heredoc, @single_heredoc] and
        fun |> Atom.to_string() |> String.starts_with?("sigil_")
    end
    """
  end
end
