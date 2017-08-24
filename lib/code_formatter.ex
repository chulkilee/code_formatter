defmodule CodeFormatter do
  import Inspect.Algebra, except: [format: 2], warn: false

  @line_length 98
  @double_quote "\""
  @double_heredoc "\"\"\""
  @single_quote "'"
  @single_heredoc "'''"

  # Operators that do not have space between operands
  @no_space_binary_operators [:..]

  # Operators that do not have newline between operands
  @no_newline_binary_operators [:\\, :in]

  # Operators that start on the next line in case of breaks
  @left_new_line_before_binary_operators [:|>, :~>>, :<<~, :~>, :<~, :<~>, :<|>]
  @right_new_line_before_binary_operators [:|, :when]

  # Operators that always require parens on operands when they are the parent
  @required_parens_on_binary_operands [:|>, :<<<, :>>>, :<~, :~>, :<<~, :~>>, :<~>, :<|>,
                                       :^^^, :in, :++, :--, :.., :<>]

  @doc """
  Formats the given code `string`.
  """
  def format(string, opts \\ []) do
    line_length = Keyword.get(opts, :line_length, @line_length)

    string
    |> to_algebra(opts)
    |> Inspect.Algebra.format(line_length)
  end

  @doc """
  Converts `string` to an algebra document.
  """
  def to_algebra(string, _opts \\ []) do
    string
    |> Code.string_to_quoted!(wrap_literals_in_blocks: true, unescape: false)
    |> quoted_to_algebra(:argument, state())
    |> elem(0)
  end

  defp state do
    %{}
  end

  defp quoted_to_algebra({:<<>>, meta, entries}, :argument, state) do
    cond do
      not interpolated?(entries) ->
        raise "not yet implemented"
      meta[:format] == :bin_heredoc ->
        {doc, state} = interpolation_to_algebra(entries, :none, state, empty(), @double_heredoc)
        {line(@double_heredoc, doc), state}
      true ->
        interpolation_to_algebra(entries, @double_quote, state, @double_quote, @double_quote)
    end
  end

  defp quoted_to_algebra({{:., _, [String, :to_charlist]}, _, [{:<<>>, meta, entries}]} = quoted,
                         :argument, state) do
    cond do
      not interpolated?(entries) ->
        remote_to_algebra(quoted, state)
      meta[:format] == :list_heredoc ->
        {doc, state} = interpolation_to_algebra(entries, :none, state, empty(), @single_heredoc)
        {line(@single_heredoc, doc), state}
      true ->
        interpolation_to_algebra(entries, @single_quote, state, @single_quote, @single_quote)
    end
  end

  defp quoted_to_algebra({{:., _, [:erlang, :binary_to_atom]}, _,
                          [{:<<>>, _, entries}, :utf8]} = quoted,
                         :argument, state) do
    if interpolated?(entries) do
      interpolation_to_algebra(entries, @double_quote, state, ":\"", @double_quote)
    else
      remote_to_algebra(quoted, state)
    end
  end

  defp quoted_to_algebra({:__block__, meta, [list]}, :argument, state)
      when is_list(list) do
    cond do
      meta[:format] == :list_heredoc ->
        string = list |> List.to_string |> escape_string(:none)
        {@single_heredoc |> line(string) |> concat(@single_heredoc), state}
      Enum.all?(list, & &1 in 0x0..0xFFFFFF) ->
        string = list |> List.to_string |> escape_string(@single_quote)
        {@single_quote |> concat(string) |> concat(@single_quote), state}
      true ->
        raise "not yet implemented"
    end
  end

  defp quoted_to_algebra({:__block__, meta, [string]}, :argument, state)
       when is_binary(string) do
    if meta[:format] == :bin_heredoc do
      string = escape_string(string, :none)
      {@double_heredoc |> line(string) |> concat(@double_heredoc), state}
    else
      string = escape_string(string, @double_quote)
      {@double_quote |> concat(string) |> concat(@double_quote), state}
    end
  end

  defp quoted_to_algebra({:__block__, _, [atom]}, :argument, state)
       when is_atom(atom) do
    {atom_to_algebra(atom), state}
  end

  defp quoted_to_algebra({:__block__, meta, [integer]}, :argument, state)
       when is_integer(integer) do
    {integer_to_algebra(Keyword.fetch!(meta, :original)), state}
  end

  defp quoted_to_algebra({:__block__, meta, [float]}, :argument, state)
       when is_float(float) do
    {float_to_algebra(Keyword.fetch!(meta, :original)), state}
  end

  defp quoted_to_algebra({:__block__, _meta, [{:unquote_splicing, fun, [_] = args}]},
                         :argument, state) do
    {doc, state} = local_to_algebra(:unquote_splicing, fun, args, state)
    {concat(concat("(", nest(doc, 1)), ")"), state}
  end

  defp quoted_to_algebra({:__block__, _meta, [arg]}, :argument, state) do
    quoted_to_algebra(arg, :argument, state)
  end

  defp quoted_to_algebra({:__aliases__, _meta, [head | tail]}, :argument, state) do
    {doc, state} =
      if is_atom(head) do
        {Atom.to_string(head), state}
      else
        quoted_to_algebra(head, :argument, state)
      end

    doc = wrap_in_parens_if_necessary(head, doc)
    {Enum.reduce(tail, doc, &concat(&2, "." <> Atom.to_string(&1))), state}
  end

  defp quoted_to_algebra({var, _meta, context}, :argument, state)
       when is_atom(context) do
    {Atom.to_string(var), state}
  end

  defp quoted_to_algebra({fun, meta, args}, :argument, state)
       when is_atom(fun) and is_list(args) do
    with :error <- maybe_sigil_to_algebra(fun, meta, args, state),
         :error <- maybe_unary_op_to_algebra(fun, meta, args, state),
         :error <- maybe_binary_op_to_algebra(fun, meta, args, state),
         do: local_to_algebra(fun, meta, args, state)
  end

  ## Operators

  # TODO: Handle @
  # TODO: Handle &
  # TODO: Handle in and not in

  defp maybe_unary_op_to_algebra(fun, _meta, args, state) do
    with [arg] <- args,
         {_, _} <- Code.Identifier.unary_op(fun) do
      {doc, state} = quoted_to_algebra(arg, :argument, state)

      # not and ! are nestable, all others are not.
      wrapped_doc =
        case arg do
          {nestable, _, [_]} when fun == nestable and nestable in [:!, :not] -> doc
          _ -> wrap_in_parens_if_necessary(arg, doc)
        end

      # not requires a space unless the doc was wrapped.
      wrapped_op =
        if fun == :not and wrapped_doc == doc do
          "not "
        else
          Atom.to_string(fun)
        end

      {concat(wrapped_op, nest_by_length(wrapped_doc, wrapped_op)), state}
    else
      _ -> :error
    end
  end

  defp maybe_binary_op_to_algebra(fun, _meta, args, state) do
    with [left, right] <- args,
         {_, _} <- Code.Identifier.binary_op(fun) do
      {doc, state} = binary_op_to_algebra(fun, left, right, state, nil, 2)
      {group(doc, :flex), state}
    else
      _ -> :error
    end
  end

  # There are five kinds of operators.
  #
  #   1. no space binary operators, e.g. 1..2
  #   2. no newline binary operators, e.g. left in right
  #   3. strict newlines before a left precedent operator, e.g. foo |> bar |> baz
  #   4. strict newlines before a right precedent operator, e.g. foo when bar when baz
  #   5. flex newlines after the operator, e.g. foo ++ bar ++ baz
  #
  # Cases 1, 2 and 5 are handled fairly easily by relying on the
  # operator precedence and making sure nesting is applied only once.
  #
  # Cases 3 and 4 are the complex ones, as it requires passing the
  # strict or flex mode around.

  defp binary_op_to_algebra(op, left_quoted, right_quoted, state, parent_info, nesting) do
    op_info = Code.Identifier.binary_op(op)
    {left, state} = binary_operand_to_algebra(left_quoted, state, op, op_info, :left, 2)
    {right, state} = binary_operand_to_algebra(right_quoted, state, op, op_info, :right, 0)

    doc =
      cond do
        op in @no_space_binary_operators ->
          op_string = Atom.to_string(op)
          concat(concat(left, op_string), right)
        op in @no_newline_binary_operators ->
          op_string = " " <> Atom.to_string(op) <> " "
          concat(concat(left, op_string), right)
        op in @left_new_line_before_binary_operators ->
          op_string = Atom.to_string(op) <> " "
          doc = concat(glue(left, op_string), nest_by_length(right, op_string))
          if op_info == parent_info, do: doc, else: group(doc, :strict)
        op in @right_new_line_before_binary_operators ->
          op_string = Atom.to_string(op) <> " "

          # If the parent is of the same type (computed via same precedence),
          # we need to nest the left side because of the associativity.
          left =
            if op_info == parent_info do
              nest_by_length(left, op_string)
            else
              left
            end

          # If the right side is of the same type, we do the nesting above
          # on the left side later on.
          right =
            case right_quoted do
              {^op, _, [_, _]} -> right
              _ -> nest_by_length(right, op_string)
            end

          doc = concat(glue(left, op_string), right)
          if op_info == parent_info, do: doc, else: group(doc, :strict)
        true ->
          op_string = " " <> Atom.to_string(op)
          concat(left, nest(glue(op_string, right), nesting))
      end

    {doc, state}
  end

  defp binary_operand_to_algebra(operand, state, parent_op, parent_info, side, nesting) do
    with {op, _, [left, right]} <- operand,
         op_info = Code.Identifier.binary_op(op),
         {_assoc, prec} <- op_info do
      {parent_assoc, parent_prec} = parent_info

      parens? =
        # If the operators have different precedence and the parent
        # requires parens, then we alwaus add parens. Otherwise we
        # rely on the precedence rules.
        cond do
          parent_prec != prec and parent_op in @required_parens_on_binary_operands -> true
          parent_prec < prec -> false
          parent_prec > prec -> true
          parent_assoc == side -> false
          true -> true
        end

      if parens? do
        {operand, state} = binary_op_to_algebra(op, left, right, state, op_info, 2)
        {group(concat(concat("(", nest(group(operand, :flex), 1)), ")"), :strict), state}
      else
        binary_op_to_algebra(op, left, right, state, op_info, nesting)
      end
    else
      _ -> quoted_to_algebra(operand, :argument, state)
    end
  end

  defp op?(quoted) do
    case quoted do
      {op, _, [_, _]} when is_atom(op) ->
        Code.Identifier.binary_op(op) != :error
      {op, _, [_]} when is_atom(op) ->
        Code.Identifier.unary_op(op) != :error
      _ ->
        false
    end
  end

  ## Remote calls

  defp remote_to_algebra(_quoted, _state) do
    raise "not yet implemented"
  end

  ## Local calls

  defp local_to_algebra(fun, _meta, [], state) do
    {Atom.to_string(fun) <> "()", state}
  end

  defp local_to_algebra(fun, _meta, [first_arg | other_args], state) do
    fun = Atom.to_string(fun)
    {args_doc, state} = quoted_to_algebra(first_arg, :argument, state)

    {args_doc, state} =
      Enum.reduce(other_args, {args_doc, state}, fn arg, {doc_acc, state_acc} ->
        {arg_doc, state_acc} = quoted_to_algebra(arg, :argument, state_acc)
        {glue(concat(doc_acc, ","), arg_doc), state_acc}
      end)

    {surround("#{fun}(", nest_by_length(args_doc, fun), ")"), state}
  end

  ## Interpolation

  defp interpolated?(entries) do
    Enum.all?(entries, fn
      {:::, _, [{{:., _, [Kernel, :to_string]}, _, [_]}, {:binary, _, _}]} -> true
      entry when is_binary(entry) -> true
      _ -> false
    end)
  end

  defp interpolation_to_algebra([entry | entries], escape, state, acc, last) when is_binary(entry) do
    acc = concat(acc, escape_string(entry, escape))
    interpolation_to_algebra(entries, escape, state, acc, last)
  end

  defp interpolation_to_algebra([entry | entries], escape, state, acc, last) do
    {:::, _, [{{:., _, [Kernel, :to_string]}, _, [quoted]}, {:binary, _, _}]} = entry
    {doc, state} = quoted_to_algebra(quoted, :argument, state)
    doc = glue(nest(glue("\#{", "", doc), 2), "", "}")
    interpolation_to_algebra(entries, escape, state, concat(acc, doc), last)
  end

  defp interpolation_to_algebra([], _escape, state, acc, last) do
    {group(concat(acc, last), :strict), state}
  end

  ## Sigils

  defp maybe_sigil_to_algebra(fun, meta, args, state) do
    case {Atom.to_string(fun), args} do
      {<<"sigil_", name>>, [{:<<>>, _, entries}, modifiers]} ->
        opening_terminator = List.to_string(Keyword.fetch!(meta, :terminator))
        acc = <<?~, name, opening_terminator::binary>>

        if opening_terminator in [@double_heredoc, @single_heredoc] do
          {doc, state} = interpolation_to_algebra(entries, :none, state, empty(), opening_terminator)
          {line(acc, doc), state}
        else
          closing_terminator = closing_sigil_terminator(opening_terminator)
          {doc, state} = interpolation_to_algebra(entries, closing_terminator, state, acc, closing_terminator)
          {concat(doc, List.to_string(modifiers)), state}
        end
      _ ->
        :error
    end
  end

  defp closing_sigil_terminator("("), do: ")"
  defp closing_sigil_terminator("["), do: "]"
  defp closing_sigil_terminator("{"), do: "}"
  defp closing_sigil_terminator("<"), do: ">"
  defp closing_sigil_terminator(other) when other in ["\"", "'", "|", "/"], do: other

  ## Literals

  defp atom_to_algebra(atom) when atom in [nil, true, false] do
    Atom.to_string(atom)
  end

  defp atom_to_algebra(atom) do
    string = Atom.to_string(atom)

    case Code.Identifier.classify(atom) do
      type when type in [:callable, :not_callable] ->
        IO.iodata_to_binary [?:, string]
      _ ->
        IO.iodata_to_binary [?:, ?", escape_string(string, "\""), ?"]
    end
  end

  defp integer_to_algebra(text) do
    case text do
      [?0, ?x | rest] ->
        "0x" <> String.upcase(List.to_string(rest))
      [?0, base | _rest] = digits when base in [?b, ?o] ->
        List.to_string(digits)
      [?? | _rest] = char ->
        List.to_string(char)
      decimal ->
        List.to_string(insert_underscores(decimal))
    end
  end

  defp float_to_algebra(text) do
    [int_part, decimal_part] = :string.split(text, '.')

    decimal_part =
      decimal_part
      |> List.to_string()
      |> String.downcase()

    List.to_string(insert_underscores(int_part)) <> "." <> decimal_part
  end

  defp insert_underscores(digits) do
    if length(digits) >= 6 do
      digits
      |> Enum.reverse()
      |> Enum.chunk_every(3)
      |> Enum.intersperse('_')
      |> List.flatten()
      |> Enum.reverse()
    else
      digits
    end
  end

  defp escape_string(string, :none) do
    insert_line_breaks(string)
  end

  defp escape_string(string, escape) when is_binary(escape) do
    string
    |> String.replace(escape, "\\" <> escape)
    |> insert_line_breaks()
  end

  defp insert_line_breaks(string) do
    string
    |> String.split("\n")
    |> Enum.reverse()
    |> Enum.reduce(&line/2)
  end

  ## Quoted helpers

  # TODO: We can remove this workaround once we remove
  # ?rearrange_uop from the parser in Elixir v2.0.
  defp wrap_in_parens_if_necessary({:__block__, [], [expr]}, doc) do
    wrap_in_parens_if_necessary(expr, doc)
  end

  # TODO: do/end blocks and the capture operator (except &INT)
  # also require parens.
  defp wrap_in_parens_if_necessary(quoted, doc) do
    if op?(quoted) do
      concat(concat("(", nest(doc, 1)), ")")
    else
      doc
    end
  end

  ## Algebra helpers

  defp nest_by_length(doc, string) do
    nest(doc, String.length(string))
  end
end
