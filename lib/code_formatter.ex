defmodule CodeFormatter do
  import Inspect.Algebra, except: [format: 2], warn: false

  @line_length 98
  @double_quote "\""
  @double_heredoc "\"\"\""
  @single_quote "'"
  @single_heredoc "'''"

  # Operators that do not have space between operands
  @no_space_binary_operators [:..]

  # Operators that start on the next line in case of breaks
  @new_line_before_binary_operators [:|, :when, :|>, :~>>, :<<~, :~>, :<~, :<~>, :<|>]

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
    |> quoted_to_algebra(state())
    |> elem(0)
  end

  defp state do
    %{}
  end

  defp quoted_to_algebra({:<<>>, meta, entries}, state) do
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
                         state) do
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
                         state) do
    if interpolated?(entries) do
      interpolation_to_algebra(entries, @double_quote, state, ":\"", @double_quote)
    else
      remote_to_algebra(quoted, state)
    end
  end

  defp quoted_to_algebra({:__block__, meta, [list]}, state) when is_list(list) do
    cond do
      meta[:format] == :list_heredoc ->
        string = list |> List.to_string |> escape(:none)
        {@single_heredoc |> line(string) |> concat(@single_heredoc), state}
      Enum.all?(list, & &1 in 0x0..0xFFFFFF) ->
        string = list |> List.to_string |> escape(@single_quote)
        {@single_quote |> concat(string) |> concat(@single_quote), state}
      true ->
        raise "not yet implemented"
    end
  end

  defp quoted_to_algebra({:__block__, meta, [string]}, state) when is_binary(string) do
    if meta[:format] == :bin_heredoc do
      string = escape(string, :none)
      {@double_heredoc |> line(string) |> concat(@double_heredoc), state}
    else
      string = escape(string, @double_quote)
      {@double_quote |> concat(string) |> concat(@double_quote), state}
    end
  end

  defp quoted_to_algebra({:__block__, _, [atom]}, state) when is_atom(atom) do
    {atom_to_algebra(atom), state}
  end

  defp quoted_to_algebra({:__block__, meta, [integer]}, state) when is_integer(integer) do
    {integer_to_algebra(Keyword.fetch!(meta, :original)), state}
  end

  defp quoted_to_algebra({:__block__, meta, [float]}, state) when is_float(float) do
    {float_to_algebra(Keyword.fetch!(meta, :original)), state}
  end

  # TODO: Add a test that unquote_splicing is not removed from block.
  defp quoted_to_algebra({:__block__, _meta, [arg]}, state) do
    quoted_to_algebra(arg, state)
  end

  defp quoted_to_algebra({:__aliases__, _meta, [head | tail]}, state) do
    {doc, state} =
      if is_atom(head) do
        {Atom.to_string(head), state}
      else
        quoted_to_algebra(head, state)
      end

    doc = wrap_in_parens_if_op(head, doc)
    {Enum.reduce(tail, doc, &concat(&2, "." <> Atom.to_string(&1))), state}
  end

  defp quoted_to_algebra({var, _meta, context}, state) when is_atom(context) do
    {Atom.to_string(var), state}
  end

  defp quoted_to_algebra({fun, meta, args}, state) when is_atom(fun) and is_list(args) do
    with :error <- maybe_sigil_to_algebra(fun, meta, args, state),
         :error <- maybe_unary_op_to_algebra(fun, meta, args, state),
         :error <- maybe_binary_op_to_algebra(fun, meta, args, state),
         do: local_to_algebra(fun, meta, args, state)
  end

  ## Operators

  # TODO: Handle @
  # TODO: Handle &
  # TODO: Handle in and not in
  # TODO: Handle operators that spawn a newline
  # TODO: Test unary operators properly handle local calls (and how that affects parens usage)

  # TODO: We can remove this workaround once we remove
  # ?rearrange_uop from the parser in Elixir v2.0.
  defp wrap_in_parens_if_op({:__block__, [], [expr]}, doc) do
    wrap_in_parens_if_op(expr, doc)
  end

  # TODO: We will likely need to generalize this to
  # wrap_in_parens_if_necessary because do/end blocks and
  # the capture operator (except &INT) also require parens.
  defp wrap_in_parens_if_op(quoted, doc) do
    if op?(quoted) do
      surround("(", doc, ")", group: :strict)
    else
      doc
    end
  end

  defp maybe_unary_op_to_algebra(fun, _meta, args, state) do
    with [arg] <- args,
         {_, _} <- Code.Identifier.unary_op(fun) do
      {doc, state} = quoted_to_algebra(arg, state)

      # not and ! are nestable, all others are not.
      wrapped_doc =
        case arg do
          {nestable, _, [_]} when fun == nestable and nestable in [:!, :not] -> doc
          _ -> wrap_in_parens_if_op(arg, doc)
        end

      # not requires a space unless the doc was wrapped.
      wrapped_op =
        if fun == :not and wrapped_doc == doc do
          "not "
        else
          Atom.to_string(fun)
        end

      {concat(wrapped_op, wrapped_doc), state}
    else
      _ -> :error
    end
  end

  defp maybe_binary_op_to_algebra(fun, _meta, args, state) do
    with [left, right] <- args,
         {_, _} <- Code.Identifier.binary_op(fun) do
      binary_op_to_algebra(fun, left, right, state, 2)
    else
      _ -> :error
    end
  end

  defp binary_op_to_algebra(fun, left, right, state, _nesting) do
    {left, state} = binary_operand_to_algebra(left, state, fun, :left)
    {right, state} = binary_operand_to_algebra(right, state, fun, :right)
    op = Atom.to_string(fun)

    cond do
      fun in @no_space_binary_operators ->
        {concat(concat(left, op), right), state}
      fun in @new_line_before_binary_operators ->
        {concat(glue(left, " ", op <> " "), right), state}
      true ->
        {glue(concat(left, " " <> op), " ", right), state}
    end
  end

  defp binary_operand_to_algebra(operand, state, parent_op, side) do
    with {op, _, [left, right]} <- operand,
         {_, prec} <- Code.Identifier.binary_op(op) do
      {parent_assoc, parent_prec} = Code.Identifier.binary_op(parent_op)
      operand = binary_op_to_algebra(op, left, right, state, 0)

      if parent_prec > prec or
           parent_assoc != side or
           parent_op in @required_parens_on_binary_operands do
        concat(concat("(", nest(operand, 1)), ")")
      else
        operand
      end
    else
      _ -> quoted_to_algebra(operand, state)
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

  defp local_to_algebra(fun, _meta, args, state) do
    {args_docs, state} = Enum.map_reduce(args, state, &quoted_to_algebra/2)
    inspect_opts = %Inspect.Opts{limit: :infinity}
    fun = Atom.to_string(fun)
    doc = surround_many("#{fun}(", args_docs, ")", inspect_opts, fn arg, _opts -> arg end, group: :flex)
    {nest(doc, String.length(fun)), state}
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
    acc = concat(acc, escape(entry, escape))
    interpolation_to_algebra(entries, escape, state, acc, last)
  end

  defp interpolation_to_algebra([entry | entries], escape, state, acc, last) do
    {:::, _, [{{:., _, [Kernel, :to_string]}, _, [quoted]}, {:binary, _, _}]} = entry
    {doc, state} = quoted_to_algebra(quoted, state)
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
        IO.iodata_to_binary [?:, ?", escape(string, "\""), ?"]
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

  defp escape(string, :none) do
    insert_line_breaks(string)
  end

  defp escape(string, escape) when is_binary(escape) do
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
end
