defmodule CodeFormatter do
  import Inspect.Algebra, except: [format: 2, surround: 3, surround: 4]

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
  @right_new_line_before_binary_operators [:|]

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
    |> quoted_to_algebra(:block, state())
    |> elem(0)
  end

  defp state do
    %{}
  end

  defp quoted_to_algebra({:<<>>, meta, entries}, _context, state) do
    cond do
      entries == [] ->
        {"<<>>", state}
      not interpolated?(entries) ->
        bitstring_to_algebra(entries, state)
      meta[:format] == :bin_heredoc ->
        interpolation_to_algebra(entries, :none, state, concat(@double_heredoc, line()), @double_heredoc)
      true ->
        interpolation_to_algebra(entries, @double_quote, state, @double_quote, @double_quote)
    end
  end

  defp quoted_to_algebra({{:., _, [String, :to_charlist]}, _, [{:<<>>, meta, entries}]} = quoted,
                         _context, state) do
    cond do
      not interpolated?(entries) ->
        remote_to_algebra(quoted, state)
      meta[:format] == :list_heredoc ->
        interpolation_to_algebra(entries, :none, state, concat(@single_heredoc, line()), @single_heredoc)
      true ->
        interpolation_to_algebra(entries, @single_quote, state, @single_quote, @single_quote)
    end
  end

  defp quoted_to_algebra({{:., _, [:erlang, :binary_to_atom]}, _,
                          [{:<<>>, _, entries}, :utf8]} = quoted,
                         _context, state) do
    if interpolated?(entries) do
      interpolation_to_algebra(entries, @double_quote, state, ":\"", @double_quote)
    else
      remote_to_algebra(quoted, state)
    end
  end

  defp quoted_to_algebra({{:., _, [_, _]}, _, _} = quoted, _context, state) do
    remote_to_algebra(quoted, state)
  end

  defp quoted_to_algebra({{:., _, [_]}, _, _} = quoted, _context, state) do
    remote_to_algebra(quoted, state)
  end

  defp quoted_to_algebra({:{}, _, args}, _context, state) do
    tuple_to_algebra(args, state)
  end

  defp quoted_to_algebra({:__block__, _, [{left, right}]}, _context, state) do
    tuple_to_algebra([left, right], state)
  end

  defp quoted_to_algebra({:__block__, meta, [list]}, _context, state)
       when is_list(list) do
    case meta[:format] do
      :list_heredoc ->
        string = list |> List.to_string |> escape_string(:none)
        {@single_heredoc |> line(string) |> concat(@single_heredoc), state}
      :charlist ->
        string = list |> List.to_string |> escape_string(@single_quote)
        {@single_quote |> concat(string) |> concat(@single_quote), state}
      _other ->
        list_to_algebra(list, state)
    end
  end

  defp quoted_to_algebra({:__block__, meta, [string]}, _context, state)
       when is_binary(string) do
    if meta[:format] == :bin_heredoc do
      string = escape_string(string, :none)
      {@double_heredoc |> line(string) |> concat(@double_heredoc), state}
    else
      string = escape_string(string, @double_quote)
      {@double_quote |> concat(string) |> concat(@double_quote), state}
    end
  end

  defp quoted_to_algebra({:__block__, _, [atom]}, _context, state)
       when is_atom(atom) do
    {atom_to_algebra(atom), state}
  end

  defp quoted_to_algebra({:__block__, meta, [integer]}, _context, state)
       when is_integer(integer) do
    {integer_to_algebra(Keyword.fetch!(meta, :original)), state}
  end

  defp quoted_to_algebra({:__block__, meta, [float]}, _context, state)
       when is_float(float) do
    {float_to_algebra(Keyword.fetch!(meta, :original)), state}
  end

  defp quoted_to_algebra({:__block__, _meta, [{:unquote_splicing, _, [_] = args}]},
                         _context, state) do
    {doc, state} = local_to_algebra(:unquote_splicing, args, state)
    {concat(concat("(", nest(doc, 1)), ")"), state}
  end

  defp quoted_to_algebra({:__block__, _meta, [arg]}, _context, state) do
    quoted_to_algebra(arg, :argument, state)
  end

  defp quoted_to_algebra({:__block__, _, []}, _context, state) do
    {"", state}
  end

  defp quoted_to_algebra({:__aliases__, _meta, [head | tail]}, _context, state) do
    {doc, state} =
      if is_atom(head) do
        {Atom.to_string(head), state}
      else
        quoted_to_algebra_with_parens_if_necessary(head, :argument, state)
      end

    {Enum.reduce(tail, doc, &concat(&2, "." <> Atom.to_string(&1))), state}
  end

  defp quoted_to_algebra({var, _meta, var_context}, _context, state)
       when is_atom(var_context) do
    {Atom.to_string(var), state}
  end

  # &1
  # &local(&1)
  # &local/1
  # &Mod.remote/1
  # & &1
  # & &1 + &2
  defp quoted_to_algebra({:&, _, [arg]}, _context, state) do
    capture_to_algebra(arg, state)
  end

  defp quoted_to_algebra({:@, _, [arg]}, context, state) do
    module_attribute_to_algebra(arg, context, state)
  end

  # not(left in right)
  # left not in right
  defp quoted_to_algebra({:not, _, [{:in, _, [left, right]}]}, _context, state) do
    binary_op_to_algebra(:in, "not in", left, right, state, nil, 2)
  end

  defp quoted_to_algebra({:fn, _, [_ | _] = clauses}, _context, state) do
    anon_fun_to_algebra(clauses, state)
  end

  defp quoted_to_algebra({fun, meta, args}, _context, state)
       when is_atom(fun) and is_list(args) do
    with :error <- maybe_sigil_to_algebra(fun, meta, args, state),
         :error <- maybe_unary_op_to_algebra(fun, args, state),
         :error <- maybe_binary_op_to_algebra(fun, args, state),
         do: local_to_algebra(fun, args, state)
  end

  ## Operators

  defp maybe_unary_op_to_algebra(fun, args, state) do
    with [arg] <- args,
         {_, _} <- Code.Identifier.unary_op(fun) do
      unary_op_to_algebra(fun, arg, state)
    else
      _ -> :error
    end
  end

  defp unary_op_to_algebra(op, arg, state) do
    {doc, state} = quoted_to_algebra(arg, :argument, state)

    # not and ! are nestable, all others are not.
    wrapped_doc =
      case arg do
        {^op, _, [_]} when op in [:!, :not] -> doc
        _ -> wrap_in_parens_if_necessary(arg, doc)
      end

    # not requires a space unless the doc was wrapped in parens.
    op_string =
      if op == :not and wrapped_doc == doc do
        "not "
      else
        Atom.to_string(op)
      end

    {concat(op_string, wrapped_doc), state}
  end

  defp maybe_binary_op_to_algebra(fun, args, state) do
    with [left, right] <- args,
         {_, _} <- Code.Identifier.binary_op(fun) do
      binary_op_to_algebra(fun, Atom.to_string(fun), left, right, state, nil, 2)
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

  defp binary_op_to_algebra(op, op_string, left_arg, right_arg, state, parent_info, nesting) do
    op_info = Code.Identifier.binary_op(op)
    {left, state} = binary_operand_to_algebra(left_arg, state, op, op_info, :left, 2)
    {right, state} = binary_operand_to_algebra(right_arg, state, op, op_info, :right, 0)

    doc =
      cond do
        op in @no_space_binary_operators ->
          concat(concat(left, op_string), right)
        op in @no_newline_binary_operators ->
          op_string = " " <> op_string <> " "
          concat(concat(left, op_string), right)
        op in @left_new_line_before_binary_operators ->
          op_string = op_string <> " "
          doc = concat(glue(left, op_string), nest_by_length(right, op_string))
          if op_info == parent_info, do: doc, else: group(doc, :strict)
        op in @right_new_line_before_binary_operators ->
          op_string = op_string <> " "

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
            case right_arg do
              {^op, _, [_, _]} -> right
              _ -> nest_by_length(right, op_string)
            end

          doc = concat(glue(left, op_string), right)
          if op_info == parent_info, do: doc, else: group(doc, :strict)
        true ->
          op_string = " " <> op_string
          concat(left, nest(glue(op_string, group(right, :strict)), nesting))
      end

    {doc, state}
  end

  # TODO: We can remove this workaround once we remove
  # ?rearrange_uop from the parser in Elixir v2.0.
  # (! left) in right
  # (not left) in right
  defp binary_operand_to_algebra({:__block__, _, [{op, _, [arg]}]}, state, :in,
                                 _parent_info, :left, _nesting) when op in [:not, :!] do
    {doc, state} = unary_op_to_algebra(op, arg, state)
    {concat(concat("(", nest(doc, 1)), ")"), state}
  end

  defp binary_operand_to_algebra(operand, state, parent_op, parent_info, side, nesting) do
    with {op, _, [left, right]} <- operand,
         op_info = Code.Identifier.binary_op(op),
         {_assoc, prec} <- op_info do
      {parent_assoc, parent_prec} = parent_info
      op_string = Atom.to_string(op)

      cond do
        # If the operator has the same precedence as the parent and is on
        # the correct side, we respect the nesting rule to avoid multiple
        # nestings.
        parent_prec == prec and parent_assoc == side ->
          binary_op_to_algebra(op, op_string, left, right, state, op_info, nesting)

        # If the parent requires parens or the precedence is inverted or
        # it is in the wrong side, then we *need* parenthesis.
        parent_op in @required_parens_on_binary_operands or
            parent_prec > prec or
            parent_prec == prec and parent_assoc != side ->
          {operand, state} = binary_op_to_algebra(op, op_string, left, right, state, op_info, 2)
          {concat(concat("(", nest(operand, 1)), ")"), state}

        # Otherwise, we rely on precedence but also nest.
        true ->
          binary_op_to_algebra(op, op_string, left, right, state, op_info, 2)
      end
    else
      _ -> quoted_to_algebra(operand, :argument, state)
    end
  end

  ## Module attributes

  # @Foo
  # @Foo.Bar
  defp module_attribute_to_algebra({:__aliases__, _, [_, _ | _]} = quoted, _context, state) do
    {doc, state} = quoted_to_algebra(quoted, :argument, state)
    {concat(concat("@(", doc), ")"), state}
  end

  # @foo bar
  # @foo(bar)
  defp module_attribute_to_algebra({name, _meta, [value]} = arg, context, state)
       when is_atom(name) and name not in [:__block__, :__aliases__] do
    if Code.Identifier.classify(name) == :callable_local do
      attr_doc = "@" <> Atom.to_string(name)
      {value_doc, state} = quoted_to_algebra(value, :argument, state)

      case context do
        :argument ->
          {concat(concat(attr_doc <> "(", value_doc), ")"), state}
        :block ->
          {space(attr_doc, value_doc), state}
      end
    else
      unary_op_to_algebra(:@, arg, state)
    end
  end

  # @foo
  # @(foo.bar())
  defp module_attribute_to_algebra(quoted, _context, state) do
    unary_op_to_algebra(:@, quoted, state)
  end

  ## Capture operator

  defp capture_to_algebra(integer, state) when is_integer(integer) do
    {"&" <> Integer.to_string(integer), state}
  end

  defp capture_to_algebra({:/, _, [{{:., _, [target, name]}, _, []}, {:__block__, _, [arity]}]}, state)
      when is_atom(name) and is_integer(arity) do
    {doc, state} = remote_target_to_algebra(target, state)
    name = Code.Identifier.inspect_as_function(name)
    {"&" |> concat(doc) |> nest(3) |> concat(".#{name}/#{arity}"), state}
  end

  defp capture_to_algebra({:/, _, [{name, _, context}, {:__block__, _, [arity]}]}, state)
       when is_atom(name) and is_atom(context) and is_integer(arity) do
    {"&#{name}/#{arity}", state}
  end

  defp capture_to_algebra(arg, state) do
    {doc, state} = quoted_to_algebra(arg, :argument, state)

    cond do
      binary_operator?(arg) ->
        {concat("& ", doc), state}
      doc |> Inspect.Algebra.format(:infinity) |> IO.iodata_to_binary() |> String.starts_with?("&") ->
        {concat("& ", doc), state}
      true ->
        {concat("&", doc), state}
    end
  end

  ## Remote calls (and anonymous function calls)

  # expression.function(arguments)
  defp remote_to_algebra({{:., _, [target, fun]}, _, args}, state)
       when is_atom(fun) do
    fun_doc = Code.Identifier.inspect_as_function(fun)
    remote_to_algebra(target, fun_doc, args, state, 2)
  end

  defp remote_to_algebra({{:., _, [target]}, _, args}, state) do
    remote_to_algebra(target, empty(), args, state, 2)
  end

  defp remote_to_algebra(target, fun_doc, args, state, nesting) do
    {target_doc, state} = remote_target_to_algebra(target, state)
    {args_doc, state} = args_to_algebra(args, &quoted_to_algebra(&1, :argument, &2), state)

    call_doc =
      if args == [] do
        concat(fun_doc, "()")
      else
        surround(concat(fun_doc, "("), args_doc, ")")
      end

    doc = nest(glue(concat(target_doc, "."), "", call_doc), nesting)

    {doc, state}
  end

  defp remote_target_to_algebra({{:., _, [target, fun]}, _, args}, state)
       when is_atom(fun) do
    fun_doc = Code.Identifier.inspect_as_function(fun)
    remote_to_algebra(target, fun_doc, args, state, 0)
  end

  defp remote_target_to_algebra({{:., _, [target]}, _, args}, state) do
    remote_to_algebra(target, empty(), args, state, 0)
  end

  defp remote_target_to_algebra(quoted, state) do
    quoted_to_algebra_with_parens_if_necessary(quoted, :argument, state)
  end

  ## Local calls

  defp local_to_algebra(fun, [], state) when is_atom(fun) do
    {"#{fun}()", state}
  end

  defp local_to_algebra(fun, args, state) when is_atom(fun) do
    fun = Atom.to_string(fun)
    {args_doc, state} = args_to_algebra(args, &quoted_to_algebra(&1, :argument, &2), state)
    {surround("#{fun}(", args_doc, ")"), state}
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
    {doc, state} = quoted_to_algebra(quoted, :block, state)
    doc = group(glue(nest(glue("\#{", "", doc), 2), "", "}"), :strict)
    interpolation_to_algebra(entries, escape, state, concat(acc, doc), last)
  end

  defp interpolation_to_algebra([], _escape, state, acc, last) do
    {concat(acc, last), state}
  end

  ## Sigils

  defp maybe_sigil_to_algebra(fun, meta, args, state) do
    case {Atom.to_string(fun), args} do
      {<<"sigil_", name>>, [{:<<>>, _, entries}, modifiers]} ->
        opening_terminator = List.to_string(Keyword.fetch!(meta, :terminator))
        acc = <<?~, name, opening_terminator::binary>>

        if opening_terminator in [@double_heredoc, @single_heredoc] do
          interpolation_to_algebra(entries, :none, state, concat(acc, line()), opening_terminator)
        else
          closing_terminator = closing_sigil_terminator(opening_terminator)
          interpolation_to_algebra(
            entries,
            closing_terminator,
            state,
            acc,
            concat(closing_terminator, List.to_string(modifiers))
          )
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

  ## Bitstrings

  defp bitstring_to_algebra(args, state) do
    last = length(args) - 1
    {args_doc, state} =
      args
      |> Enum.with_index()
      |> args_to_algebra(&bitstring_to_algebra(&1, &2, last), state)
    {surround(args, "<<", args_doc, ">>", 2), state}
  end

  defp bitstring_to_algebra({{:::, _, [segment, spec]}, i}, state, _last) do
    {doc, state} = quoted_to_algebra(segment, :argument, state)
    doc = bitstring_wrap_parens(segment, doc, i == 0)

    {spec, state} = bitstring_spec_to_algebra(spec, state)
    doc = concat(concat(doc, "::"), wrap_in_parens_if_inspected_atom(spec))
    {doc, state}
  end

  defp bitstring_to_algebra({segment, i}, state, last) do
    {doc, state} = quoted_to_algebra(segment, :argument, state)
    {bitstring_wrap_parens(segment, doc, i == 0 or i == last), state}
  end

  defp bitstring_spec_to_algebra({:-, _, [left, right]}, state) do
    {left, state} = bitstring_spec_to_algebra(left, state)
    {right, state} = quoted_to_algebra_with_parens_if_necessary(right, :argument, state)
    {concat(concat(left, "-"), right), state}
  end

  defp bitstring_spec_to_algebra(spec, state) do
    quoted_to_algebra_with_parens_if_necessary(spec, :argument, state)
  end

  defp bitstring_wrap_parens({:<<>>, _, _}, doc, true) do
    concat(concat("(", doc), ")")
  end

  defp bitstring_wrap_parens(_, doc, _) do
    doc
  end

  ## Literals

  defp tuple_to_algebra([], state) do
    {"{}", state}
  end

  defp tuple_to_algebra(args, state) do
    {args_doc, state} = args_to_algebra(args, &quoted_to_algebra(&1, :argument, &2), state)
    {surround(args, "{", args_doc, "}", 1), state}
  end

  defp atom_to_algebra(atom) when atom in [nil, true, false] do
    Atom.to_string(atom)
  end

  defp atom_to_algebra(atom) do
    string = Atom.to_string(atom)

    case Code.Identifier.classify(atom) do
      type when type in [:callable_local, :callable_operator, :not_callable] ->
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

  ## Lists

  defp list_to_algebra(args, state) do
    {args_doc, state} = args_to_algebra(args, &quoted_to_algebra(&1, :argument, &2), state)
    {surround(args, "[", args_doc, "]", 1), state}
  end

  ## Anonymous functions

  # fn -> block end
  # fn args -> block end
  defp anon_fun_to_algebra([{:"->", _, [args, body]}], state) do
    {args_doc, state} = args_to_algebra(args, &quoted_to_algebra(&1, :argument, &2), state)
    {body_doc, state} = quoted_to_algebra(body, :block, state)
    head_doc =
      case args do
        [] ->
          "fn ->"
        _other ->
          "fn" |> space(args_doc) |> space("->")
      end
    doc = group(glue(nest(glue(head_doc, " ", body_doc), 2), " ", "end"), :strict)
    {doc, state}
  end

  # fn
  #   args1 ->
  #     block1
  #   args2 ->
  #     block2
  # end
  defp anon_fun_to_algebra(clauses, state) do
    {clauses_docs, state} = clauses_to_algebra(clauses, state)
    {line(nest(line("fn", clauses_docs), 2), "end"), state}
  end

  defp clauses_to_algebra([clause | [_ | _] = clauses], state) do
    {clause_doc, state} = split_clause_to_algebra(clause, state)

    Enum.reduce(clauses, {clause_doc, state}, fn clause, {doc_acc, state_acc} ->
      {clause_doc, state_acc} = split_clause_to_algebra(clause, state_acc)
      {line(doc_acc, clause_doc), state_acc}
    end)
  end

  defp split_clause_to_algebra({:"->", _, [[_ | _] = args, body]}, state) do
    {args_doc, state} = args_to_algebra(args, &quoted_to_algebra(&1, :argument, &2), state)
    {body_doc, state} = quoted_to_algebra(body, :block, state)
    {args_doc |> space("->") |> line(body_doc) |> nest(2), state}
  end

  ## Quoted helpers

  defp quoted_to_algebra_with_parens_if_necessary(ast, context, state) do
    {doc, state} = quoted_to_algebra(ast, context, state)
    {wrap_in_parens_if_necessary(ast, doc), state}
  end

  # TODO: We can remove this workaround once we remove
  # ?rearrange_uop from the parser in Elixir v2.0.
  defp wrap_in_parens_if_necessary({:__block__, [], [expr]}, doc) do
    wrap_in_parens_if_necessary(expr, doc)
  end

  defp wrap_in_parens_if_necessary(quoted, doc) do
    if operator?(quoted) and not module_attribute_read?(quoted) and not integer_capture?(quoted) do
      concat(concat("(", nest(doc, 1)), ")")
    else
      doc
    end
  end

  defp wrap_in_parens_if_inspected_atom(":" <> _ = doc) do
    "(" <> doc <> ")"
  end

  defp wrap_in_parens_if_inspected_atom(doc) do
    doc
  end

  defp args_to_algebra([], _fun, state) do
    {empty(), state}
  end

  defp args_to_algebra([arg | args], fun, state) do
    Enum.reduce(args, fun.(arg, state), fn arg, {doc_acc, state_acc} ->
      {arg_doc, state_acc} = fun.(arg, state_acc)
      {glue(concat(doc_acc, ","), arg_doc), state_acc}
    end)
  end

  defp module_attribute_read?({:@, _, [{var, _, var_context}]})
       when is_atom(var) and is_atom(var_context) do
    Code.Identifier.classify(var) == :callable_local
  end
  defp module_attribute_read?(_), do: false

  defp integer_capture?({:&, _, [integer]}) when is_integer(integer), do: true
  defp integer_capture?(_), do: false

  defp binary_operator?(quoted) do
    case quoted do
      {op, _, [_, _]} when is_atom(op) ->
        Code.Identifier.binary_op(op) != :error
      _ ->
        false
    end
  end

  defp unary_operator?(quoted) do
    case quoted do
      {op, _, [_]} when is_atom(op) ->
        Code.Identifier.unary_op(op) != :error
      _ ->
        false
    end
  end

  defp operator?(quoted) do
    unary_operator?(quoted) or binary_operator?(quoted)
  end

  ## Algebra helpers

  defp surround(left, doc, right) do
    group(glue(nest(glue(left, "", doc), 2), "", right), :strict)
  end

  # TODO: Perform simple check for all data structures
  # (calls not included).
  defp surround(_args, left, doc, right, _flex_nesting) do
    surround(left, doc, right)
  end

  defp nest_by_length(doc, string) do
    nest(doc, String.length(string))
  end
end
