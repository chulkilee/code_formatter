defmodule CodeFormatter do
  import Inspect.Algebra, except: [format: 2, surround: 3, surround: 4]

  @line_length 98
  @double_quote "\""
  @double_heredoc "\"\"\""
  @single_quote "'"
  @single_heredoc "'''"
  @keywords [:do, :rescue, :catch, :else, :after]

  # Operators that do not have space between operands
  @no_space_binary_operators [:..]

  # Operators that do not have newline between operands
  @no_newline_binary_operators [:\\, :in]

  # Operators that start on the next line in case of breaks
  @left_new_line_before_binary_operators [:|>, :~>>, :<<~, :~>, :<~, :<~>, :<|>]

  # Operators that always require parens on operands when they are the parent
  @required_parens_on_binary_operands [:|>, :<<<, :>>>, :<~, :~>, :<<~, :~>>, :<~>, :<|>,
                                       :^^^, :in, :++, :--, :.., :<>]

  @locals_without_parens [
    # Special forms
    alias: 1,
    alias: 2,
    import: 1,
    import: 2,
    with: :*,
    for: :*,

    # Kernel
    def: 1,
    def: 2,
    defp: 1,
    defp: 2,
    defmacro: 1,
    defmacro: 2,
    defmacrop: 1,
    defmacrop: 2,
    defdelegate: 1,
    defexception: 1,
    defoverridable: 1,
    defstruct: 1,
    raise: 1,
    raise: 2,
    if: 2,
    unless: 2,
    use: 1,
    use: 2,

    # Testing
    all: :*,
    assert: 1,
    assert: 2
  ]

  @doc """
  Checks if two strings are equivalent.
  """
  def equivalent(string1, string2) when is_binary(string1) and is_binary(string2) do
    quoted1 = Code.string_to_quoted!(string1)
    quoted2 = Code.string_to_quoted!(string2)
    case not_equivalent(quoted1, quoted2) do
      {left, right} -> {:error, left, right}
      nil -> :ok
    end
  end

  # TODO: We can remove this workaround once we remove
  # ?rearrange_uop from the parser in Elixir v2.0.
  defp not_equivalent({:__block__, _, left}, {:__block__, _, right}) do
    not_equivalent(left, right)
  end

  defp not_equivalent({:__block__, _, [left]}, right) do
    not_equivalent(left, right)
  end

  defp not_equivalent(left, {:__block__, _, [right]}) do
    not_equivalent(left, right)
  end

  defp not_equivalent({:__block__, _, []}, nil) do
    nil
  end

  defp not_equivalent(nil, {:__block__, _, []}) do
    nil
  end

  defp not_equivalent([left | lefties], [right | righties]) do
    not_equivalent(left, right) || not_equivalent(lefties, righties)
  end

  defp not_equivalent({left_name, _, left_args}, {right_name, _, right_args}) do
    not_equivalent(left_name, right_name) || not_equivalent(left_args, right_args)
  end

  defp not_equivalent({left1, left2}, {right1, right2}) do
    not_equivalent(left1, right1) || not_equivalent(left2, right2)
  end

  defp not_equivalent(side, side) do
    nil
  end

  defp not_equivalent(left, right) do
    {left, right}
  end

  @doc """
  Formats the given code `string`.
  """
  def format!(string, opts \\ []) when is_binary(string) and is_list(opts) do
    line_length = Keyword.get(opts, :line_length, @line_length)

    string
    |> to_algebra!(opts)
    |> Inspect.Algebra.format(line_length)
  end

  @doc """
  Converts `string` to an algebra document.
  """
  def to_algebra!(string, opts \\ []) when is_binary(string) and is_list(opts) do
    string
    |> Code.string_to_quoted!(wrap_literals_in_blocks: true, unescape: false)
    |> block_to_algebra(state(opts))
    |> elem(0)
  end

  defp state(_opts) do
    %{locals_without_parens: @locals_without_parens,
      operand_nesting: 2}
  end

  # Special AST nodes from compiler feedback.

  defp quoted_to_algebra({:special, :arguments, []}, _context, state) do
    {"()", state}
  end

  defp quoted_to_algebra({:special, :arguments, args}, context, state) do
    {doc, state} = args_to_algebra(args, state, &quoted_to_algebra(&1, context, &2))
    {group(doc), state}
  end

  defp quoted_to_algebra({var, _meta, var_context}, _context, state) when is_atom(var_context) do
    {var |> Atom.to_string() |> string(), state}
  end

  defp quoted_to_algebra({:<<>>, meta, entries}, _context, state) do
    cond do
      entries == [] ->
        {"<<>>", state}
      not interpolated?(entries) ->
        bitstring_to_algebra(entries, state)
      meta[:format] == :bin_heredoc ->
        initial = @double_heredoc |> concat(line()) |> force_break()
        interpolation_to_algebra(entries, :heredoc, state, initial, @double_heredoc)
      true ->
        interpolation_to_algebra(entries, @double_quote, state, @double_quote, @double_quote)
    end
  end

  defp quoted_to_algebra({{:., _, [String, :to_charlist]}, _, [{:<<>>, meta, entries}]} = quoted,
                         context, state) do
    cond do
      not interpolated?(entries) ->
        remote_to_algebra(quoted, context, state)
      meta[:format] == :list_heredoc ->
        initial = @single_heredoc |> concat(line()) |> force_break()
        interpolation_to_algebra(entries, :heredoc, state, initial, @single_heredoc)
      true ->
        interpolation_to_algebra(entries, @single_quote, state, @single_quote, @single_quote)
    end
  end

  defp quoted_to_algebra({{:., _, [:erlang, :binary_to_atom]}, _,
                          [{:<<>>, _, entries}, :utf8]} = quoted,
                         context, state) do
    if interpolated?(entries) do
      interpolation_to_algebra(entries, @double_quote, state, ":\"", @double_quote)
    else
      remote_to_algebra(quoted, context, state)
    end
  end

  # foo[bar]
  # TODO: Remove Access in favor of our own special form.
  defp quoted_to_algebra({{:., _, [Access, :get]}, _, [target | args]}, _context, state) do
    {target_doc, state} = remote_target_to_algebra(target, state)
    {call_doc, state} = list_to_algebra(args, state)
    {concat(target_doc, call_doc), state}
  end

  # %Foo{}
  # %name{foo: 1}
  # %name{bar | foo: 1}
  defp quoted_to_algebra({:%, _, [name, {:%{}, _, args}]}, _context, state) do
    {name_doc, state} = quoted_to_algebra(name, :argument, state)
    map_to_algebra(name_doc, args, state)
  end

  # %{foo: 1}
  # %{foo => bar}
  # %{name | foo => bar}
  defp quoted_to_algebra({:%{}, _, args}, _context, state) do
    map_to_algebra(empty(), args, state)
  end

  # {}
  # {1, 2}
  defp quoted_to_algebra({:{}, _, args}, _context, state) do
    tuple_to_algebra(args, state)
  end

  defp quoted_to_algebra({:__block__, _, [{left, right}]}, _context, state) do
    tuple_to_algebra([left, right], state)
  end

  defp quoted_to_algebra({:__block__, meta, [list]}, _context, state) when is_list(list) do
    case meta[:format] do
      :list_heredoc ->
        string = list |> List.to_string |> escape_string(:heredoc)
        {@single_heredoc |> line(string) |> concat(@single_heredoc) |> force_break(), state}
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
      string = escape_string(string, :heredoc)
      {@double_heredoc |> line(string) |> concat(@double_heredoc) |> force_break(), state}
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
                         context, state) do
    {doc, state} = local_to_algebra(:unquote_splicing, args, context, state)
    {concat(concat("(", nest(doc, 1)), ")"), state}
  end

  defp quoted_to_algebra({:__block__, _meta, [arg]}, context, state) do
    quoted_to_algebra(arg, context, state)
  end

  defp quoted_to_algebra({:__block__, _, []}, _context, state) do
    {"", state}
  end

  defp quoted_to_algebra({:__block__, _, _} = block, _context, state) do
    {block, state} = block_to_algebra(block, state)
    {wrap_in_parens(block), state}
  end

  defp quoted_to_algebra({:__aliases__, meta, [head | tail]}, context, state) do
    if meta[:format] == :alias do
      {doc, state} =
        if is_atom(head) do
          {Atom.to_string(head), state}
        else
          quoted_to_algebra_with_parens_if_necessary(head, context, state)
        end
      {Enum.reduce(tail, doc, &concat(&2, "." <> Atom.to_string(&1))), state}
    else
      local_to_algebra(:__aliases__, [head | tail], context, state)
    end
  end

  # &1
  # &local(&1)
  # &local/1
  # &Mod.remote/1
  # & &1
  # & &1 + &2
  defp quoted_to_algebra({:&, _, [arg]}, context, state) do
    capture_to_algebra(arg, context, state)
  end

  defp quoted_to_algebra({:@, _, [arg]}, context, state) do
    module_attribute_to_algebra(arg, context, state)
  end

  # not(left in right)
  # left not in right
  defp quoted_to_algebra({:not, _, [{:in, _, [left, right]}]}, context, state) do
    binary_op_to_algebra(:in, "not in", left, right, context, state)
  end

  defp quoted_to_algebra({:fn, _, [_ | _] = clauses}, _context, state) do
    anon_fun_to_algebra(clauses, state)
  end

  defp quoted_to_algebra({fun, meta, args}, context, state)
       when is_atom(fun) and is_list(args) do
    with :error <- maybe_sigil_to_algebra(fun, meta, args, state),
         :error <- maybe_unary_op_to_algebra(fun, args, context, state),
         :error <- maybe_binary_op_to_algebra(fun, args, context, state),
         do: local_to_algebra(fun, args, context, state)
  end

  defp quoted_to_algebra({_, _, args} = quoted, context, state) when is_list(args) do
    remote_to_algebra(quoted, context, state)
  end

  # (left -> right)
  defp quoted_to_algebra([{:"->", _, _} | _] = clauses, _context, state) do
    type_fun_to_algebra(clauses, state)
  end

  # [keyword: :list] (inner part)
  # %{:foo => :bar} (inner part)
  defp quoted_to_algebra(list, context, state) when is_list(list) do
    args_to_algebra(list, state, &quoted_to_algebra(&1, context, &2))
  end

  # keyword: :list
  # key => value
  defp quoted_to_algebra({left, right}, context, state) do
    if keyword_key?(left) do
      {left, state} =
        case left do
          {:__block__, _, [atom]} when is_atom(atom) ->
            {atom |> Code.Identifier.inspect_as_key() |> string(), state}
          {{:., _, [:erlang, :binary_to_atom]}, _, [{:<<>>, _, entries}, :utf8]} ->
            interpolation_to_algebra(entries, @double_quote, state, "\"", "\": ")
        end
      {right, state} = quoted_to_algebra(right, context, state)
      {concat(left, right), state}
    else
      {left, state} = quoted_to_algebra(left, context, state)
      {right, state} = quoted_to_algebra(right, context, state)
      {left |> concat(" => ") |> concat(right), state}
    end
  end

  ## Blocks

  # TODO: Introduce smart spacing based on special forms, @, assignments, etc.
  defp block_to_algebra({:__block__, _, [_, _ | _] = args}, state) do
    {args, last} = split_last(args)

    {args_doc, state} =
      Enum.reduce(args, {[], state}, fn quoted, {acc, state} ->
        {doc, state} = quoted_to_algebra(quoted, :block, state)
        doc = doc |> concat(maybe_empty_line()) |> group()
        {[doc | acc], state}
      end)

    {last_doc, state} = quoted_to_algebra(last, :block, state)
    block_doc = Enum.reduce(args_doc, group(last_doc), &line/2)
    {force_break(block_doc), state}
  end

  defp block_to_algebra(block, state) do
    {doc, state} = quoted_to_algebra(block, :block, state)
    {group(doc), state}
  end

  ## Operators

  defp maybe_unary_op_to_algebra(fun, args, context, state) do
    with [arg] <- args,
         {_, _} <- Code.Identifier.unary_op(fun) do
      unary_op_to_algebra(fun, arg, context, state)
    else
      _ -> :error
    end
  end

  defp unary_op_to_algebra(op, arg, context, state) do
    {doc, state} = quoted_to_algebra(arg, at_least_argument(context), state)

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

  defp maybe_binary_op_to_algebra(fun, args, context, state) do
    with [left, right] <- args,
         {_, _} <- Code.Identifier.binary_op(fun) do
      binary_op_to_algebra(fun, Atom.to_string(fun), left, right, context, state)
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
  defp binary_op_to_algebra(op, op_string, left_arg, right_arg, context, state) do
    %{operand_nesting: nesting} = state
    binary_op_to_algebra(op, op_string, left_arg, right_arg, context, state, nil, nesting)
  end

  defp binary_op_to_algebra(op, op_string, left_arg, right_arg, context, state, parent_info, nesting) do
    op_info = Code.Identifier.binary_op(op)
    op_context = at_least_argument(context)
    {left, state} = binary_operand_to_algebra(left_arg, op_context, state, op, op_info, :left, 2)
    {right, state} = binary_operand_to_algebra(right_arg, op_context, state, op, op_info, :right, 0)

    doc =
      cond do
        op in @no_space_binary_operators ->
          concat(concat(left, op_string), right)
        op in @no_newline_binary_operators ->
          op_string = " " <> op_string <> " "
          concat(concat(left, op_string), right)
        op in @left_new_line_before_binary_operators ->
          op_string = op_string <> " "
          doc = glue(left, concat(op_string, nest_by_length(right, op_string)))
          if op_info == parent_info, do: doc, else: group(doc)
        op == :| and parent_info != nil ->
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

          doc = glue(left, concat(op_string, right))
          if op_info == parent_info, do: doc, else: group(doc)
        true ->
          apply_cancel_break(right_arg, right, fn right ->
            op_string = " " <> op_string
            concat(left, group(nest(glue(op_string, group(right)), nesting, :break)))
          end)
      end

    {doc, state}
  end

  # TODO: We can remove this workaround once we remove
  # ?rearrange_uop from the parser in Elixir v2.0.
  # (! left) in right
  # (not left) in right
  defp binary_operand_to_algebra({:__block__, _, [{op, _, [arg]}]}, context, state, :in,
                                 _parent_info, :left, _nesting) when op in [:not, :!] do
    {doc, state} = unary_op_to_algebra(op, arg, context, state)
    {concat(concat("(", nest(doc, 1)), ")"), state}
  end

  defp binary_operand_to_algebra(operand, context, state, parent_op, parent_info, side, nesting) do
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
          binary_op_to_algebra(op, op_string, left, right, context, state, op_info, nesting)

        # If the parent requires parens or the precedence is inverted or
        # it is in the wrong side, then we *need* parenthesis.
        parent_op in @required_parens_on_binary_operands or
            parent_prec > prec or
            parent_prec == prec and parent_assoc != side ->
          {operand, state} =
            binary_op_to_algebra(op, op_string, left, right, context, state, op_info, 2)

          {concat(concat("(", nest(operand, 1)), ")"), state}

        # Otherwise, we rely on precedence but also nest.
        true ->
          binary_op_to_algebra(op, op_string, left, right, context, state, op_info, 2)
      end
    else
      {:&, _, [arg]} when not is_integer(arg) ->
        {doc, state} = quoted_to_algebra(operand, context, state)
        {wrap_in_parens(doc), state}
      _ ->
        quoted_to_algebra(operand, context, state)
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
      name = Atom.to_string(name)
      {value_doc, state} = quoted_to_algebra(value, :no_parens_argument, state)

      case context do
        :block ->
          attr = "@#{name} "
          {attr |> string |> concat(nest_by_length(value_doc, attr)), state}
        _ ->
          attr = "@#{name}("
          {attr |> string |> concat(value_doc) |> concat(")"), state}
      end
    else
      unary_op_to_algebra(:@, arg, context, state)
    end
  end

  # @foo
  # @(foo.bar())
  defp module_attribute_to_algebra(quoted, context, state) do
    unary_op_to_algebra(:@, quoted, context, state)
  end

  ## Capture operator

  defp capture_to_algebra(integer, _context, state) when is_integer(integer) do
    {"&" <> Integer.to_string(integer), state}
  end

  defp capture_to_algebra(arg, context, state) do
    {{doc, state}, force_space?} = capture_target_to_algebra(arg, context, state)

    if force_space? or (doc |> format_to_string() |> String.starts_with?("&")) do
      {concat("& ", doc), state}
    else
      {concat("&", doc), state}
    end
  end

  defp capture_target_to_algebra({:/, _, [{{:., _, [target, name]}, _, []}, {:__block__, _, [arity]}]}, _context, state)
      when is_atom(name) and is_integer(arity) do
    {doc, state} = remote_target_to_algebra(target, state)
    name = Code.Identifier.inspect_as_function(name)
    {{doc |> nest(1) |> concat(string(".#{name}/#{arity}")), state}, false}
  end

  defp capture_target_to_algebra({:/, _, [{name, _, var_context}, {:__block__, _, [arity]}]}, _context, state)
       when is_atom(name) and is_atom(var_context) and is_integer(arity) do
    {{string("#{name}/#{arity}"), state}, false}
  end

  defp capture_target_to_algebra(arg, context, state) do
    {quoted_to_algebra(arg, context, state), binary_operator?(arg)}
  end


  ## Calls (local, remote and anonymous)

  # expression.{arguments}
  defp remote_to_algebra({{:., _, [target, :{}]}, _, args}, _context, state) do
    {target_doc, state} = remote_target_to_algebra(target, state)
    {call_doc, state} = tuple_to_algebra(args, state)
    {concat(concat(target_doc, "."), call_doc), state}
  end

  # expression.(arguments)
  defp remote_to_algebra({{:., _, [target]}, _, args}, context, state) do
    {target_doc, state} = remote_target_to_algebra(target, state)
    {{call_doc, state}, wrap_in_parens?} = call_args_to_algebra(args, context, :maybe, state)
    doc = concat(concat(target_doc, "."), call_doc)
    doc = if wrap_in_parens?, do: wrap_in_parens(doc), else: doc
    {doc, state}
  end

  # expression.function(arguments)
  defp remote_to_algebra({{:., _, [target, fun]}, _, args}, context, state) when is_atom(fun) do
    {target_doc, state} = remote_target_to_algebra(target, state)
    {{call_doc, state}, wrap_in_parens?} = call_args_to_algebra(args, context, :maybe, state)

    fun_doc =
      fun
      |> Code.Identifier.inspect_as_function()
      |> string()
      |> concat(call_doc)

    doc = concat(concat(target_doc, "."), fun_doc)
    doc = if wrap_in_parens?, do: wrap_in_parens(doc), else: doc
    {doc, state}
  end

  # call(call)(arguments)
  defp remote_to_algebra({target, _, args}, context, state) do
    {target_doc, state} = quoted_to_algebra(target, :no_parens_argument, state)
    {{call_doc, state}, wrap_in_parens?} = call_args_to_algebra(args, context, :no, state)

    doc = concat(target_doc, call_doc)
    doc = if wrap_in_parens?, do: wrap_in_parens(doc), else: doc
    {doc, state}
  end

  defp remote_target_to_algebra({:fn, _, [_ | _]} = quoted, state) do
    # This change is not semantically required but for beautification.
    {doc, state} = quoted_to_algebra(quoted, :no_parens_argument, state)
    {wrap_in_parens(doc), state}
  end

  defp remote_target_to_algebra(quoted, state) do
    quoted_to_algebra_with_parens_if_necessary(quoted, :no_parens_argument, state)
  end

  # function(arguments)
  defp local_to_algebra(fun, args, context, state) when is_atom(fun) do
    skip_parens = if skip_parens?(fun, args, state), do: :yes, else: :maybe

    {{call_doc, state}, wrap_in_parens?} =
      call_args_to_algebra(args, context, skip_parens, state)

    doc =
      fun
      |> Atom.to_string()
      |> string()
      |> concat(call_doc)

    doc = if wrap_in_parens?, do: wrap_in_parens(doc), else: doc
    {doc, state}
  end

  defp call_args_to_algebra([], _context, _skip_parens, state) do
    {{"()", state}, false}
  end

  defp call_args_to_algebra(args, context, skip_parens, state) do
    {args, last} = split_last(args)

    if blocks = do_end_blocks(last, [], @keywords) do
      {call_doc, state} =
        case args do
          [] ->
            {empty(), state}
          _ ->
            {args, last} = split_last(args)
            call_args_to_algebra_without_do_end_blocks(args, last, skip_parens != :no, state)
        end

      {blocks_doc, state} = do_end_blocks_to_algebra(blocks, state)
      call_doc = call_doc |> space(blocks_doc) |> line("end") |> force_break()
      {{call_doc, state}, context == :no_parens_argument}
    else
      skip_parens? = context == :block and skip_parens == :yes
      {call_args_to_algebra_without_do_end_blocks(args, last, skip_parens?, state), false}
    end
  end

  defp call_args_to_algebra_without_do_end_blocks(left, right, skip_parens?, state) do
    context = if skip_parens?, do: :no_parens_argument, else: :argument

    {left, right} =
      if keyword?(right) do
        {kw_left, kw_right} = split_last(right)
        {left ++ kw_left, kw_right}
      else
        {left, right}
      end

    {left_doc, state} = args_to_algebra(left, state, &quoted_to_algebra(&1, context, &2))
    {right_doc, state} = quoted_to_algebra(right, context, state)

    doc =
      apply_cancel_break(right, right_doc, fn right_doc ->
        args_doc =
          if left == [] do
            right_doc
          else
            glue(concat(left_doc, ","), right_doc)
          end

        if skip_parens? do
          " "
          |> concat(nest(args_doc, :cursor, :break))
          |> group()
        else
          surround("(", args_doc, ")", :break)
        end
      end)

    {doc, state}
  end

  defp skip_parens?(fun, args, %{locals_without_parens: locals_without_parens}) do
    length = length(args)
    length > 0 and Enum.any?(locals_without_parens, fn {key, val} ->
      key == fun and (val == :* or val == length)
    end)
  end

  defp do_end_blocks([{{:__block__, meta, [atom]}, value} | list], acc, available) do
    if meta[:format] == :keyword and atom in available do
      do_end_blocks(list, [{atom, value} | acc], List.delete(available, atom))
    end
  end

  defp do_end_blocks(rest, acc, _keywords) do
    if rest == [] and Keyword.has_key?(acc, :do) do
      acc
    end
  end

  defp do_end_blocks_to_algebra(blocks, state) do
    {acc, state} = do_end_block_to_algebra(:do, Keyword.fetch!(blocks, :do), state)

    Enum.reduce(tl(@keywords), {acc, state}, fn key, {acc, state} ->
      case Keyword.fetch(blocks, key) do
        {:ok, value} ->
          {doc, state} = do_end_block_to_algebra(key, value, state)
          {line(acc, doc), state}
        :error ->
          {acc, state}
      end
    end)
  end

  defp do_end_block_to_algebra(key, value, state) do
    key_doc = Atom.to_string(key)
    {value_doc, state} = clauses_to_algebra(value, state)
    {nest(line(key_doc, group(value_doc)), 2), state}
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
    {doc, state} = block_to_algebra(quoted, state)
    doc = surround("\#{", doc, "}")
    interpolation_to_algebra(entries, escape, state, concat(acc, doc), last)
  end

  defp interpolation_to_algebra([], _escape, state, acc, last) do
    {concat(acc, last), state}
  end

  ## Sigils

  defp maybe_sigil_to_algebra(fun, meta, args, state) do
    case {Atom.to_string(fun), args} do
      {<<"sigil_", name>>, [{:<<>>, _, entries}, modifiers]} ->
        opening_terminator = Keyword.fetch!(meta, :terminator)
        acc = <<?~, name, opening_terminator::binary>>

        if opening_terminator in [@double_heredoc, @single_heredoc] do
          acc = force_break(concat(acc, line()))
          closing_terminator = concat(opening_terminator, List.to_string(modifiers))
          interpolation_to_algebra(entries, :heredoc, state, acc, closing_terminator)
        else
          escape = closing_sigil_terminator(opening_terminator)
          closing_terminator = concat(escape, List.to_string(modifiers))
          interpolation_to_algebra(entries, escape, state, acc, closing_terminator)
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
      |> args_to_algebra(state, &bitstring_to_algebra(&1, &2, last))
    {container(args, "<<", args_doc, ">>"), state}
  end

  defp bitstring_to_algebra({{:::, _, [segment, spec]}, i}, state, last) do
    {doc, state} = quoted_to_algebra(segment, :argument, state)
    {spec, state} = bitstring_spec_to_algebra(spec, state)
    doc = concat(concat(doc, "::"), wrap_in_parens_if_inspected_atom(spec))
    {bitstring_wrap_parens(doc, i, last), state}
  end

  defp bitstring_to_algebra({segment, i}, state, last) do
    {doc, state} = quoted_to_algebra(segment, :argument, state)
    {bitstring_wrap_parens(doc, i, last), state}
  end

  defp bitstring_spec_to_algebra({:-, _, [left, right]}, state) do
    {left, state} = bitstring_spec_to_algebra(left, state)
    {right, state} = quoted_to_algebra_with_parens_if_necessary(right, :argument, state)
    {concat(concat(left, "-"), right), state}
  end

  defp bitstring_spec_to_algebra(spec, state) do
    quoted_to_algebra_with_parens_if_necessary(spec, :argument, state)
  end

  defp bitstring_wrap_parens(doc, i, last) do
    if i == 0 or i == last do
      string = format_to_string(doc)

      if i == 0 and String.starts_with?(string, "<<") or
           i == last and String.ends_with?(string, ">>") do
        concat(concat("(", doc), ")")
      else
        doc
      end
    else
      doc
    end
  end

  ## Literals

  defp list_to_algebra([], state) do
    {"[]", state}
  end

  defp list_to_algebra(args, state) do
    {args_doc, state} = args_to_algebra(args, state, &quoted_to_algebra(&1, :argument, &2))
    {container(args, "[", args_doc, "]"), state}
  end

  defp map_to_algebra(name_doc, [], state) do
    {"%" |> concat(name_doc) |> concat("{}"), state}
  end

  defp map_to_algebra(name_doc, args, state) do
    {args_doc, state} = args_to_algebra(args, state, &quoted_to_algebra(&1, :argument, &2))
    name_doc = "%" |> concat(name_doc) |> concat("{")
    {container(args, name_doc, args_doc, "}"), state}
  end

  defp tuple_to_algebra([], state) do
    {"{}", state}
  end

  defp tuple_to_algebra(args, state) do
    {args_doc, state} = args_to_algebra(args, state, &quoted_to_algebra(&1, :argument, &2))
    {container(args, "{", args_doc, "}"), state}
  end

  defp atom_to_algebra(atom) when atom in [nil, true, false] do
    Atom.to_string(atom)
  end

  defp atom_to_algebra(atom) do
    string = Atom.to_string(atom)

    case Code.Identifier.classify(atom) do
      type when type in [:callable_local, :callable_operator, :not_callable] ->
        [?:, string] |> IO.iodata_to_binary |> string()
      _ ->
        [?:, ?", String.replace(string, "\"", "\\\""), ?"] |> IO.iodata_to_binary |> string()
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

  defp escape_string(string, :heredoc) do
    heredoc_to_algebra(String.split(string, "\n"))
  end

  defp escape_string(string, escape) when is_binary(escape) do
    string
    |> String.replace(escape, "\\" <> escape)
    |> String.split("\n")
    |> Enum.reverse()
    |> Enum.map(&string/1)
    |> Enum.reduce(&concat(&1, concat(nest(line(), :reset), &2)))
  end

  defp heredoc_to_algebra([string]) do
    string(string)
  end

  defp heredoc_to_algebra([string, ""]) do
    string
    |> string()
    |> concat(line())
  end

  defp heredoc_to_algebra([string, "" | rest]) do
    string
    |> string()
    |> concat(nest(line(), :reset))
    |> line(heredoc_to_algebra(rest))
  end

  defp heredoc_to_algebra([string | rest]) do
    line(string(string), heredoc_to_algebra(rest))
  end

  ## Anonymous functions

  # fn -> block end
  defp anon_fun_to_algebra([{:"->", _, [[], body]}], state) do
    {body_doc, state} = block_to_algebra(body, state)

    doc =
      "fn ->"
      |> glue(body_doc)
      |> nest(2)
      |> glue("end")
      |> group()

    {doc, state}
  end

  # fn x -> y end
  # fn x ->
  #   y
  # end
  defp anon_fun_to_algebra([{:"->", _, [args, body]}], state) do
    {args_doc, state} = clause_args_to_algebra(args, state)
    {body_doc, state} = block_to_algebra(body, state)

    doc =
      "fn "
      |> concat(group(args_doc))
      |> concat(" ->")
      |> nest(1)
      |> glue(body_doc)
      |> nest(2)
      |> glue("end")
      |> group()

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
    {"fn" |> line(group(clauses_docs)) |> nest(2) |> line("end") |> force_break(), state}
  end

  ## Type functions

  # (-> block)
  defp type_fun_to_algebra([{:"->", _, [[], body]}], state) do
    {body_doc, state} = block_to_algebra(body, state)

    doc =
      "(-> "
      |> concat(nest(body_doc, :cursor))
      |> concat(")")
      |> group()

    {doc, state}
  end

  # (x -> y)
  # (x ->
  #    y)
  defp type_fun_to_algebra([{:"->", _, [args, body]}], state) do
    {args_doc, state} = clause_args_to_algebra(args, state)
    {body_doc, state} = block_to_algebra(body, state)

    clause_doc =
      " ->"
      |> glue(body_doc)
      |> nest(2)

    doc =
      args_doc
      |> group()
      |> concat(clause_doc)
      |> wrap_in_parens()
      |> group()

    {doc, state}
  end

  # (
  #   args1 ->
  #     block1
  #   args2 ->
  #     block2
  # )
  defp type_fun_to_algebra(clauses, state) do
    {clauses_docs, state} = clauses_to_algebra(clauses, state)
    {"(" |> line(group(clauses_docs)) |> nest(2) |> line(")") |> force_break(), state}
  end

  ## Clauses

  defp clauses_to_algebra([{:"->", _, _} = clause | clauses], state) do
    {clause_doc, state} = clause_to_algebra(clause, state)

    # If we have at least three clauses, then we apply extra empty lines.
    empty_lines? = match?([_, _ | _], clauses)

    Enum.reduce(clauses, {clause_doc, state}, fn clause, {doc_acc, state_acc} ->
      {clause_doc, state_acc} = clause_to_algebra(clause, state_acc)
      doc_acc = if empty_lines?, do: concat(doc_acc, maybe_empty_line()), else: doc_acc
      {line(doc_acc, clause_doc), state_acc}
    end)
  end

  defp clauses_to_algebra(other, state) do
    block_to_algebra(other, state)
  end

  defp clause_to_algebra({:"->", _, [[], body]}, state) do
    {body_doc, state} = block_to_algebra(body, state)
    {"() ->" |> glue(body_doc) |> nest(2), state}
  end

  defp clause_to_algebra({:"->", _, [args, body]}, %{operand_nesting: nesting} = state) do
    {args_doc, state} = clause_args_to_algebra(args, %{state | operand_nesting: nesting + 2})
    {body_doc, state} = block_to_algebra(body, %{state | operand_nesting: nesting})
    {concat(args_doc, " ->" |> glue(body_doc) |> nest(2)), state}
  end

  # fn a, b, c when d -> e end
  defp clause_args_to_algebra([{:when, _, args}], state) do
    {args, right} = split_last(args)
    left = {:special, :arguments, args}
    binary_op_to_algebra(:when, "when", left, right, :no_parens_argument, state)
  end

  # fn a, b, c -> e end
  defp clause_args_to_algebra(args, state) do
    args_to_algebra(args, state, &quoted_to_algebra(&1, :no_parens_argument, &2))
  end

  ## Quoted helpers

  defp at_least_argument(:block), do: :argument
  defp at_least_argument(other), do: other

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
      wrap_in_parens(doc)
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

  defp wrap_in_parens(doc) do
    concat(concat("(", nest(doc, :cursor)), ")")
  end

  defp args_to_algebra([], state, _fun) do
    {empty(), state}
  end

  defp args_to_algebra([arg | args], state, fun) do
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

  defp operator?(quoted) do
    unary_operator?(quoted) or binary_operator?(quoted)
  end

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

  # TODO: Perform simple check for all data structures (calls not included).
  defp container(_args, left, doc, right) do
    surround(left, doc, right)
  end

  defp apply_cancel_break(arg, doc, fun) do
    if apply_cancel_break?(arg) do
      doc
      |> cancel_break(:enabled)
      |> fun.()
      |> cancel_break(:disabled)
    else
      fun.(doc)
    end
  end

  defp apply_cancel_break?({:<<>>, meta, [_ | _] = entries}) do
    meta[:format] == :bin_heredoc or not interpolated?(entries)
  end

  defp apply_cancel_break?({{:., _, [String, :to_charlist]}, _, [{:<<>>, meta, [_ | _]}]}) do
    meta[:format] == :list_heredoc
  end

  defp apply_cancel_break?({:{}, _, _}) do
    true
  end

  defp apply_cancel_break?({:__block__, _meta, [{_, _}]}) do
    true
  end

  defp apply_cancel_break?({:__block__, meta, [string]}) when is_binary(string) do
    meta[:format] == :bin_heredoc
  end

  defp apply_cancel_break?({:__block__, meta, [list]}) when is_list(list) do
    meta[:format] != :charlist
  end

  defp apply_cancel_break?({form, _, [_ | _]}) when form in [:fn, :%{}, :%] do
    true
  end

  defp apply_cancel_break?({fun, meta, args}) when is_atom(fun) and is_list(args) do
    meta[:terminator] in [@double_heredoc, @single_heredoc] and
      (fun |> Atom.to_string() |> String.starts_with?("sigil_"))
  end

  defp apply_cancel_break?({{:__block__, _, [atom]}, expr}) when is_atom(atom) do
    apply_cancel_break?(expr)
  end

  defp apply_cancel_break?(_) do
    false
  end

  defp keyword?([{key, _} | list]) do
    keyword_key?(key) and keyword?(list)
  end

  defp keyword?(rest) do
    rest == []
  end

  defp keyword_key?({:__block__, meta, [_]}) do
    meta[:format] == :keyword
  end

  defp keyword_key?({{:., _, [:erlang, :binary_to_atom]}, _, [{:<<>>, meta, _}, :utf8]}) do
    meta[:format] == :keyword
  end

  defp keyword_key?(_) do
    false
  end

  ## Algebra helpers

  defp format_to_string(doc) do
    doc |> Inspect.Algebra.format(:infinity) |> IO.iodata_to_binary()
  end

  defp maybe_empty_line() do
    nest(break(""), :reset)
  end

  defp surround(left, doc, right, nest \\ :always) do
    group(glue(nest(glue(left, "", doc), 2, nest), "", right))
  end

  defp nest_by_length(doc, string) do
    nest(doc, String.length(string))
  end

  defp split_last(list) do
    {left, [right]} = Enum.split(list, -1)
    {left, right}
  end
end
