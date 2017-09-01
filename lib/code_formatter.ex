defmodule CodeFormatter do
  import Inspect.Algebra, except: [format: 2, surround: 3, surround: 4]

  @line_length 98
  @double_quote "\""
  @double_heredoc "\"\"\""
  @single_quote "'"
  @single_heredoc "'''"
  @do_end_keywords [:rescue, :catch, :else, :after]
  @newlines 2

  # Operators that do not have space between operands
  @no_space_binary_operators [:..]

  # Operators that do not have newline between operands (as well as => and keywords)
  @no_newline_binary_operators [:\\, :in]

  # Left associative operators that start on the next line in case of breaks
  @left_new_line_before_binary_operators [:|>, :~>>, :<<~, :~>, :<~, :<~>, :<|>]

  # Right associative operators that start on the next line in case of breaks
  @right_new_line_before_binary_operators [:|]

  # Operators that are logical cannot be mixed without parens
  @required_parens_logical_binary_operands [:||, :|||, :or, :&&, :&&&, :and]

  # Operators that always require parens on operands when they are the parent
  @required_parens_on_binary_operands [:|>, :<<<, :>>>, :<~, :~>, :<<~, :~>>, :<~>, :<|>,
                                       :^^^, :in, :++, :--, :.., :<>]

  @locals_without_parens [
    # Special forms
    alias: 1,
    alias: 2,
    import: 1,
    import: 2,
    require: 1,
    require: 2,
    for: :*,
    with: :*,

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

  The formatter is opinionated by design and provides little to no
  configuration. The formatter also never changes the semantics of
  the code.

  ## Options

    * `:line_length` - the line length to aim for when formatting
      the document

    * `:locals_without_parens` - a keyword list of name and arity
      pairs that should be kept without parens whenever possible.
      The arity may be the atom `:*`, which implies all arities of
      that name. The formatter already includes a list of functions
      and this option augments this list.

    * `:rename_deprecated_at` - rename all known deprecated functions
      at the given version to their non-deprecated equivalent. It
      expects a valid `Version` which is usually the minimum Elixir
      version supported by the project.

  ## Keeping input formatting

  The formatter respects the input format in some cases. Those are
  listed below:

    * Insignificant digits in numbers are kept as is. The formatter
      however inserts underscores for decimal numbers with more than
      5 digits

    * Strings, charlists, atoms and sigils are kept as is. No character
      is automatically escaped or unescaped. The choice of delimiter is
      also respected from the input

    * Newlines inside blocks are kept as in the input except for:
      1) expressions that take multiple lines will always have an empty
      line before and after and 2) empty lines are always squeezed
      together into a single empty line

    * The choice between `:do` keyword and `do/end` blocks is left
      to the user

    * Lists, tuples, bitstrings, maps, and structs will be expanded if
      they were also expanded in the input. For example with a newline
      after `[` and another before `]` in lists

    * Pipeline operators, like `|>` and others with the same precedence,
      will span multiple lines if they spanned multiple lines in the input

  The behaviours above are not guaranteed. We may remove or add new
  rules in the future. The goal of documenting them is to provide better
  understanding on what to expect from the formatter.
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
    |> Code.string_to_quoted!(formatter_metadata: true, unescape: false)
    |> block_to_algebra(state(opts))
    |> elem(0)
  end

  defp state(opts) do
    rename_deprecated_at =
      if version = opts[:rename_deprecated_at] do
        case Version.parse(version) do
          {:ok, parsed} ->
            parsed
          :error ->
            raise ArgumentError, "invalid version #{inspect(version)} given to :rename_deprecated_at"
        end
      end

    locals_without_parens = Keyword.get(opts, :locals_without_parens, [])

    %{
      locals_without_parens: locals_without_parens ++ @locals_without_parens,
      operand_nesting: 2,
      rename_deprecated_at: rename_deprecated_at
    }
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
        bitstring_to_algebra(meta, entries, state)
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
    {call_doc, state} = list_to_algebra([], args, state)
    {concat(target_doc, call_doc), state}
  end

  # %Foo{}
  # %name{foo: 1}
  # %name{bar | foo: 1}
  defp quoted_to_algebra({:%, _, [name, {:%{}, meta, args}]}, _context, state) do
    {name_doc, state} = quoted_to_algebra(name, :argument, state)
    map_to_algebra(meta, name_doc, args, state)
  end

  # %{foo: 1}
  # %{foo => bar}
  # %{name | foo => bar}
  defp quoted_to_algebra({:%{}, meta, args}, _context, state) do
    map_to_algebra(meta, empty(), args, state)
  end

  # {}
  # {1, 2}
  defp quoted_to_algebra({:{}, meta, args}, _context, state) do
    tuple_to_algebra(meta, args, state)
  end

  defp quoted_to_algebra({:__block__, meta, [{left, right}]}, _context, state) do
    tuple_to_algebra(meta, [left, right], state)
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
        list_to_algebra(meta, list, state)
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

  defp quoted_to_algebra({:__aliases__, _meta, [head | tail]}, context, state) do
    {doc, state} =
      if is_atom(head) do
        {Atom.to_string(head), state}
      else
        quoted_to_algebra_with_parens_if_necessary(head, context, state)
      end
    {Enum.reduce(tail, doc, &concat(&2, "." <> Atom.to_string(&1))), state}
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

  defp quoted_to_algebra({:@, meta, [arg]}, context, state) do
    module_attribute_to_algebra(meta, arg, context, state)
  end

  # not(left in right)
  # left not in right
  defp quoted_to_algebra({:not, meta, [{:in, _, [left, right]}]}, context, state) do
    binary_op_to_algebra(:in, "not in", meta, left, right, context, state)
  end

  defp quoted_to_algebra({:fn, _, [_ | _] = clauses}, _context, state) do
    anon_fun_to_algebra(clauses, state)
  end

  defp quoted_to_algebra({fun, meta, args}, context, state)
       when is_atom(fun) and is_list(args) do
    with :error <- maybe_sigil_to_algebra(fun, meta, args, state),
         :error <- maybe_unary_op_to_algebra(fun, meta, args, context, state),
         :error <- maybe_binary_op_to_algebra(fun, meta, args, context, state),
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

  defp block_to_algebra({:__block__, _, [_, _ | _] = args}, state) do
    length = length(args) - 1

    {args_doc, state} =
      args
      |> group_blocks(:none, 0)
      |> Enum.map_reduce(state, fn {left, quoted, right, i}, state ->
           {doc, state} = quoted_to_algebra(quoted, :block, state)
           doc = if i != 0, do: concat(left, doc), else: doc
           doc = if i != length, do: concat(doc, concat(collapse_lines(2), right)), else: doc
           {group(doc), state}
         end)

    {args_doc |> Enum.reduce(&line(&2, &1)) |> force_break(), state}
  end

  defp block_to_algebra(block, state) do
    {doc, state} = quoted_to_algebra(block, :block, state)
    {group(doc), state}
  end

  # Below are the rules for block rendering in the formatter:
  #
  #   1. respect the user's choice
  #   2. and add empty lines around expressions that take multiple lines
  #      (except for module attributes)
  #   3. empty lines are collapsed as to not exceed more than one
  #
  defp group_blocks([{local, meta, _} = expr | exprs], _previous, index) when is_list(meta) do
    left = group_line_separator(local, meta)
    right = group_next_line_separator(exprs, local)
    entry = {left, expr, right, index}

    case group_blocks(exprs, :none, index + 1) do
      [{_, next_expr, next_right, next_index} | rest] ->
        [entry, {right, next_expr, next_right, next_index} | rest]
      [] ->
        [entry]
    end
  end

  defp group_blocks([], _, _) do
    []
  end

  defp group_next_line_separator([{_, meta, _} | _], local) when is_list(meta) do
    group_line_separator(local, meta)
  end

  defp group_next_line_separator([], _) do
    line()
  end

  defp group_line_separator(:@, meta) do
    if Keyword.get(meta, :newlines, @newlines) >= @newlines, do: line(), else: empty()
  end

  defp group_line_separator(_, meta) do
    if Keyword.get(meta, :newlines, @newlines) >= @newlines, do: line(), else: break("")
  end

  ## Operators

  defp maybe_unary_op_to_algebra(fun, meta, args, context, state) do
    with [arg] <- args,
         {_, _} <- Code.Identifier.unary_op(fun) do
      unary_op_to_algebra(fun, meta, arg, context, state)
    else
      _ -> :error
    end
  end

  defp unary_op_to_algebra(op, _meta, arg, context, state) do
    {doc, state} = quoted_to_algebra(arg, if_operand_or_block(context, :operand), state)

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

  defp maybe_binary_op_to_algebra(fun, meta, args, context, state) do
    with [left, right] <- args,
         {_, _} <- Code.Identifier.binary_op(fun) do
      binary_op_to_algebra(fun, Atom.to_string(fun), meta, left, right, context, state)
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
  defp binary_op_to_algebra(op, op_string, meta, left_arg, right_arg, context, state) do
    %{operand_nesting: nesting} = state
    binary_op_to_algebra(op, op_string, meta, left_arg, right_arg, context, state, nil, nesting)
  end

  defp binary_op_to_algebra(op, op_string, meta, left_arg, right_arg, context, state, parent_info, nesting) do
    op_info = Code.Identifier.binary_op(op)
    left_context = if_operand_or_block(context, :argument)
    right_context = if_operand_or_block(context, :operand)

    {left, state} =
      binary_operand_to_algebra(left_arg, left_context, state, op, op_info, :left, 2)

    {right, state} =
      binary_operand_to_algebra(right_arg, right_context, state, op, op_info, :right, 0)

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
          doc = if Keyword.get(meta, :eol, false), do: force_break(doc), else: doc
          if op_info == parent_info, do: doc, else: group(doc)

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

          doc = glue(left, concat(op_string, right))
          if is_nil(parent_info) or op_info == parent_info, do: doc, else: group(doc)

        true ->
          with_next_break_fits(next_break_fits?(right_arg), right, fn right ->
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
  defp binary_operand_to_algebra({:__block__, _, [{op, meta, [arg]}]}, context, state, :in,
                                 _parent_info, :left, _nesting) when op in [:not, :!] do
    {doc, state} = unary_op_to_algebra(op, meta, arg, context, state)
    {concat(concat("(", nest(doc, 1)), ")"), state}
  end

  defp binary_operand_to_algebra(operand, context, state, parent_op, parent_info, side, nesting) do
    with {op, meta, [left, right]} <- operand,
         op_info = Code.Identifier.binary_op(op),
         {_assoc, prec} <- op_info do
      {parent_assoc, parent_prec} = parent_info
      op_string = Atom.to_string(op)

      cond do
        # If the operator has the same precedence as the parent and is on
        # the correct side, we respect the nesting rule to avoid multiple
        # nestings.
        parent_prec == prec and parent_assoc == side ->
          binary_op_to_algebra(op, op_string, meta, left, right, context, state, op_info, nesting)

        # If the parent requires parens or the precedence is inverted or
        # it is in the wrong side, then we *need* parenthesis.
        (parent_op in @required_parens_on_binary_operands and
            op not in @no_space_binary_operators) or
            (op in @required_parens_logical_binary_operands and
              parent_op in @required_parens_logical_binary_operands) or
            parent_prec > prec or (parent_prec == prec and parent_assoc != side) ->
          {operand, state} =
            binary_op_to_algebra(op, op_string, meta, left, right, context, state, op_info, 2)

          {concat(concat("(", nest(operand, 1)), ")"), state}

        # Otherwise, we rely on precedence but also nest.
        true ->
          binary_op_to_algebra(op, op_string, meta, left, right, context, state, op_info, 2)
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
  defp module_attribute_to_algebra(_meta, {:__aliases__, _, [_, _ | _]} = quoted, _context, state) do
    {doc, state} = quoted_to_algebra(quoted, :argument, state)
    {concat(concat("@(", doc), ")"), state}
  end

  # @foo bar
  # @foo(bar)
  defp module_attribute_to_algebra(meta, {name, _, [_] = args} = expr, context, state)
       when is_atom(name) and name not in [:__block__, :__aliases__] do
    if Code.Identifier.classify(name) == :callable_local do
      {{call_doc, state}, wrap_in_parens?} =
        call_args_to_algebra(args, context, :skip_unless_argument, state)

      doc =
        "@#{name}"
        |> string()
        |> concat(call_doc)

      doc = if wrap_in_parens?, do: wrap_in_parens(doc), else: doc
      {doc, state}
    else
      unary_op_to_algebra(:@, meta, expr, context, state)
    end
  end

  # @foo
  # @(foo.bar())
  defp module_attribute_to_algebra(meta, quoted, context, state) do
    unary_op_to_algebra(:@, meta, quoted, context, state)
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

  defp capture_target_to_algebra({:/, _, [{{:., _, [target, fun]}, _, []}, {:__block__, _, [arity]}]}, _context, state)
      when is_atom(fun) and is_integer(arity) do
    {target_doc, state} = remote_target_to_algebra(target, state)
    fun = remote_fun_to_algebra(target, fun, arity, state)
    {{target_doc |> nest(1) |> concat(string(".#{fun}/#{arity}")), state}, false}
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
    {call_doc, state} = tuple_to_algebra([], args, state)
    {concat(concat(target_doc, "."), call_doc), state}
  end

  # expression.(arguments)
  defp remote_to_algebra({{:., _, [target]}, _, args}, context, state) do
    {target_doc, state} = remote_target_to_algebra(target, state)
    {{call_doc, state}, wrap_in_parens?} = call_args_to_algebra(args, context, :skip_if_do_end, state)
    doc = concat(concat(target_doc, "."), call_doc)
    doc = if wrap_in_parens?, do: wrap_in_parens(doc), else: doc
    {doc, state}
  end

  # Mod.function()
  # var.function
  defp remote_to_algebra({{:., _, [target, fun]}, _, []}, _context, state) when is_atom(fun) do
    {target_doc, state} = remote_target_to_algebra(target, state)
    fun = remote_fun_to_algebra(target, fun, 0, state)

    if remote_target_is_a_module?(target) do
      {target_doc |> concat(".") |> concat(string(fun)) |> concat("()"), state}
    else
      {target_doc |> concat(".") |> concat(string(fun)), state}
    end
  end

  # expression.function(arguments)
  defp remote_to_algebra({{:., _, [target, fun]}, _, args}, context, state) when is_atom(fun) do
    {target_doc, state} = remote_target_to_algebra(target, state)
    fun = remote_fun_to_algebra(target, fun, length(args), state)

    {{call_doc, state}, wrap_in_parens?} =
      call_args_to_algebra(args, context, :skip_if_do_end, state)

    doc = concat(concat(target_doc, "."), concat(string(fun), call_doc))
    doc = if wrap_in_parens?, do: wrap_in_parens(doc), else: doc
    {doc, state}
  end

  # call(call)(arguments)
  defp remote_to_algebra({target, _, args}, context, state) do
    {target_doc, state} = quoted_to_algebra(target, :no_parens, state)
    {{call_doc, state}, wrap_in_parens?} = call_args_to_algebra(args, context, :required, state)

    doc = concat(target_doc, call_doc)
    doc = if wrap_in_parens?, do: wrap_in_parens(doc), else: doc
    {doc, state}
  end

  defp remote_target_is_a_module?(target) do
    case target do
      {:__MODULE__, _, context} when is_atom(context) -> true
      {:__block__, _, [atom]} when is_atom(atom) -> true
      {:__aliases__, _, _} -> true
      _ -> false
    end
  end

  defp remote_fun_to_algebra(target, fun, arity, state) do
    %{rename_deprecated_at: since} = state

    atom_target =
      case since && target do
        {:__aliases__, _, [alias | _] = aliases} when is_atom(alias) ->
          Module.concat(aliases)
        {:__block__, _, [atom]} when is_atom(atom) ->
          atom
        _ ->
          nil
      end

    with {fun, requirement} <- deprecated(atom_target, fun, arity),
         true <- Version.match?(since, requirement) do
      fun
    else
      _ -> Code.Identifier.inspect_as_function(fun)
    end
  end

  # We can only rename functions in the same module because
  # introducing a new module may wrong due to aliases.
  defp deprecated(Enum, :partition, 2), do: {"split_with", "~> 1.4"}
  defp deprecated(_, _, _), do: :error

  defp remote_target_to_algebra({:fn, _, [_ | _]} = quoted, state) do
    # This change is not semantically required but for beautification.
    {doc, state} = quoted_to_algebra(quoted, :no_parens, state)
    {wrap_in_parens(doc), state}
  end

  defp remote_target_to_algebra(quoted, state) do
    quoted_to_algebra_with_parens_if_necessary(quoted, :no_parens, state)
  end

  # function(arguments)
  defp local_to_algebra(fun, args, context, state) when is_atom(fun) do
    skip_parens =
      if skip_parens?(fun, args, state), do: :skip_unless_argument, else: :skip_if_do_end

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

  # parens may one of:
  #
  #   * :skip_if_block - skips parens if we are inside the block context
  #   * :skip_if_do_end - skip parens if we are do-end
  #   * :required - never skip parens
  #
  defp call_args_to_algebra([], _context, _parens, state) do
    {{"()", state}, false}
  end

  defp call_args_to_algebra(args, context, parens, state) do
    {args, last} = split_last(args)

    if blocks = do_end_blocks(last) do
      {call_doc, state} =
        case args do
          [] ->
            {empty(), state}
          _ ->
            {args, last} = split_last(args)
            call_args_to_algebra_without_do_end_blocks(args, last, parens != :required, state)
        end

      {blocks_doc, state} = do_end_blocks_to_algebra(blocks, state)
      call_doc = call_doc |> space(blocks_doc) |> line("end") |> force_break()
      {{call_doc, state}, context == :no_parens}
    else
      skip_parens? = parens == :skip_unless_argument and context in [:block, :operand]
      {call_args_to_algebra_without_do_end_blocks(args, last, skip_parens?, state), false}
    end
  end

  defp call_args_to_algebra_without_do_end_blocks(left, right, skip_parens?, state) do
    context = if skip_parens?, do: :no_parens, else: :argument
    multiple_generators? = multiple_generators?([right | left])

    {left_doc, state} = args_to_algebra(left, state, &quoted_to_algebra(&1, context, &2))
    {right_doc, state} = quoted_to_algebra(right, context, state)

    doc =
      if left != [] and skip_parens? and not multiple_generators? and keyword?(right) do
        call_args_to_algebra_with_no_parens_keywords(left_doc, right_doc)
      else
        with_next_break_fits(next_break_fits?(right), right_doc, fn right_doc ->
          args_doc =
            if left == [] do
              right_doc
            else
              glue(concat(left_doc, ","), right_doc)
            end

          args_doc =
            if multiple_generators? do
              force_break(args_doc)
            else
              args_doc
            end

          if skip_parens? do
            " "
            |> concat(nest(args_doc, :cursor, :break))
            |> group()
          else
            surround("(", args_doc, ")", :break)
          end
        end)
      end

    {doc, state}
  end

  defp call_args_to_algebra_with_no_parens_keywords(left_doc, right_doc) do
    right_doc = break(" ") |> concat(right_doc) |> group()

    with_next_break_fits(true, right_doc, fn right_doc ->
      args_doc = concat(concat(left_doc, ","), right_doc)

      " "
      |> concat(nest(args_doc, :cursor, :break))
      |> nest(2)
      |> group()
    end)
  end

  defp skip_parens?(fun, args, %{locals_without_parens: locals_without_parens}) do
    length = length(args)
    length > 0 and Enum.any?(locals_without_parens, fn {key, val} ->
      key == fun and (val == :* or val == length)
    end)
  end

  defp multiple_generators?(args) do
    Enum.count(args, &match?({:<-, _, [_, _]}, &1)) >= 2
  end

  defp do_end_blocks([{{:__block__, meta, [:do]}, _} | _] = blocks) do
    if meta[:format] == :block do
      for {{:__block__, _, [key]}, value} <- blocks do
        {key, value}
      end
    end
  end

  defp do_end_blocks(_) do
    nil
  end

  defp do_end_blocks_to_algebra(blocks, state) do
    {acc, state} = do_end_block_to_algebra(:do, Keyword.fetch!(blocks, :do), state)

    ordered =
      for key <- @do_end_keywords,
          value <- Keyword.get_values(blocks, key),
          do: {key, value}

    Enum.reduce(ordered, {acc, state}, fn {key, value}, {acc, state} ->
      {doc, state} = do_end_block_to_algebra(key, value, state)
      {line(acc, doc), state}
    end)
  end

  defp do_end_block_to_algebra(key, value, state) do
    key_doc = Atom.to_string(key)
    {value_doc, state} = clauses_to_algebra(value, state)
    {key_doc |> line(value_doc) |> nest(2), state}
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

  defp bitstring_to_algebra(meta, args, state) do
    last = length(args) - 1
    {args_doc, state} =
      args
      |> Enum.with_index()
      |> args_to_algebra(state, &bitstring_segment_to_algebra(&1, &2, last))
    {container(meta, "<<", args_doc, ">>"), state}
  end

  defp bitstring_segment_to_algebra({{:::, _, [segment, spec]}, i}, state, last) do
    {doc, state} = quoted_to_algebra(segment, :argument, state)
    {spec, state} = bitstring_spec_to_algebra(spec, state)
    doc = concat(concat(doc, "::"), wrap_in_parens_if_inspected_atom(spec))
    {bitstring_wrap_parens(doc, i, last), state}
  end

  defp bitstring_segment_to_algebra({segment, i}, state, last) do
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

  defp list_to_algebra(_meta, [], state) do
    {"[]", state}
  end

  defp list_to_algebra(meta, args, state) do
    {args_doc, state} = args_to_algebra(args, state, &quoted_to_algebra(&1, :argument, &2))
    {container(meta, "[", args_doc, "]"), state}
  end

  defp map_to_algebra(_meta, name_doc, [], state) do
    {"%" |> concat(name_doc) |> concat("{}"), state}
  end

  defp map_to_algebra(meta, name_doc, args, state) do
    {args_doc, state} = args_to_algebra(args, state, &quoted_to_algebra(&1, :argument, &2))
    name_doc = "%" |> concat(name_doc) |> concat("{")
    {container(meta, name_doc, args_doc, "}"), state}
  end

  defp tuple_to_algebra(_meta, [], state) do
    {"{}", state}
  end

  defp tuple_to_algebra(meta, args, state) do
    {args_doc, state} = args_to_algebra(args, state, &quoted_to_algebra(&1, :argument, &2))
    {container(meta, "{", args_doc, "}"), state}
  end

  defp atom_to_algebra(atom) when atom in [nil, true, false] do
    Atom.to_string(atom)
  end

  defp atom_to_algebra(atom) do
    string = Atom.to_string(atom)

    iodata =
      case Code.Identifier.classify(atom) do
        type when type in [:callable_local, :callable_operator, :not_callable] ->
          [?:, string]
        _ ->
          [?:, ?", String.replace(string, "\"", "\\\""), ?"]
      end

    iodata |> IO.iodata_to_binary |> string()
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
    {clauses_doc, state} = clauses_to_algebra(clauses, state)
    {"fn" |> line(clauses_doc) |> nest(2) |> line("end") |> force_break(), state}
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
    {clauses_doc, state} = clauses_to_algebra(clauses, state)
    {"(" |> line(clauses_doc) |> nest(2) |> line(")") |> force_break(), state}
  end

  ## Clauses

  defp maybe_force_clauses(doc, clauses) do
    if Enum.any?(clauses, fn {:"->", meta, _} -> Keyword.get(meta, :eol, false) end) do
      force_break(doc)
    else
      doc
    end
  end

  defp clauses_to_algebra([{:"->", _, _} = clause | clauses], state) do
    {clause_doc, state} = clause_to_algebra(clause, state)

    # If we have at least three clauses, then we apply extra empty lines.
    empty_lines? = match?([_, _ | _], clauses)

    {clauses_doc, state} =
      Enum.reduce(clauses, {clause_doc, state}, fn clause, {doc_acc, state_acc} ->
        {clause_doc, state_acc} = clause_to_algebra(clause, state_acc)
        doc_acc = if empty_lines?, do: concat(doc_acc, maybe_empty_line()), else: doc_acc
        {line(doc_acc, clause_doc), state_acc}
      end)

    {clauses_doc |> maybe_force_clauses([clause | clauses]) |> group(), state}
  end

  defp clauses_to_algebra(other, state) do
    {doc, state} = block_to_algebra(other, state)
    {group(doc), state}
  end

  defp clause_to_algebra({:"->", _, [[], body]}, state) do
    {body_doc, state} = block_to_algebra(body, state)
    {"() ->" |> glue(body_doc) |> nest(2), state}
  end

  defp clause_to_algebra({:"->", _, [args, body]}, %{operand_nesting: nesting} = state) do
    {args_doc, state} = clause_args_to_algebra(args, %{state | operand_nesting: nesting + 2})
    {body_doc, state} = block_to_algebra(body, %{state | operand_nesting: nesting})
    {concat(group(args_doc), " ->" |> glue(body_doc) |> nest(2)), state}
  end

  # fn a, b, c when d -> e end
  defp clause_args_to_algebra([{:when, meta, args}], state) do
    {args, right} = split_last(args)
    left = {:special, :arguments, args}
    binary_op_to_algebra(:when, "when", meta, left, right, :no_parens, state)
  end

  # fn a, b, c -> e end
  defp clause_args_to_algebra(args, state) do
    args_to_algebra(args, state, &quoted_to_algebra(&1, :no_parens, &2))
  end

  ## Quoted helpers

  defp if_operand_or_block(:operand, choice), do: choice
  defp if_operand_or_block(:block, choice), do: choice
  defp if_operand_or_block(other, _choice), do: other

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

  defp args_to_algebra(args, state, fun) do
    args_to_algebra(args, state, fun, &glue/2)
  end

  defp args_to_algebra([], state, _fun, _joiner) do
    {empty(), state}
  end

  defp args_to_algebra([arg | args], state, fun, joiner) do
    Enum.reduce(args, fun.(arg, state), fn arg, {doc_acc, state_acc} ->
      {arg_doc, state_acc} = fun.(arg, state_acc)
      {joiner.(concat(doc_acc, ","), arg_doc), state_acc}
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

  defp with_next_break_fits(condition, doc, fun) do
    if condition do
      doc
      |> next_break_fits(:enabled)
      |> fun.()
      |> next_break_fits(:disabled)
    else
      fun.(doc)
    end
  end

  defp next_break_fits?({:<<>>, meta, [_ | _] = entries}) do
    meta[:format] == :bin_heredoc or not interpolated?(entries)
  end

  defp next_break_fits?({{:., _, [String, :to_charlist]}, _, [{:<<>>, meta, [_ | _]}]}) do
    meta[:format] == :list_heredoc
  end

  defp next_break_fits?({:{}, _, _}) do
    true
  end

  defp next_break_fits?({:__block__, _meta, [{_, _}]}) do
    true
  end

  defp next_break_fits?({:__block__, meta, [string]}) when is_binary(string) do
    meta[:format] == :bin_heredoc
  end

  defp next_break_fits?({:__block__, meta, [list]}) when is_list(list) do
    meta[:format] != :charlist
  end

  defp next_break_fits?({form, _, [_ | _]}) when form in [:fn, :%{}, :%] do
    true
  end

  defp next_break_fits?({fun, meta, args}) when is_atom(fun) and is_list(args) do
    meta[:terminator] in [@double_heredoc, @single_heredoc] and
      (fun |> Atom.to_string() |> String.starts_with?("sigil_"))
  end

  defp next_break_fits?({{:__block__, _, [atom]}, expr}) when is_atom(atom) do
    next_break_fits?(expr)
  end

  defp next_break_fits?(_) do
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

  defp container(meta, left, doc, right) do
    if Keyword.get(meta, :eol, false) do
      surround(left, force_break(doc), right)
    else
      surround(left, doc, right)
    end
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
