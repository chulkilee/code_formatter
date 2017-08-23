defmodule CodeFormatter do
  import Inspect.Algebra, except: [format: 2], warn: false

  @line_length 98
  @double_quote "\""
  @double_heredoc "\"\"\""
  @single_quote "'"
  @single_heredoc "'''"

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
    state = %{}
    string
    |> Code.string_to_quoted!(wrap_literals_in_blocks: true, unescape: false)
    |> quoted_to_algebra(state)
    |> elem(0)
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

  defp quoted_to_algebra({fun, meta, args} = quoted, state)
       when is_atom(fun) and is_list(args) do
    case {Atom.to_string(fun), args} do
      {<<"sigil_", name>>, [{:<<>>, _, entries}, modifiers]} ->
        opening_terminator = List.to_string(Keyword.fetch!(meta, :terminator))
        sigil_to_algebra(name, opening_terminator, entries, modifiers, state)
      _other ->
        local_to_algebra(quoted, state)
    end
  end

  ## Remote calls

  defp remote_to_algebra(_quoted, _state) do
    raise "not yet implemented"
  end

  ## Local calls

  defp local_to_algebra(_quoted, _state) do
    raise "not yet implemented"
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

  defp sigil_to_algebra(name, opening_terminator, entries, modifiers, state) do
    closing_terminator = closing_sigil_terminator(opening_terminator)
    acc = <<?~, name, opening_terminator::binary>>
    {doc, state} = interpolation_to_algebra(entries, closing_terminator, state, acc, closing_terminator)
    {concat(doc, List.to_string(modifiers)), state}
  end

  defp closing_sigil_terminator("("), do: ")"
  defp closing_sigil_terminator("["), do: "]"
  defp closing_sigil_terminator("{"), do: "}"
  defp closing_sigil_terminator("<"), do: ">"
  defp closing_sigil_terminator(other) when other in ["\"", "'", "|", "/"], do: other

  ## Literals

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
