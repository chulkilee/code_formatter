defmodule CodeFormatter do
  import Inspect.Algebra, except: [format: 2], warn: false

  @line_length 98

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
    |> Code.string_to_quoted!(wrap_literals_in_blocks: true)
    |> quoted_to_algebra(state)
    |> elem(0)
  end

  defp quoted_to_algebra({:<<>>, _, entries}, state) do
    if interpolated?(entries) do
      interpolation_to_algebra(entries, ?", state, "\"")
    else
      raise "not yet implemented"
    end
  end

  defp quoted_to_algebra({{:., _, [String, :to_charlist]}, _, [{:<<>>, _, entries}]} = quoted,
                         state) do
    if interpolated?(entries) do
      interpolation_to_algebra(entries, ?', state, "'")
    else
      remote_to_algebra(quoted, state)
    end
  end

  defp quoted_to_algebra({{:., _, [:erlang, :binary_to_atom]}, _,
                          [{:<<>>, _, entries}, :utf8]} = quoted,
                         state) do
    if interpolated?(entries) do
      interpolation_to_algebra(entries, ?", state, ":\"")
    else
      remote_to_algebra(quoted, state)
    end
  end

  defp quoted_to_algebra({:__block__, _, [list]}, state) when is_list(list) do
    if Enum.all?(list, & &1 in 0x0..0xFFFFFF) do
      {charlist_to_algebra(list), state}
    else
      raise "not yet implemented"
    end
  end

  defp quoted_to_algebra({:__block__, _, [string]}, state) when is_binary(string) do
    {string_to_algebra(string), state}
  end

  defp quoted_to_algebra({:__block__, meta, [integer]}, state) when is_integer(integer) do
    {integer_to_algebra(Keyword.fetch!(meta, :original)), state}
  end

  ## Remote calls

  defp remote_to_algebra(_quoted, _state) do
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

  defp interpolation_to_algebra([entry | entries], escape, state, acc) when is_binary(entry) do
    {escaped, _} = Code.Identifier.escape(entry, escape)
    doc = IO.iodata_to_binary(escaped)
    interpolation_to_algebra(entries, escape, state, concat(acc, doc))
  end

  defp interpolation_to_algebra([entry | entries], escape, state, acc) do
    {:::, _, [{{:., _, [Kernel, :to_string]}, _, [quoted]}, {:binary, _, _}]} = entry
    {doc, state} = quoted_to_algebra(quoted, state)
    doc = glue(nest(glue("\#{", "", doc), 2), "", "}")
    interpolation_to_algebra(entries, escape, state, concat(acc, doc))
  end

  defp interpolation_to_algebra([], escape, state, acc) do
    {group(concat(acc, <<escape>>), :strict), state}
  end

  ## Literals

  defp charlist_to_algebra(charlist) do
    {escaped, _} = Code.Identifier.escape(List.to_string(charlist), ?')
    IO.iodata_to_binary([?', escaped, ?'])
  end

  defp string_to_algebra(string) do
    {escaped, _} = Code.Identifier.escape(string, ?")
    IO.iodata_to_binary([?", escaped, ?"])
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
        if length(decimal) >= 6 do
          List.to_string(insert_underscores(decimal))
        else
          List.to_string(decimal)
        end
    end
  end

  defp insert_underscores(digits) do
    digits
    |> Enum.reverse()
    |> Enum.chunk_every(3)
    |> Enum.intersperse('_')
    |> List.flatten()
    |> Enum.reverse()
  end
end
