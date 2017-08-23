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

  defp quoted_to_algebra(quoted, state) do
    {literal_to_algebra(quoted), state}
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

  defp literal_to_algebra({:__block__, _meta, [string]}) when is_binary(string) do
    {escaped, _} = Code.Identifier.escape(string, ?")
    IO.iodata_to_binary([?", escaped, ?"])
  end

  defp literal_to_algebra({:__block__, meta, [int]}) when is_integer(int) do
    case Keyword.fetch!(meta, :original) do
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
