defmodule CodeFormatter do
  import Inspect.Algebra, except: [format: 2]

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

  defp quoted_to_algebra(quoted, state) do
    {literal_to_algebra(quoted), state}
  end

  defp literal_to_algebra({:__block__, _meta, [string]}) when is_binary(string) do
    {escaped, _} = Code.Identifier.escape(string, ?")
    IO.iodata_to_binary([?", escaped, ?"])
  end

  defp literal_to_algebra({:__block__, meta, [int]}) when is_integer(int) do
    case Keyword.fetch!(meta, :format) do
      base when base in [:decimal, :binary, :octal, :hex] ->
        inspect(int, base: base)
      :char when int == ?\s ->
        "?\\s"
      :char when int == ?\\ ->
        "?\\\\"
      :char ->
        <<??, int::utf8>>
    end
  end
end
