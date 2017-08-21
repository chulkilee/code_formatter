defmodule CodeFormatter do
  def format(string) do
    string
    |> Code.string_to_quoted!(wrap_literals_in_blocks: true)
    |> format_quoted()
    |> IO.chardata_to_string()
  end

  defp format_quoted({:__block__, meta, [int]}) when is_integer(int) do
    case Keyword.fetch!(meta, :format) do
      base when base in [:decimal, :binary, :octal, :hex] ->
        inspect(int, base: base)
      :char when int == ?\s ->
        "?\\s"
      :char when int == ?\\ ->
        "?\\\\"
      :char ->
        [??, int]
    end
  end
end
