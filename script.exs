{opts, patterns, _} = OptionParser.parse(System.argv, strict: [save: :boolean, verbose: :boolean])

for pattern <- patterns, path <- Path.wildcard(pattern) do
  IO.write "#{path} is... "
  pre = File.read!(path)
  formatted = CodeFormatter.format!(pre)
  pos = IO.iodata_to_binary([formatted, ?\n])

  if opts[:verbose] do
    IO.puts :stderr, pos
  end

  case CodeFormatter.equivalent(pre, pos) do
    :ok ->
      IO.puts "equivalent"
    {:error, left, right} ->
      IO.puts "not equivalent (#{inspect left}, #{inspect right})"
      System.halt(1)
  end

  if opts[:save] do
    File.write!(path, pos)
  end

  :ok
end
