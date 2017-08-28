{opts, patterns, _} = OptionParser.parse(System.argv, strict: [save: :boolean, print: :boolean])

for pattern <- patterns, path <- Path.wildcard(pattern) do
  IO.write "#{path} is... "
  pre = File.read!(path)
  formatted = CodeFormatter.format!(pre)
  pos = IO.iodata_to_binary([formatted, ?\n])

  if opts[:verbose] do
    IO.puts :stderr, pos
  end

  if CodeFormatter.equivalent?(pre, pos) do
    IO.puts :stderr, "equivalent"
  else
    IO.puts :stderr, "not equivalent"
    IO.puts :stderr, pos
    System.halt(1)
  end

  if opts[:save] do
    File.write!(path, pos)
  end

  :ok
end
