{opts, patterns, _} = OptionParser.parse(System.argv, strict: [save: :boolean])

for pattern <- patterns, path <- Path.wildcard(pattern) do
  IO.write "#{path} is... "
  pre = File.read!(path)
  formatted = CodeFormatter.format!(pre)
  pos = IO.iodata_to_binary([formatted, ?\n])

  if CodeFormatter.equivalent?(pre, pos) do
    IO.puts "equivalent"
  else
    IO.puts "not equivalent"
    IO.write :stderr, pos
    System.halt(1)
  end

  if opts[:save] do
    File.write!(path, pos)
  end

  :ok
end
