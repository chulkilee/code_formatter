ExUnit.start()

defmodule CodeFormatter.Case do
  defmacro assert_same(good, opts \\ []) do
    quote bind_quoted: [good: good, opts: opts] do
      assert IO.iodata_to_binary(CodeFormatter.format(good, opts)) == good
    end
  end

  defmacro assert_format(bad, good, opts \\ []) do
    quote bind_quoted: [bad: bad, good: good, opts: opts] do
      assert IO.iodata_to_binary(CodeFormatter.format(bad, opts)) == good
      assert IO.iodata_to_binary(CodeFormatter.format(good, opts)) == good
    end
  end
end
