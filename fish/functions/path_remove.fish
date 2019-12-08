function path_remove
  count $PATH
  for i in $argv
    if test -d $i
      for j in (seq (count $PATH))
        if test $i = PATH[$j]
          set -e PATH[$j]
        end
      end
    end
  end
end
