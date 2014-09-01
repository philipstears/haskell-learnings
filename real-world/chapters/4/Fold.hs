foldlSum xs = foldl step 0 xs
    where
        step acc x = acc + x
