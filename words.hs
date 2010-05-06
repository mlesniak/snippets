-- Split words using arbitrary split-characters.
words' sep xs =
    let a@(w, rest) = span (/=sep) xs
    in w : if null rest then [] else words' sep (tail rest)

