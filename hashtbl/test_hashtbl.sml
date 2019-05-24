let
    val _ = print "1\n"
    fun str_eq (s1, s2) =
        case (String.compare (s1, s2)) of
            EQUAL => true
          | LESS => false
          | GREATER => false
    val tbl = HashTable.mkTable (HashString.hashString, str_eq) 1000
    val str = "zhezhou"
    val _ = print ((Word.toString (HashString.hashString str)) ^ "\n")
in
    ()
end;
