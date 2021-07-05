Statement'CreateIndex
    ( CreateIndexStatement
        { unique = False
        , ifNotExists = False
        , name = Namespaced
            { namespace = Nothing
            , value = "abc_index"
            }
        , table = "abc"
        , columns = IndexedColumn
            { column = "a"
            , collation = Nothing
            , ordering = Ascending
            } :| []
        , where_ = Nothing
        }
    )