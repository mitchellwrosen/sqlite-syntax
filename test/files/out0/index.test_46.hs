Statement'CreateIndex
    ( CreateIndexStatement
        { unique = False
        , ifNotExists = False
        , name = Namespaced
            { namespace = Nothing
            , value = "test1"
            }
        , table = "test2"
        , columns = IndexedColumn
            { column = "g1"
            , collation = Nothing
            , ordering = Ascending
            } :| []
        , where_ = Nothing
        }
    )