Statement'CreateIndex
    ( CreateIndexStatement
        { unique = False
        , ifNotExists = False
        , name = Namespaced
            { namespace = Nothing
            , value = "idx1"
            }
        , table = "test1"
        , columns = IndexedColumn
            { column = "f2"
            , collation = Nothing
            , ordering = Ascending
            } :| []
        , where_ = Nothing
        }
    )