Statement'CreateIndex
    ( CreateIndexStatement
        { unique = False
        , ifNotExists = False
        , name = Namespaced
            { namespace = Nothing
            , value = "i2"
            }
        , table = "t1"
        , columns = IndexedColumn
            { column = "a"
            , collation = Nothing
            , ordering = Descending
            } :| []
        , where_ = Nothing
        }
    )