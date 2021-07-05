Statement'CreateIndex
    ( CreateIndexStatement
        { unique = True
        , ifNotExists = False
        , name = Namespaced
            { namespace = Nothing
            , value = "t3_i2"
            }
        , table = "t3"
        , columns = IndexedColumn
            { column = "b"
            , collation = Nothing
            , ordering = Ascending
            } :| []
        , where_ = Nothing
        }
    )