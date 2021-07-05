Statement'CreateIndex
    ( CreateIndexStatement
        { unique = False
        , ifNotExists = False
        , name = Namespaced
            { namespace = Nothing
            , value = "i1"
            }
        , table = "t1"
        , columns = IndexedColumn
            { column = "x"
            , collation = Nothing
            , ordering = Ascending
            } :| []
        , where_ = Nothing
        }
    )