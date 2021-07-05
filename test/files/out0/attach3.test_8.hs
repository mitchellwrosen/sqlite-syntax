Statement'CreateIndex
    ( CreateIndexStatement
        { unique = False
        , ifNotExists = False
        , name = Namespaced
            { namespace = Just "aux"
            , value = "i1"
            }
        , table = "t3"
        , columns = IndexedColumn
            { column = "e"
            , collation = Nothing
            , ordering = Ascending
            } :| []
        , where_ = Nothing
        }
    )