Statement'CreateIndex
    ( CreateIndexStatement
        { unique = False
        , ifNotExists = False
        , name = Namespaced
            { namespace = Nothing
            , value = "i1"
            }
        , table = ""
        , columns = IndexedColumn
            { column = ""
            , collation = Just "nocase"
            , ordering = Ascending
            } :| []
        , where_ = Nothing
        }
    )