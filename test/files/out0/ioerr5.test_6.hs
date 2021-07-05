Statement'CreateIndex
    ( CreateIndexStatement
        { unique = False
        , ifNotExists = False
        , name = Namespaced
            { namespace = Nothing
            , value = "i1"
            }
        , table = "a"
        , columns = IndexedColumn
            { column = "id"
            , collation = Nothing
            , ordering = Ascending
            } :|
            [ IndexedColumn
                { column = "name"
                , collation = Nothing
                , ordering = Ascending
                }
            ]
        , where_ = Nothing
        }
    )