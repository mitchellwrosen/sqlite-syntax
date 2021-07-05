Statement'CreateIndex
    ( CreateIndexStatement
        { unique = False
        , ifNotExists = False
        , name = Namespaced
            { namespace = Nothing
            , value = "idxt1"
            }
        , table = "t1"
        , columns = IndexedColumn
            { column = "x"
            , collation = Nothing
            , ordering = Ascending
            } :|
            [ IndexedColumn
                { column = "rowid"
                , collation = Nothing
                , ordering = Ascending
                }
            ]
        , where_ = Nothing
        }
    )