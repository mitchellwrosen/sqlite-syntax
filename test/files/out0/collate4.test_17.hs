Statement'CreateIndex
    ( CreateIndexStatement
        { unique = False
        , ifNotExists = False
        , name = Namespaced
            { namespace = Nothing
            , value = "collate4i1"
            }
        , table = "collate4t1"
        , columns = IndexedColumn
            { column = "a"
            , collation = Nothing
            , ordering = Ascending
            } :|
            [ IndexedColumn
                { column = "b"
                , collation = Nothing
                , ordering = Ascending
                }
            , IndexedColumn
                { column = "c"
                , collation = Nothing
                , ordering = Ascending
                }
            ]
        , where_ = Nothing
        }
    )