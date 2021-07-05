Statement'CreateIndex
    ( CreateIndexStatement
        { unique = False
        , ifNotExists = False
        , name = Namespaced
            { namespace = Nothing
            , value = "index1"
            }
        , table = "test1"
        , columns = IndexedColumn
            { column = "f1"
            , collation = Nothing
            , ordering = Ascending
            } :|
            [ IndexedColumn
                { column = "f2"
                , collation = Nothing
                , ordering = Ascending
                }
            , IndexedColumn
                { column = "f4"
                , collation = Nothing
                , ordering = Ascending
                }
            , IndexedColumn
                { column = "f3"
                , collation = Nothing
                , ordering = Ascending
                }
            ]
        , where_ = Nothing
        }
    )