Statement'CreateIndex
    ( CreateIndexStatement
        { unique = False
        , ifNotExists = False
        , name = Namespaced
            { namespace = Nothing
            , value = "i3"
            }
        , table = "t1"
        , columns = IndexedColumn
            { column = "c"
            , collation = Nothing
            , ordering = Ascending
            } :|
            [ IndexedColumn
                { column = "a"
                , collation = Nothing
                , ordering = Ascending
                }
            ]
        , where_ = Nothing
        }
    )