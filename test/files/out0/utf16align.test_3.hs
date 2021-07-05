Statement'CreateIndex
    ( CreateIndexStatement
        { unique = False
        , ifNotExists = False
        , name = Namespaced
            { namespace = Nothing
            , value = "t1i2"
            }
        , table = "t1"
        , columns = IndexedColumn
            { column = "spacer"
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