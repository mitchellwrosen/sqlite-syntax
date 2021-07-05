Statement'CreateIndex
    ( CreateIndexStatement
        { unique = False
        , ifNotExists = False
        , name = Namespaced
            { namespace = Nothing
            , value = "udx_orig_code_data"
            }
        , table = "orig"
        , columns = IndexedColumn
            { column = "code"
            , collation = Nothing
            , ordering = Ascending
            } :|
            [ IndexedColumn
                { column = "data"
                , collation = Nothing
                , ordering = Ascending
                }
            ]
        , where_ = Nothing
        }
    )