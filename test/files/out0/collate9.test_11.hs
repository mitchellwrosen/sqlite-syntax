Statement'CreateIndex
    ( CreateIndexStatement
        { unique = False
        , ifNotExists = False
        , name = Namespaced
            { namespace = Nothing
            , value = "xy_i2"
            }
        , table = "xy"
        , columns = IndexedColumn
            { column = "y"
            , collation = Just "reverse sort"
            , ordering = Ascending
            } :| []
        , where_ = Nothing
        }
    )