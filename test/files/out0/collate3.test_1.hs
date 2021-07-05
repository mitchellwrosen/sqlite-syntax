Statement'CreateTable
    ( CreateTableStatement
        { temporary = False
        , ifNotExists = False
        , name = Namespaced
            { namespace = Nothing
            , value = "collate3t1"
            }
        , definition = Right
            ( TableDefinition
                { columns = ColumnDefinition
                    { name = "c1"
                    , type_ = Nothing
                    , constraints = []
                    } :| []
                , constraints = []
                , withoutRowid = False
                }
            )
        }
    )