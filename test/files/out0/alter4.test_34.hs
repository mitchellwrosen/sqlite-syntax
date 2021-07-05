Statement'CreateTable
    ( CreateTableStatement
        { temporary = True
        , ifNotExists = False
        , name = Namespaced
            { namespace = Nothing
            , value = "t4"
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