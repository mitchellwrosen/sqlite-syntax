Statement'CreateTable
    ( CreateTableStatement
        { temporary = True
        , ifNotExists = False
        , name = Namespaced
            { namespace = Nothing
            , value = "dummy"
            }
        , definition = Right
            ( TableDefinition
                { columns = ColumnDefinition
                    { name = "dummy"
                    , type_ = Nothing
                    , constraints = []
                    } :| []
                , constraints = []
                , withoutRowid = False
                }
            )
        }
    )