Statement'CreateTable
    ( CreateTableStatement
        { temporary = True
        , ifNotExists = False
        , name = Namespaced
            { namespace = Nothing
            , value = "temp_table"
            }
        , definition = Right
            ( TableDefinition
                { columns = ColumnDefinition
                    { name = "a"
                    , type_ = Nothing
                    , constraints = []
                    } :| []
                , constraints = []
                , withoutRowid = False
                }
            )
        }
    )