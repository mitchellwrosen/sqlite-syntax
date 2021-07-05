Statement'CreateTable
    ( CreateTableStatement
        { temporary = True
        , ifNotExists = True
        , name = Namespaced
            { namespace = Nothing
            , value = "a"
            }
        , definition = Right
            ( TableDefinition
                { columns = ColumnDefinition
                    { name = "b"
                    , type_ = Nothing
                    , constraints = []
                    } :| []
                , constraints = []
                , withoutRowid = False
                }
            )
        }
    )