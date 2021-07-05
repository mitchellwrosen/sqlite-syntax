Statement'CreateTable
    ( CreateTableStatement
        { temporary = False
        , ifNotExists = False
        , name = Namespaced
            { namespace = Nothing
            , value = "create"
            }
        , definition = Right
            ( TableDefinition
                { columns = ColumnDefinition
                    { name = "f1"
                    , type_ = Just "int"
                    , constraints = []
                    } :| []
                , constraints = []
                , withoutRowid = False
                }
            )
        }
    )