Statement'CreateTable
    ( CreateTableStatement
        { temporary = False
        , ifNotExists = False
        , name = Namespaced
            { namespace = Nothing
            , value = "tab1"
            }
        , definition = Right
            ( TableDefinition
                { columns = ColumnDefinition
                    { name = "a"
                    , type_ = Just "int"
                    , constraints = []
                    } :| []
                , constraints = []
                , withoutRowid = False
                }
            )
        }
    )