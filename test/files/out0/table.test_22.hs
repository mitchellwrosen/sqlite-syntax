Statement'CreateTable
    ( CreateTableStatement
        { temporary = False
        , ifNotExists = False
        , name = Namespaced
            { namespace = Nothing
            , value = "test3"
            }
        , definition = Right
            ( TableDefinition
                { columns = ColumnDefinition
                    { name = "two"
                    , type_ = Just "text"
                    , constraints = []
                    } :| []
                , constraints = []
                , withoutRowid = False
                }
            )
        }
    )