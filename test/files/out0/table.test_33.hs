Statement'CreateTable
    ( CreateTableStatement
        { temporary = False
        , ifNotExists = False
        , name = Namespaced
            { namespace = Nothing
            , value = "Big"
            }
        , definition = Right
            ( TableDefinition
                { columns = ColumnDefinition
                    { name = "xyz"
                    , type_ = Just "foo"
                    , constraints = []
                    } :| []
                , constraints = []
                , withoutRowid = False
                }
            )
        }
    )