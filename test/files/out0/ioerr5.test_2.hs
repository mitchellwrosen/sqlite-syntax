Statement'CreateTable
    ( CreateTableStatement
        { temporary = False
        , ifNotExists = False
        , name = Namespaced
            { namespace = Nothing
            , value = "A"
            }
        , definition = Right
            ( TableDefinition
                { columns = ColumnDefinition
                    { name = "Id"
                    , type_ = Just "INTEGER"
                    , constraints = []
                    } :|
                    [ ColumnDefinition
                        { name = "Name"
                        , type_ = Just "TEXT"
                        , constraints = []
                        }
                    ]
                , constraints = []
                , withoutRowid = False
                }
            )
        }
    )