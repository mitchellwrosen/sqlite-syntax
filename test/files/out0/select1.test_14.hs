Statement'CreateTable
    ( CreateTableStatement
        { temporary = False
        , ifNotExists = False
        , name = Namespaced
            { namespace = Nothing
            , value = "test2"
            }
        , definition = Right
            ( TableDefinition
                { columns = ColumnDefinition
                    { name = "r1"
                    , type_ = Just "real"
                    , constraints = []
                    } :|
                    [ ColumnDefinition
                        { name = "r2"
                        , type_ = Just "real"
                        , constraints = []
                        }
                    ]
                , constraints = []
                , withoutRowid = False
                }
            )
        }
    )