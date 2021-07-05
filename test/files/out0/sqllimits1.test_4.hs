Statement'CreateTable
    ( CreateTableStatement
        { temporary = False
        , ifNotExists = False
        , name = Namespaced
            { namespace = Nothing
            , value = "trig"
            }
        , definition = Right
            ( TableDefinition
                { columns = ColumnDefinition
                    { name = "a"
                    , type_ = Just "INTEGER"
                    , constraints = []
                    } :|
                    [ ColumnDefinition
                        { name = "b"
                        , type_ = Just "INTEGER"
                        , constraints = []
                        }
                    ]
                , constraints = []
                , withoutRowid = False
                }
            )
        }
    )