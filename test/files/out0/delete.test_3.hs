Statement'CreateTable
    ( CreateTableStatement
        { temporary = False
        , ifNotExists = False
        , name = Namespaced
            { namespace = Nothing
            , value = "table1"
            }
        , definition = Right
            ( TableDefinition
                { columns = ColumnDefinition
                    { name = "f1"
                    , type_ = Just "int"
                    , constraints = []
                    } :|
                    [ ColumnDefinition
                        { name = "f2"
                        , type_ = Just "int"
                        , constraints = []
                        }
                    ]
                , constraints = []
                , withoutRowid = False
                }
            )
        }
    )